;; The `$np-expand-primitives` pass is used only by "cpnanopass.ss".
;; This pass is in its own file just to break up the compiler itself
;; into smaller compilation units.

(let ()
(define-syntax define-once
  (syntax-rules ()
    [(_ id rhs) (define-once id (id) rhs)]
    [(_ id (name . _) rhs) (define id ($sgetprop 'name 'once #f))]))

(include "np-languages.ss")
(import (nanopass) np-languages)

(include "np-register.ss")
(include "np-info.ss")
(include "np-help.ss")

;; --------------------------------------------------------------------------------

(define (known-flonum-result? e)
  (let flonum-result? ([e e] [fuel 10])
    (and
     (fx> fuel 0)
     (nanopass-case (L7 Expr) e
       [,x (and (uvar? x) (eq? (uvar-type x) 'fp))]
       [(quote ,d) (flonum? d)]
       [(call ,info ,mdcl ,pr ,e* ...)
        (or (eq? 'flonum ($sgetprop (primref-name pr) '*result-type* #f))
            (and (eq? '$object-ref (primref-name pr))
                 (pair? e*)
                 (nanopass-case (L7 Expr) (car e*)
                   [(quote ,d) (eq? d 'double)])))]
       [(seq ,e0 ,e1) (flonum-result? e1 (fx- fuel 1))]
       [(let ([,x* ,e*] ...) ,body) (flonum-result? body (fx- fuel 1))]
       [(if ,e1 ,e2 ,e3) (and (flonum-result? e2 (fxsrl fuel 1))
                              (flonum-result? e3 (fxsrl fuel 1)))]
       [else #f]))))

(define rtd-ancestry (csv7:record-field-accessor #!base-rtd 'ancestry))

;; After the `np-expand-primitives` pass, some expressions produce
;; double (i.e., floating-point) values instead of pointer values.
;; Those expression results always flow to an `inline` primitive
;; that expects double values. The main consequence is that a later
;; pass must only put such returns in a temporary with type 'fp.

; TODO: recognize a direct call when it is at the end of a sequence, closures, or let form
; TODO: push call into if? (would need to pull arguments into temporaries to ensure order of evaluation)
; TODO: how does this interact with mvcall?
(module (np-expand-primitives)
  (define-threaded new-l*)
  (define-threaded new-le*)
  (define ht2 (make-hashtable symbol-hash eq?))
  (define ht3 (make-hashtable symbol-hash eq?))
  (define handle-prim
    (lambda (src sexpr level name e*)
      (let ([handler (or (and (fx= level 3) (symbol-hashtable-ref ht3 name #f))
                         (symbol-hashtable-ref ht2 name #f))])
        (and handler (handler src sexpr e*)))))
  (define-syntax Symref
    (lambda (x)
      (syntax-case x ()
        [(k ?sym)
         (with-implicit (k quasiquote)
           #'`(literal ,(make-info-literal #t 'object ?sym (constant symbol-value-disp))))])))
  (define single-valued?
    (case-lambda
     [(e) (single-valued? e 5)]
     [(e fuel)
      (and (not (zero? fuel))
           (nanopass-case (L7 Expr) e
             [,x #t]
             [(immediate ,imm) #t]
             [(literal ,info) #t]
             [(label-ref ,l ,offset) #t]
             [(mref ,e1 ,e2 ,imm ,type) #t]
             [(quote ,d) #t]
             [,pr #t]
             [(call ,info ,mdcl ,pr ,e* ...)
              (all-set? (prim-mask single-valued) (primref-flags pr))]
             [(foreign-call ,info ,e, e* ...) #t]
             [(alloc ,info ,e) #t]
             [(set! ,lvalue ,e) #t]
             [(profile ,src) #t]
             [(pariah) #t]
             [(let ([,x* ,e*] ...) ,body)
              (single-valued? body (fx- fuel 1))]
             [(if ,e0 ,e1 ,e2)
              (and (single-valued? e1 (fx- fuel 1))
                   (single-valued? e2 (fx- fuel 1)))]
             [(seq ,e0 ,e1)
              (single-valued? e1 (fx- fuel 1))]
             [(unboxed-fp ,e) #t]
             [else #f]))]))
  (define ensure-single-valued
    (case-lambda
     [(e unsafe-omit?)
      (if (or unsafe-omit?
              (single-valued? e))
          e
          (with-output-language (L7 Expr)
            (let ([t (make-tmp 'v)])
              `(values ,(make-info-call #f #f #f #f #f) ,e))))]
     [(e) (ensure-single-valued e (fx= (optimize-level) 3))]))
  (define-pass np-expand-primitives : L7 (ir) -> L9 ()
    (definitions
      (define Expr1
        (lambda (e)
          (let-values ([(e unboxed-fp?) (Expr e #f)])
            e)))
      (define Expr*
        (lambda (e*)
          (map Expr1 e*)))
      (define unboxed-fp->boxed
        (lambda (e)
          (let ([t (make-tmp 't)])
            (with-output-language (L9 Expr)
              `(let ([,t ,(%constant-alloc type-flonum (constant size-flonum))])
                 (seq
                  (set! ,(%mref ,t ,%zero ,(constant flonum-data-disp) fp) ,e)
                  ,t))))))
      (define (fp-lvalue? lvalue)
        (nanopass-case (L9 Lvalue) lvalue
          [,x (and (uvar? x) (eq? (uvar-type x) 'fp))]
          [(mref ,e1 ,e2 ,imm ,type) (eq? type 'fp)])))
    (Program : Program (ir) -> Program ()
      [(labels ([,l* ,le*] ...) ,l)
       (fluid-let ([new-l* '()] [new-le* '()])
         (let ([le* (map CaseLambdaExpr le*)])
           `(labels ([,l* ,le*] ... [,new-l* ,new-le*] ...) ,l)))])
    (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ())
    (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
      [(clause (,x* ...) ,mcp ,interface ,[body #f -> body unboxed-fp?])
       `(clause (,x* ...) ,mcp ,interface ,body)])
    ;; The result of `Expr` can be unboxed (second result is #t) only
    ;; if the `can-unbox-fp?` argument is #t, but the result can always
    ;; be a boxed expression (even if `can-unbox-fp?` is #t)
    (Expr : Expr (ir [can-unbox-fp? #f]) -> Expr (#f)
      [(quote ,d)
       (values (cond
                 [(ptr->imm d) => (lambda (i) `(immediate ,i))]
                 [else `(literal ,(make-info-literal #f 'object d 0))])
               #f)]
      [,pr (values (Symref (primref-name pr)) #f)]
      [(unboxed-fp ,[e #t -> e unboxed-fp?])
       (if can-unbox-fp?
           (values e #t)
           (values (unboxed-fp->boxed e) #f))]
      [(call ,info0 ,mdcl0
         (call ,info1 ,mdcl1 ,pr (quote ,d))
         ,[e* #f -> e* unboxed-fp?*] ...)
       (guard (and (eq? (primref-name pr) '$top-level-value) (symbol? d)))
       (values `(call ,info0 ,mdcl0 ,(Symref d) ,e* ...) #f)]
      [(call ,info ,mdcl ,pr ,e* ...)
       (cond
         [(and
           (or (not (info-call-shift-attachment? info))
               ;; Note: single-valued also implies that the primitive doesn't
               ;; tail-call an arbitary function (which might inspect attachments):
               (all-set? (prim-mask single-valued) (primref-flags pr)))
           (handle-prim (info-call-src info) (info-call-sexpr info) (primref-level pr) (primref-name pr) e*))
          => (lambda (e)
               (let-values ([(e unboxed-fp?) (Expr e can-unbox-fp?)])
                  (values
                   (cond
                     [(info-call-shift-attachment? info)
                      (let ([t (make-tmp 't (if unboxed-fp? 'fp 'ptr))])
                        `(let ([,t ,e])
                           (seq
                            (attachment-set pop #f)
                            ,t)))]
                     [else e])
                   unboxed-fp?)))]
         [else
          (let ([e* (Expr* e*)])
            ; NB: expand calls through symbol top-level values similarly
            (let ([info (if (any-set? (prim-mask abort-op) (primref-flags pr))
                            (make-info-call (info-call-src info) (info-call-sexpr info)
                                            (info-call-check? info) #t #t
                                            (info-call-shift-attachment? info)
                                            (info-call-shift-consumer-attachment?* info))
                            info)])
              (values `(call ,info ,mdcl ,(Symref (primref-name pr)) ,e* ...)
                      ;; an error can be treated as unboxed if the context wants that:
                      (and can-unbox-fp? (info-call-error? info)))))])]
      [(call ,info ,mdcl ,x ,e* ...)
       (guard (uvar-loop? x))
       (let ([e* (map (lambda (x1 e)
                        (let ([unbox? (eq? (uvar-type x1) 'fp)])
                          (let-values ([(e unboxed-fp?) (Expr e unbox?)])
                            (cond
                              [(and unbox? (not unboxed-fp?))
                               (%mref ,e ,%zero ,(constant flonum-data-disp) fp)]
                              [else e]))))
                      (uvar-location x) e*)])
         (values `(call ,info ,mdcl ,x ,e* ...) #f))]
      [(call ,info ,mdcl ,e ,e*  ...)
       (let ([e (and e (Expr1 e))]
             [e* (Expr* e*)])
         (values `(call ,info ,mdcl ,e ,e* ...) #f))]
      [(inline ,info ,prim ,e* ...)
       (cond
         [(info-unboxed-args? info)
          (let ([e* (map (lambda (e unbox-arg?)
                           (let-values ([(e unboxed-arg?) (Expr e unbox-arg?)])
                             (if (and unbox-arg? (not unboxed-arg?))
                                 (%mref ,e ,%zero ,(constant flonum-data-disp) fp)
                                 e)))
                         e*
                         (info-unboxed-args-unboxed?* info))])
            (values `(inline ,info ,prim ,e* ...)
                    ;; Especially likely to be replaced by enclosing `unboxed-fp` wrapper:
                    #f))]
         [else
          (let ([e* (Expr* e*)])
            (values `(inline ,info ,prim ,e* ...) #f))])]
      [(set! ,[lvalue #t -> lvalue fp-unboxed?l] ,e)
       (let ([fp? (fp-lvalue? lvalue)])
         (let-values ([(e unboxed?) (Expr e fp?)])
           (let ([e (if (and fp? (not unboxed?))
                        (%mref ,e ,%zero ,(constant flonum-data-disp) fp)
                        e)])
             (values `(set! ,lvalue ,e) #f))))]
      [(values ,info ,[e* #f -> e* unboxed-fp?*] ...) (values `(values ,info ,e* ...) #f)]
      [(alloc ,info ,e) (values `(alloc ,info ,(Expr1 e)) #f)]
      [(if ,[e0 #f -> e0 unboxed-fp?0] ,[e1 can-unbox-fp? -> e1 unboxed-fp?1] ,[e2 can-unbox-fp? -> e2 unboxed-fp?2])
       (let* ([unboxed-fp? (or unboxed-fp?1 unboxed-fp?2)]
              [e1 (if (and unboxed-fp? (not unboxed-fp?1))
                      (%mref ,e1 ,%zero ,(constant flonum-data-disp) fp)
                      e1)]
              [e2 (if (and unboxed-fp? (not unboxed-fp?2))
                      (%mref ,e2 ,%zero ,(constant flonum-data-disp) fp)
                      e2)])
         (values `(if ,e0 ,e1 ,e2) unboxed-fp?))]
      [(seq ,[e0 #f -> e0 unboxed-fp?0] ,[e1 can-unbox-fp? -> e1 unboxed-fp?])
       (values `(seq ,e0 ,e1) unboxed-fp?)]
      [(let ([,x* ,e*] ...) ,body)
       (let ([e* (map (lambda (x e)
                        (if (eq? (uvar-type x) 'fp)
                            (let-values ([(e unboxed?) (Expr e #t)])
                              (if (not unboxed?)
                                  (%mref ,e ,%zero ,(constant flonum-data-disp) fp)
                                  e))
                            (Expr1 e)))
                      x* e*)])
         (let-values ([(body unboxed-fp?) (Expr body can-unbox-fp?)])
           (values `(let ([,x* ,e*] ...) ,body) unboxed-fp?)))]
      [(loop ,x (,x* ...) ,body)
       (uvar-location-set! x x*)
       (let-values ([(body unboxed-fp?) (Expr body can-unbox-fp?)])
         (uvar-location-set! x #f)
         (values `(loop ,x (,x* ...) ,body) unboxed-fp?))]
      [(raw ,e) (values `(raw ,(Expr1 e)) #f)]
      [(attachment-set ,aop ,e) (values `(attachment-set ,aop ,(and e (Expr1 e))) #f)]
      [(attachment-get ,reified ,e) (values `(attachment-get ,reified ,(and e (Expr1 e))) #f)]
      [(attachment-consume ,reified ,e) (values `(attachment-consume ,reified ,(and e (Expr1 e))) #f)]
      [(continuation-set ,cop ,e1 ,e2) (values `(continuation-set ,cop ,(Expr1 e1) ,(Expr1 e2)) #f)]
      [(label ,l ,[body can-unbox-fp? -> body unboxed-fp?]) (values `(label ,l ,body) unboxed-fp?)]
      [(foreign-call ,info ,e ,e* ...)
       (let ([e (Expr1 e)]
             [e* (if (info-foreign-unboxed? info)
                     (map (lambda (e type)
                            (let ([unbox-arg? (fp-type? type)])
                              (let-values ([(e unboxed-fp?) (Expr  e unbox-arg?)])
                                (if (and unbox-arg? (not unboxed-fp?))
                                    (%mref ,e ,%zero ,(constant flonum-data-disp) fp)
                                    e))))
                          e*
                          (info-foreign-arg-type* info))
                     (map Expr1 e*))])
         (let ([new-e `(foreign-call ,info ,e ,e* ...)]
               [unboxed? (and (info-foreign-unboxed? info)
                              (fp-type? (info-foreign-result-type info)))])
           (if (and unboxed? (not can-unbox-fp?))
               (values (unboxed-fp->boxed new-e) #f)
               (values new-e unboxed?))))]
      [(mvcall ,info ,e1 ,e2) (values `(mvcall ,info ,(Expr1 e1) ,(Expr1 e2)) #f)]
      [(mvlet ,e ((,x** ...) ,interface* ,body*) ...)
       (values `(mvlet ,(Expr1 e) ((,x** ...) ,interface* ,(map Expr1 body*)) ...) #f)]
      [,lvalue (Lvalue lvalue can-unbox-fp?)])
    (Lvalue : Lvalue (ir [unboxed-fp? #f]) -> Lvalue (#f)
      [(mref ,e1 ,e2 ,imm ,type)
       (let ([e `(mref ,(Expr1 e1) ,(Expr1 e2) ,imm ,type)])
         (if (and (eq? type 'fp) (not unboxed-fp?))
             (values (unboxed-fp->boxed e) #f)
             (values e (eq? type 'fp))))]
      [,x
       (let ([fp? (and (uvar? x) (eq? (uvar-type x) 'fp))])
         (if (and fp? (not unboxed-fp?))
             (values (unboxed-fp->boxed x) #f)
             (values x fp?)))]))
  (define-who unhandled-arity
    (lambda (name args)
      (sorry! who "unhandled argument count ~s for ~s" (length args) 'name)))
  (with-output-language (L7 Expr)
    (define-$type-check (L7 Expr))
    (define-syntax define-inline
      (let ()
        (define ctht2 (make-hashtable symbol-hash eq?))
        (define ctht3 (make-hashtable symbol-hash eq?))
        (define check-and-record
          (lambda (level name)
            (let ([a (symbol-hashtable-cell (if (fx= level 2) ctht2 ctht3) (syntax->datum name) #f)])
              (when (cdr a) (syntax-error name "duplicate inline"))
              (set-cdr! a #t))))
        (lambda (x)
          (define compute-interface
            (lambda (clause)
              (syntax-case clause ()
                [(x e1 e2 ...) (identifier? #'x) -1]
                [((x ...) e1 e2 ...) (length #'(x ...))]
                [((x ... . r) e1 e2 ...) (fxlognot (length #'(x ...)))])))
          (define bitmaskify
            (lambda (i*)
              (fold-left (lambda (mask i)
                           (logor mask (if (fx< i 0) (ash -1 (fxlognot i)) (ash 1 i))))
                0 i*)))
          (syntax-case x ()
            [(k level id clause ...)
             (identifier? #'id)
             (let ([level (datum level)] [name (datum id)])
               (unless (memv level '(2 3))
                 (syntax-error x (format "invalid level ~s in inline definition" level)))
               (let ([pr ($sgetprop name (if (eqv? level 2) '*prim2* '*prim3*) #f)])
                 (include "primref.ss")
                 (unless pr
                   (syntax-error x (format "unrecognized primitive name ~s in inline definition" name)))
                 (let ([arity (primref-arity pr)])
                   (when arity
                     (unless (= (bitmaskify arity) (bitmaskify (map compute-interface #'(clause ...))))
                       (syntax-error x (format "arity mismatch for ~s" name))))))
               (check-and-record level #'id)
               (with-implicit (k src sexpr moi)
                 #`(symbol-hashtable-set! #,(if (eqv? level 2) #'ht2 #'ht3) 'id
                     (rec moi
                       (lambda (src sexpr args)
                         (apply (case-lambda clause ... [rest #f]) args))))))]))))
    (define no-need-to-bind?
      (lambda (multiple-ref? e)
        (nanopass-case (L7 Expr) e
          [,x (if (uvar? x) (not (uvar-assigned? x)) (eq? x %zero))]
          [(immediate ,imm) #t] ; might should produce binding if imm is large
          [(quote ,d) (or (not multiple-ref?) (ptr->imm d))]
          [,pr (not multiple-ref?)]
          [(literal ,info) (and (not multiple-ref?) (not (info-literal-indirect? info)))]
          [(profile ,src) #t]
          [(pariah) #t]
          [else #f])))
    (define binder
      (lambda (multiple-ref? type e)
        (if (and (not (eq? multiple-ref? 'always))
                 (no-need-to-bind? multiple-ref? e))
            (values e values)
            (let-values ([(type e)
                          (nanopass-case (L7 Expr) e
                            [(raw ,e) (values 'uptr e)]
                            [else (values type e)])])
              (let ([t (make-tmp 't type)])
                (values t (lift-fp-unboxed
                           (lambda (body)
                             `(let ([,t ,e]) ,body)))))))))
    (define list-binder
      (lambda (multiple-ref? type e*)
        (if (null? e*)
            (values '() values)
            (let-values ([(e dobind) (binder multiple-ref? type (car e*))]
                         [(e* dobind*) (list-binder multiple-ref? type (cdr e*))])
              (values (cons e e*)
                (lambda (body)
                  (dobind (dobind* body))))))))
    (define dirty-store-binder
      (lambda (multiple-ref? type e)
        (nanopass-case (L7 Expr) e
          [(call ,info ,mdcl ,pr ,e)
           (guard (eq? (primref-name pr) '$fixmediate))
           (let-values ([(t dobind) (binder multiple-ref? type e)])
             (values `(call ,info ,mdcl ,pr ,t) dobind))]
          [else
           (binder multiple-ref? type e)])))
    (define-syntax $bind
      (lambda (x)
        (syntax-case x ()
          [(_ binder multiple-ref? type (b ...) e)
           (let ([t0* (generate-temporaries #'(b ...))])
             (let f ([b* #'(b ...)] [t* t0*] [x* '()])
               (if (null? b*)
                   (with-syntax ([(x ...) (reverse x*)] [(t ...) t0*])
                     #`(let ([x t] ...) e))
                   (syntax-case (car b*) ()
                     [x (identifier? #'x)
                      #`(let-values ([(#,(car t*) dobind) (binder multiple-ref? 'type x)])
                          (dobind #,(f (cdr b*) (cdr t*) (cons #'x x*))))]
                     [(x e) (identifier? #'x)
                      #`(let-values ([(#,(car t*) dobind) (binder multiple-ref? 'type e)])
                          (dobind #,(f (cdr b*) (cdr t*) (cons #'x x*))))]))))])))
    (define-syntax bind
      (syntax-rules ()
        [(_ multiple-ref? type (b ...) e)
         (identifier? #'type)
         ($bind binder multiple-ref? type (b ...) e)]
        [(_ multiple-ref? (b ...) e)
         ($bind binder multiple-ref? ptr (b ...) e)]))
    (define-syntax list-bind
      (syntax-rules ()
        [(_ multiple-ref? type (b ...) e)
         (identifier? #'type)
         ($bind list-binder multiple-ref? type (b ...) e)]
        [(_ multiple-ref? (b ...) e)
         ($bind list-binder multiple-ref? ptr (b ...) e)]))
    (define-syntax dirty-store-bind
      (syntax-rules ()
        [(_ multiple-ref? (b ...) e)
         ($bind dirty-store-binder multiple-ref? ptr (b ...) e)]))
    (define lift-fp-unboxed
      (lambda (k)
        (lambda (e)
          ;; Propagate unboxing information:
          (nanopass-case (L7 Expr) e
            [(unboxed-fp ,e) `(unboxed-fp ,(k e))]
            [else
             (let ([new-e (k e)])
               (nanopass-case (L7 Expr) e
                 [(mref ,e0 ,e1 ,imm ,type)
                  (if (eq? type 'fp)
                      `(unboxed-fp ,new-e)
                      new-e)]
                 [,x (if (and (uvar? x) (eq? (uvar-type x) 'fp))
                         `(unboxed-fp ,new-e)
                         new-e)]
                 [else new-e]))]))))
    (define-syntax build-libcall
      (lambda (x)
        (syntax-case x ()
          [(k pariah? src sexpr name e ...)
           (let ([libspec ($sgetprop (datum name) '*libspec* #f)])
             (define interface-okay?
               (lambda (interface* cnt)
                 (ormap
                   (lambda (interface)
                     (if (fx< interface 0)
                         (fx>= cnt (lognot interface))
                         (fx= cnt interface)))
                   interface*)))
             (unless libspec (syntax-error x "unrecognized library routine"))
             (unless (eqv? (length #'(e ...)) (libspec-interface libspec))
               (syntax-error x "invalid number of arguments"))
             (let ([is-pariah? (datum pariah?)])
               (unless (boolean? is-pariah?)
                 (syntax-error x "pariah indicator must be a boolean literal"))
               (when (and (libspec-error? libspec) (not is-pariah?))
                 (syntax-error x "pariah indicator is inconsistent with libspec-error indicator"))
               (with-implicit (k quasiquote)
                 (with-syntax ([body #`(call ,(make-info-call src sexpr #f pariah? #,(libspec-error? libspec)) #f
                                         (literal ,(make-info-literal #f 'library '#,(datum->syntax #'* libspec) 0))
                                         ,e ...)])
                   (if is-pariah?
                       #'`(seq (pariah) body)
                       #'`body)))))])))
    (define-syntax when-known-endianness
      (lambda (stx)
        (syntax-case stx ()
          [(_ e ...)
           #'(constant-case native-endianness
               [(unknown) (void)]
               [else e ...])])))
    (define constant?
      (case-lambda
        [(x)
         (nanopass-case (L7 Expr) x
           [(quote ,d) #t]
           ; TODO: handle immediate?
           [else #f])]
        [(pred? x)
         (nanopass-case (L7 Expr) x
           [(quote ,d) (pred? d)]
           ; TODO: handle immediate?
           [else #f])]))
    (define constant-value
      (lambda (x)
        (nanopass-case (L7 Expr) x
          [(quote ,d) d]
          ; TODO: handle immediate if constant? does
          [else #f])))
    (define maybe-add-label
      (lambda (Llib body)
        (if Llib
            `(label ,Llib ,body)
            body)))
    (define build-and
      (lambda (e1 e2)
        `(if ,e1 ,e2 ,(%constant sfalse))))
    (define maybe-build-and
      (lambda (e1 e2)
        (if e1
            (build-and e1 e2)
            e2)))
    (define build-simple-or
      (lambda (e1 e2)
        `(if ,e1 ,(%constant strue) ,e2)))
     (define build-fix
       (lambda (e)
         (%inline sll ,e ,(%constant fixnum-offset))))
     (define build-double-scale
       (lambda (e)
         (constant-case ptr-bits
           [(32) (%inline sll ,e (immediate 1))]
           [(64) e]
           [else ($oops 'build-double-scale "unknown ptr-bit size ~s" (constant ptr-bits))])))
     (define build-unfix
       (lambda (e)
         (nanopass-case (L7 Expr) e
           [(quote ,d) (guard (target-fixnum? d)) `(immediate ,d)]
           [else `(raw ,(%inline sra ,e ,(%constant fixnum-offset)))])))
    (define build-not
      (lambda (e)
        `(if ,e ,(%constant sfalse) ,(%constant strue))))
    (define build-null?
      (lambda (e)
        (%type-check mask-nil snil ,e)))
    (define build-eq?
      (lambda (e1 e2)
        (%inline eq? ,e1 ,e2)))
    (define build-eqv?
      (lambda (src sexpr e1 e2)
        (bind #t (e1 e2)
          (build-simple-or
           (build-eq? e1 e2)
           (build-and
            ;; checking just one argument is good enough for typical
            ;; uses, where `eqv?` almost always receives two fixnums
            ;; or two characters; checking both arguments appears to
            ;; by counter-productive by introducing too many branches
            (build-simple-or
             (%type-check mask-flonum type-flonum ,e1)
             (build-and
              (%type-check mask-typed-object type-typed-object ,e1)
              (%type-check mask-other-number type-other-number
                ,(%mref ,e1 ,(constant bignum-type-disp)))))
            (build-libcall #f src sexpr eqv? e1 e2))))))
    (define make-build-eqv?
      (lambda (src sexpr)
        (lambda (e1 e2)
          (build-eqv? src sexpr e1 e2))))
    (define fixnum-constant?
      (lambda (e)
        (constant? target-fixnum? e)))
    (define expr->index
      (lambda (e alignment limit)
        (nanopass-case (L7 Expr) e
          [(quote ,d)
           (and (target-fixnum? d)
                (>= d 0)
                (< d limit)
                (fxzero? (logand d (fx- alignment 1)))
                d)]
          [else #f])))
    (define build-fixnums?
      (lambda (e*)
        (let ([e* (remp fixnum-constant? e*)])
          (if (null? e*)
              `(quote #t)
              (%type-check mask-fixnum type-fixnum
                ,(fold-left (lambda (e1 e2) (%inline logor ,e1 ,e2))
                   (car e*) (cdr e*)))))))
    (define build-flonums?
      (lambda (e*)
        (let ([e* (remp (lambda (e) (constant? flonum? e)) e*)])
          (if (null? e*)
              `(quote #t)
              (let f ([e* e*])
                (let ([e (car e*)] [e* (cdr e*)])
                  (let ([check (%type-check mask-flonum type-flonum ,e)])
                    (if (null? e*)
                        check
                        (build-and check (f e*))))))))))
    (define build-fl=
      (lambda (e1 e2) ; must be bound
        `(inline ,(make-info-unboxed-args '(#t #t)) ,%fp= ,e1 ,e2)))
    (define build-chars?
      (lambda (e1 e2)
        (define char-constant?
          (lambda (e)
            (constant? char? e)))
        (if (char-constant? e1)
            (if (char-constant? e2)
                (%constant strue)
                (%type-check mask-char type-char ,e2))
            (if (char-constant? e2)
                (%type-check mask-char type-char ,e1)
                (build-and
                  (%type-check mask-char type-char ,e1)
                  (%type-check mask-char type-char ,e2))))))
    (define build-list
      (lambda (e*)
        (if (null? e*)
            (%constant snil)
            (list-bind #f (e*)
              (bind #t ([t (%constant-alloc type-pair (fx* (constant size-pair) (length e*)))])
                (let loop ([e* e*] [i 0])
                  (let ([e (car e*)] [e* (cdr e*)])
                    `(seq
                       (set! ,(%mref ,t ,(fx+ i (constant pair-car-disp))) ,e)
                       ,(if (null? e*)
                            `(seq
                               (set! ,(%mref ,t ,(fx+ i (constant pair-cdr-disp))) ,(%constant snil))
                               ,t)
                            (let ([next-i (fx+ i (constant size-pair))])
                              `(seq
                                 (set! ,(%mref ,t ,(fx+ i (constant pair-cdr-disp)))
                                   ,(%inline + ,t (immediate ,next-i)))
                                 ,(loop e* next-i))))))))))))
    (define build-pair?
      (lambda (e)
        (%type-check mask-pair type-pair ,e)))
    (define build-car
      (lambda (e)
        (%mref ,e ,(constant pair-car-disp))))
    (define build-cdr
      (lambda (e)
        (%mref ,e ,(constant pair-cdr-disp))))
    (define build-char->integer
      (lambda (e)
        (%inline srl ,e
          (immediate ,(fx- (constant char-data-offset) (constant fixnum-offset))))))
    (define build-integer->char
      (lambda (e)
        (%inline +
          ,(%inline sll ,e
             (immediate ,(fx- (constant char-data-offset) (constant fixnum-offset))))
          ,(%constant type-char))))
    (define need-store-fence?
      (if-feature pthreads
	              (constant-case architecture
                                 [(arm32 arm64 riscv64 loongarch64 ppc32 pb) #t]
                                 [else #f])
                  #f))
    (define add-store-fence
      ;; A store--store fence should be good enough for safety on a platform that
      ;; orders load dependencies (which is anything except Alpha)
      (lambda (e)
        (if need-store-fence?
            `(seq ,(%inline store-store-fence) ,e)
            e)))
    (define build-dirty-store
      (case-lambda
        [(base offset e) (build-dirty-store base %zero offset e)]
        [(base index offset e) (build-dirty-store base index offset e
                                 (lambda (base index offset e) `(set! ,(%mref ,base ,index ,offset) ,e))
                                 (lambda (s r) `(seq ,s ,r)))]
        [(base index offset e build-assign build-remember-seq)
         (nanopass-case (L7 Expr) e
           [(call ,info ,mdcl ,pr ,e)
            (guard (eq? (primref-name pr) '$fixmediate))
            (build-assign base index offset e)]
           [else
            (if (let loop ([e e] [fuel 5])
                  (nanopass-case (L7 Expr) e
                    [(quote ,d) (ptr->imm d)]
                    [(call ,info ,mdcl ,pr ,e* ...)
                     (memq ($sgetprop (primref-name pr) '*result-type* #f)
                           '(fixnum boolean))]
                    [(if ,e1 ,e2 ,e3)
                     (and (fx> fuel 0) (loop e2 (fx- fuel 1)) (loop e3 (fx- fuel 1)))]
                    [(seq ,e1 ,e2)
                     (and (fx> fuel 0) (loop e2 (fx- fuel 1)))]
                    [else #f]))
                (build-assign base index offset e)
                (let ([a (if (eq? index %zero)
                             (%lea ,base offset)
                             (%lea ,base ,index offset))])
                  ; NB: should work harder to determine cases where x can't be a fixnum
                  (if (nanopass-case (L7 Expr) e
                        [(quote ,d) #t]
                        [(literal ,info) #t]
                        [else #f])
                      (bind #f ([e e])
                        ; eval a second so the address is not live across any calls
                        (bind #t uptr ([a a]) ; uptr for clarity, though safe given eval order
                          (add-store-fence
                           (build-remember-seq
                            (build-assign a %zero 0 e)
                            (%inline remember ,a)))))
                      (bind #t ([e e])
                        ; eval a second so the address is not live across any calls
                        (bind #t uptr ([a a]) ; uptr for clarity, though safe given eval order
                          (if need-store-fence?
                              ;; Fence needs to be before store, so duplicate
                              ;; store instruction to lift out fixnum check; this
                              ;; appears to be worthwhile on the Apple M1 to avoid
                              ;; tighly interleaved writes and fences
                              `(if ,(%type-check mask-fixnum type-fixnum ,e)
                                   ,(build-assign a %zero 0 e)
                                   ,(add-store-fence
			             (build-remember-seq
                                      (build-assign a %zero 0 e)
                                      (%inline remember ,a))))
                              ;; Generate one copy of store instruction
			      (build-remember-seq
                               (build-assign a %zero 0 e)
                               `(if ,(%type-check mask-fixnum type-fixnum ,e)
                                    ,(%constant svoid)
                                    ,(%inline remember ,a)))))))))])]))
    (define make-build-cas
      (lambda (old-v)
        (lambda (base index offset v)
          `(seq
            ,(%inline cas ,base ,index (immediate ,offset) ,old-v ,v)
            (inline ,(make-info-condition-code 'eq? #f #t) ,%condition-code)))))
    (define build-cas-seq
      (lambda (cas remember)
        `(if ,cas
             (seq ,remember ,(%constant strue))
             ,(%constant sfalse))))
    (define build-$record
      (lambda (tag args)
        (bind #f (tag)
          (list-bind #f (args)
            (let ([n (fx+ (length args) 1)])
              (bind #t ([t (%constant-alloc type-typed-object (fx* n (constant ptr-bytes)))])
                `(seq
                   (set! ,(%mref ,t ,(constant record-type-disp)) ,tag)
                   ,(let f ([args args] [offset (constant record-data-disp)])
                      (if (null? args)
                          t
                          `(seq
                             (set! ,(%mref ,t ,offset) ,(car args))
                             ,(f (cdr args) (fx+ offset (constant ptr-bytes)))))))))))))
    (define build-$real->flonum
      (lambda (src sexpr x who)
        (if (known-flonum-result? x)
            x
            (bind #t (x)
              (bind #f (who)
                `(if ,(%type-check mask-flonum type-flonum ,x)
                     ,x
                     ,(build-libcall #t src sexpr real->flonum x who)))))))
    (define build-$inexactnum-real-part
      (lambda (e)
        (%lea ,e (fx+ (constant inexactnum-real-disp)
                   (fx- (constant type-flonum) (constant typemod))))))
    (define build-$inexactnum-imag-part
      (lambda (e)
        (%lea ,e (fx+ (constant inexactnum-imag-disp)
                   (fx- (constant type-flonum) (constant typemod))))))
    (define make-build-fill
      (lambda (elt-bytes data-disp)
        (define ptr-bytes (constant ptr-bytes))
        (define super-size
          (lambda (e-fill)
            (define-who super-size-imm
              (lambda (imm)
                `(immediate
                   ,(constant-case ptr-bytes
                      [(4)
                       (case elt-bytes
                         [(1) (let ([imm (logand imm #xff)])<
                                (let ([imm (logor (ash imm 8) imm)])
                                  (logor (ash imm 16) imm)))]
                         [(2) (let ([imm (logand imm #xffff)])
                                (logor (ash imm 16) imm))]
                         [else (sorry! who "unexpected elt-bytes ~s" elt-bytes)])]
                      [(8)
                       (case elt-bytes
                         [(1) (let ([imm (logand imm #xff)])
                                (let ([imm (logor (ash imm 8) imm)])
                                  (let ([imm (logor (ash imm 16) imm)])
                                    (logor (ash imm 32) imm))))]
                         [(2) (let ([imm (logand imm #xffff)])
                                (let ([imm (logor (ash imm 16) imm)])
                                  (logor (ash imm 32) imm)))]
                         [(4) (let ([imm (logand imm #xffffffff)])
                                (logor (ash imm 32) imm))]
                         [else (sorry! who "unexpected elt-bytes ~s" elt-bytes)])]))))
            (define-who super-size-expr
              (lambda (e-fill)
                (define (double e-fill k)
                  (%inline logor
                     ,(%inline sll ,e-fill (immediate ,k))
                     ,e-fill))
                (define (mask e-fill k)
                  (%inline logand ,e-fill (immediate ,k)))
                (constant-case ptr-bytes
                  [(4)
                   (case elt-bytes
                     [(1) (bind #t ([e-fill (mask e-fill #xff)])
                            (bind #t ([e-fill (double e-fill 8)])
                              (double e-fill 16)))]
                     [(2) (bind #t ([e-fill (mask e-fill #xffff)])
                            (double e-fill 16))]
                     [else (sorry! who "unexpected elt-bytes ~s" elt-bytes)])]
                  [(8)
                   (case elt-bytes
                     [(1) (bind #t ([e-fill (mask e-fill #xff)])
                            (bind #t ([e-fill (double e-fill 8)])
                              (bind #t ([e-fill (double e-fill 16)])
                                (double e-fill 32))))]
                     [(2) (bind #t ([e-fill (mask e-fill #xffff)])
                            (bind #t ([e-fill (double e-fill 16)])
                              (double e-fill 32)))]
                     [(4) (bind #t ([e-fill (mask e-fill #xffffffff)])
                            (double e-fill 32))]
                     [else (sorry! who "unexpected elt-bytes ~s" elt-bytes)])])))
            (if (fx= elt-bytes ptr-bytes)
                e-fill
                (nanopass-case (L7 Expr) e-fill
                  [(quote ,d)
                   (cond
                     [(ptr->imm d) => super-size-imm]
                     [else (super-size-expr e-fill)])]
                  [(immediate ,imm) (super-size-imm imm)]
                  [else (super-size-expr e-fill)]))))
        (lambda (e-vec e-bytes e-fill)
          ; NB: caller must bind e-vec and e-fill
          (safe-assert (no-need-to-bind? #t e-vec))
          (safe-assert (no-need-to-bind? #f e-fill))
          (nanopass-case (L7 Expr) e-bytes
            [(immediate ,imm)
             (guard (fixnum? imm) (fx<= 0 imm (fx* 4 ptr-bytes)))
             (if (fx= imm 0)
                 e-vec
                 (bind #t ([e-fill (super-size e-fill)])
                   (let f ([n (if (fx>= elt-bytes ptr-bytes)
                                  imm
                                  (fxlogand (fx+ imm (fx- ptr-bytes 1)) (fx- ptr-bytes)))])
                     (let ([n (fx- n ptr-bytes)])
                       `(seq
                          (set! ,(%mref ,e-vec ,(fx+ data-disp n)) ,e-fill)
                          ,(if (fx= n 0) e-vec (f n)))))))]
            [else
             (let ([Ltop (make-local-label 'Ltop)] [t (make-assigned-tmp 't 'uptr)])
               (bind #t ([e-fill (super-size e-fill)])
                 `(let ([,t ,(if (fx>= elt-bytes ptr-bytes)
                                 e-bytes
                                 (nanopass-case (L7 Expr) e-bytes
                                   [(immediate ,imm)
                                    `(immediate ,(logand (+ imm (fx- ptr-bytes 1)) (fx- ptr-bytes)))]
                                   [else
                                     (%inline logand
                                       ,(%inline +
                                          ,e-bytes
                                          (immediate ,(fx- ptr-bytes 1)))
                                       (immediate ,(fx- ptr-bytes)))]))])
                    (label ,Ltop
                      (if ,(%inline eq? ,t (immediate 0))
                          ,e-vec
                          ,(%seq
                             (set! ,t ,(%inline - ,t (immediate ,ptr-bytes)))
                             (set! ,(%mref ,e-vec ,t ,data-disp) ,e-fill)
                             (goto ,Ltop)))))))]))))

    ;; NOTE: integer->ptr and unsigned->ptr DO NOT handle 64-bit integers on a 32-bit machine.
    ;; this is okay for $object-ref and $object-set!, which do not support moving 64-bit values
    ;; as single entities on a 32-bit machine, but care should be taken if these are used with
    ;; other primitives.
    (define-who integer->ptr
      (lambda (x width)
        (if (fx>= (constant fixnum-bits) width)
            (build-fix x)
            (%seq
              (set! ,%ac0 ,x)
              (set! ,%xp ,(build-fix %ac0))
              (set! ,%xp ,(build-unfix %xp))
              (if ,(%inline eq? ,%ac0 ,%xp)
                  ,(build-fix %ac0)
                  (seq
                    (set! ,%ac0
                      (inline
                        ,(case width
                           [(32) (intrinsic-info-asmlib dofretint32 #f)]
                           [(64) (intrinsic-info-asmlib dofretint64 #f)]
                             [else ($oops who "can't handle width ~s" width)])
                        ,%asmlibcall))
                    ,%ac0))))))
    (define-who unsigned->ptr
      (lambda (x width)
        (if (fx>= (constant fixnum-bits) width)
            (build-fix x)
            `(seq
               (set! ,%ac0 ,x)
               (if ,(%inline u< ,(%constant most-positive-fixnum) ,%ac0)
                   (seq
                     (set! ,%ac0
                       (inline
                         ,(case width
                            [(32) (intrinsic-info-asmlib dofretuns32 #f)]
                            [(64) (intrinsic-info-asmlib dofretuns64 #f)]
                            [else ($oops who "can't handle width ~s" width)])
                         ,%asmlibcall))
                     ,%ac0)
                   ,(build-fix %ac0))))))
    (define-who i32xu32->ptr
      (lambda (hi lo)
        (safe-assert (eqv? (constant ptr-bits) 32))
        (let ([Lbig (make-local-label 'Lbig)])
          (bind #t (lo hi)
            `(if ,(%inline eq? ,hi ,(%inline sra ,lo (immediate 31)))
                 ,(bind #t ([fxlo (build-fix lo)])
                    `(if ,(%inline eq? ,(build-unfix fxlo) ,lo)
                         ,fxlo
                         (goto ,Lbig)))
                 (label ,Lbig
                   ,(%seq
                      (set! ,%ac0 ,lo)
                      (set! ,(ref-reg %ac1) ,hi)
                      (set! ,%ac0 (inline ,(intrinsic-info-asmlib dofretint64 #f) ,%asmlibcall))
                      ,%ac0)))))))
    (define-who u32xu32->ptr
      (lambda (hi lo)
        (safe-assert (eqv? (constant ptr-bits) 32))
        (let ([Lbig (make-local-label 'Lbig)])
          (bind #t (lo hi)
            `(if ,(%inline eq? ,hi (immediate 0))
                 (if ,(%inline u< ,(%constant most-positive-fixnum) ,lo)
                     (goto ,Lbig)
                     ,(build-fix lo))
                 (label ,Lbig
                   ,(%seq
                      (set! ,%ac0 ,lo)
                      (set! ,(ref-reg %ac1) ,hi)
                      (set! ,%ac0 (inline ,(intrinsic-info-asmlib dofretuns64 #f) ,%asmlibcall))
                      ,%ac0)))))))

    (define-who ptr->integer
      (lambda (value width)
        (if (fx> (constant fixnum-bits) width)
            (build-unfix value)
            `(raw
               (seq
                 (set! ,%ac0 ,value)
                 (if ,(%type-check mask-fixnum type-fixnum ,%ac0)
                     ,(build-unfix %ac0)
                     (seq
                       (set! ,%ac0
                         (inline
                           ,(cond
                              [(fx<= width 32) (intrinsic-info-asmlib dofargint32 #f)]
                              [(fx<= width 64) (intrinsic-info-asmlib dofargint64 #f)]
                              [else ($oops who "can't handle width ~s" width)])
                           ,%asmlibcall))
                       ,%ac0)))))))
    (define ptr-type (constant-case ptr-bits
                       [(32) 'unsigned-32]
                       [(64) 'unsigned-64]
                       [else ($oops 'ptr-type "unknown ptr-bit size ~s" (constant ptr-bits))]))
    (define-who type->width
      (lambda (x)
        (case x
          [(integer-8 unsigned-8 char) 8]
          [(integer-16 unsigned-16) 16]
          [(integer-24 unsigned-24) 24]
          [(integer-32 unsigned-32 single-float) 32]
          [(integer-40 unsigned-40) 40]
          [(integer-48 unsigned-48) 48]
          [(integer-56 unsigned-56) 56]
          [(integer-64 unsigned-64 double-float) 64]
          [(scheme-object fixnum) (constant ptr-bits)]
          [(wchar) (constant wchar-bits)]
          [else ($oops who "unknown type ~s" x)])))
    (define offset-expr->index+offset
      (lambda (offset)
        (if (fixnum-constant? offset)
            (values %zero (constant-value offset))
            (values (build-unfix offset) 0))))
    (define-who build-int-load
      ;; assumes aligned (if required) offset
      (lambda (swapped? type base index offset build-int)
        (case type
          [(integer-8 unsigned-8)
           (build-int `(inline ,(make-info-load type #f) ,%load ,base ,index (immediate ,offset)))]
          [(integer-16 integer-32 unsigned-16 unsigned-32)
           (build-int `(inline ,(make-info-load type swapped?) ,%load ,base ,index (immediate ,offset)))]
          [(integer-64 unsigned-64)
           ;; NB: doesn't handle unknown endiannesss for 32-bit machines
           (constant-case ptr-bits
             [(32)
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 4) offset)
                                        (values offset (+ offset 4)))])
                (bind #t (base index)
                  (build-int
                    `(inline ,(make-info-load 'integer-32 swapped?) ,%load ,base ,index (immediate ,hi))
                    `(inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo)))))]
             [(64)
              (build-int `(inline ,(make-info-load type swapped?) ,%load ,base ,index (immediate ,offset)))])]
          [(integer-24 unsigned-24)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 1) offset)
                                        (values offset (+ offset 2)))])
                (define hi-type (if (eq? type 'integer-24) 'integer-8 'unsigned-8))
                (bind #t (base index)
                  (build-int
                    (%inline logor
                      ,(%inline sll
                         (inline ,(make-info-load hi-type #f) ,%load ,base ,index (immediate ,hi))
                         (immediate 16))
                      (inline ,(make-info-load 'unsigned-16 swapped?) ,%load ,base ,index (immediate ,lo))))))])]
          [(integer-40 unsigned-40)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 1) offset)
                                        (values offset (+ offset 4)))])
                (define hi-type (if (eq? type 'integer-40) 'integer-8 'unsigned-8))
                (bind #t (base index)
                  (constant-case ptr-bits
                    [(32)
                     (build-int
                       `(inline ,(make-info-load hi-type #f) ,%load ,base ,index (immediate ,hi))
                       `(inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo)))]
                    [(64)
                     (build-int
                       (%inline logor
                         ,(%inline sll
                            (inline ,(make-info-load hi-type #f) ,%load ,base ,index (immediate ,hi))
                            (immediate 32))
                         (inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo))))])))])]
          [(integer-48 unsigned-48)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 2) offset)
                                        (values offset (+ offset 4)))])
                (define hi-type (if (eq? type 'integer-48) 'integer-16 'unsigned-16))
                (bind #t (base index)
                  (constant-case ptr-bits
                    [(32)
                     (build-int
                       `(inline ,(make-info-load hi-type swapped?) ,%load ,base ,index (immediate ,hi))
                       `(inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo)))]
                    [(64)
                     (build-int
                       (%inline logor
                         ,(%inline sll
                            (inline ,(make-info-load hi-type swapped?) ,%load ,base ,index (immediate ,hi))
                            (immediate 32))
                         (inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo))))])))])]
          [(integer-56 unsigned-56)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (safe-assert (not (eq? (constant native-endianness) 'unknown)))
              (let-values ([(lo mi hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                           (values (+ offset 3) (+ offset 1) offset)
                                           (values offset (+ offset 4) (+ offset 6)))])
                (define hi-type (if (eq? type 'integer-56) 'integer-8 'unsigned-8))
                (bind #t (base index)
                  (constant-case ptr-bits
                    [(32)
                     (build-int
                       (%inline logor
                         ,(%inline sll
                            (inline ,(make-info-load hi-type #f) ,%load ,base ,index (immediate ,hi))
                            (immediate 16))
                         (inline ,(make-info-load 'unsigned-16 swapped?) ,%load ,base ,index (immediate ,mi)))
                       `(inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo)))]
                    [(64)
                     (build-int
                       (%inline logor
                         ,(%inline sll
                            ,(%inline logor
                               ,(%inline sll
                                  (inline ,(make-info-load hi-type #f) ,%load ,base ,index (immediate ,hi))
                                  (immediate 16))
                               (inline ,(make-info-load 'unsigned-16 swapped?) ,%load ,base ,index (immediate ,mi)))
                            (immediate 32))
                         (inline ,(make-info-load 'unsigned-32 swapped?) ,%load ,base ,index (immediate ,lo))))])))])]
          [else (sorry! who "unsupported type ~s" type)])))
    (define-who build-object-ref
      ;; assumes aligned (if required) offset
      (case-lambda
        [(swapped? type base offset-expr)
         (let-values ([(index offset) (offset-expr->index+offset offset-expr)])
           (build-object-ref swapped? type base index offset))]
        [(swapped? type base index offset)
         (case type
           [(scheme-object) `(inline ,(make-info-load ptr-type swapped?) ,%load ,base ,index (immediate ,offset))]
           [(double-float)
            (if swapped?
                (constant-case ptr-bits
                  [(32)
                   (bind #t (base index)
                     (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                       (%seq
                         (set! ,(%mref ,t ,(constant flonum-data-disp))
                           (inline ,(make-info-load 'unsigned-32 #t) ,%load ,base ,index
                             (immediate ,(+ offset 4))))
                         (set! ,(%mref ,t ,(+ (constant flonum-data-disp) 4))
                           (inline ,(make-info-load 'unsigned-32 #t) ,%load ,base ,index
                             (immediate ,offset)))
                         ,t)))]
                  [(64)
                   (bind #f (base index)
                     (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                       `(seq
                          (set! ,(%mref ,t ,(constant flonum-data-disp))
                            (inline ,(make-info-load 'unsigned-64 #t) ,%load ,base ,index
                              (immediate ,offset)))
                          ,t)))])
                (bind #f (base index)
                  (%mref ,base ,index ,offset fp)))]
           [(single-float)
            (if swapped?
                (bind #f (base index)
                  (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                    (%seq
                      (inline ,(make-info-load 'unsigned-32 #f) ,%store ,t ,%zero ,(%constant flonum-data-disp)
                        (inline ,(make-info-load 'unsigned-32 #t) ,%load ,base ,index
                          (immediate ,offset)))
                      (set! ,(%mref ,t ,%zero ,(constant flonum-data-disp) fp)
                            (unboxed-fp (inline ,(make-info-unboxed-args '(#t))
                                                ,%load-single->double
                                                ;; slight abuse to call this "unboxed", but `load-single->double`
                                                ;; wants an FP-flavored address
                                                ,(%mref ,t ,%zero ,(constant flonum-data-disp) fp))))
                      ,t)))
                (bind #f (base index)
                  (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                    (%seq
                      (set! ,(%mref ,t ,%zero ,(constant flonum-data-disp) fp)
                            (unboxed-fp (inline ,(make-info-unboxed-args '(#t))
                                                ,%load-single->double
                                                ;; slight abuse to call this "unboxed", but `load-single->double`
                                                ;; wants an FP-flavored address
                                                ,(%mref ,base ,index ,offset fp))))
                      ,t))))]
           [(integer-8 integer-16 integer-24 integer-32 integer-40 integer-48 integer-56 integer-64)
            (build-int-load swapped? type base index offset
              (if (and (eqv? (constant ptr-bits) 32) (memq type '(integer-40 integer-48 integer-56 integer-64)))
                  i32xu32->ptr
                  (lambda (x) (integer->ptr x (type->width type)))))]
           [(unsigned-8 unsigned-16 unsigned-24 unsigned-32 unsigned-40 unsigned-48 unsigned-56 unsigned-64)
            (build-int-load swapped? type base index offset
              (if (and (eqv? (constant ptr-bits) 32) (memq type '(unsigned-40 unsigned-48 unsigned-56 unsigned-64)))
                  u32xu32->ptr
                  (lambda (x) (unsigned->ptr x (type->width type)))))]
           [(fixnum) (build-fix `(inline ,(make-info-load ptr-type swapped?) ,%load ,base ,index (immediate ,offset)))]
           [else (sorry! who "unsupported type ~s" type)])]))
    (define-who build-int-store
      ;; assumes aligned (if required) offset
      (lambda (swapped? type base index offset value)
        (case type
          [(integer-8 unsigned-8)
           `(inline ,(make-info-load type #f) ,%store ,base ,index (immediate ,offset) ,value)]
          [(integer-16 integer-32 integer-64 unsigned-16 unsigned-32 unsigned-64)
           `(inline ,(make-info-load type swapped?) ,%store ,base ,index (immediate ,offset) ,value)]
          [(integer-24 unsigned-24)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 1) offset)
                                        (values offset (+ offset 2)))])
                (bind #t (base index value)
                  (%seq
                    (inline ,(make-info-load 'unsigned-16 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                    (inline ,(make-info-load 'unsigned-8 #f) ,%store ,base ,index (immediate ,hi)
                            ,(%inline srl ,value (immediate 16))))))])]
          [(integer-40 unsigned-40)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 1) offset)
                                        (values offset (+ offset 4)))])
                (bind #t (base index value)
                  (%seq
                    (inline ,(make-info-load 'unsigned-32 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                    (inline ,(make-info-load 'unsigned-8 #f) ,%store ,base ,index (immediate ,hi)
                      ,(%inline srl ,value (immediate 32))))))])]
          [(integer-48 unsigned-48)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                        (values (+ offset 2) offset)
                                        (values offset (+ offset 4)))])
                (bind #t (base index value)
                  (%seq
                    (inline ,(make-info-load 'unsigned-32 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                    (inline ,(make-info-load 'unsigned-16 swapped?) ,%store ,base ,index (immediate ,hi)
                      ,(%inline srl ,value (immediate 32))))))])]
          [(integer-56 unsigned-56)
           (constant-case native-endianness
             [(unknown) #f]
             [else
              (let-values ([(lo mi hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                           (values (+ offset 3) (+ offset 1) offset)
                                           (values offset (+ offset 4) (+ offset 6)))])
                (bind #t (base index value)
                  (%seq
                    (inline ,(make-info-load 'unsigned-32 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                    (inline ,(make-info-load 'unsigned-16 swapped?) ,%store ,base ,index (immediate ,mi)
                      ,(%inline srl ,value (immediate 32)))
                    (inline ,(make-info-load 'unsigned-8 #f) ,%store ,base ,index (immediate ,hi)
                      ,(%inline srl ,value (immediate 48))))))])]
          [else (sorry! who "unsupported type ~s" type)])))
    (define-who build-object-set!
      ;; assumes aligned (if required) offset
      (case-lambda
        [(type base offset-expr value)
         (let-values ([(index offset) (offset-expr->index+offset offset-expr)])
           (build-object-set! type base index offset value))]
        [(type base index offset value)
         (case type
           [(scheme-object) (build-dirty-store base index offset value)]
           [(double-float)
            (bind #f (base index)
              `(set! ,(%mref ,base ,index ,offset fp) ,value))]
           [(single-float)
            (bind #f (base index)
              `(inline ,(make-info-unboxed-args '(#t #t)) ,%store-double->single
                       ;; slight abuse to call this "unboxed", but `store-double->single`
                       ;; wants an FP-flavored address
                       ,(%mref ,base ,index ,offset fp)
                       (raw ,(%mref ,value ,%zero ,(constant flonum-data-disp) fp))))]
           ; 40-bit+ only on 64-bit machines
           [(integer-8 integer-16 integer-24 integer-32 integer-40 integer-48 integer-56 integer-64
             unsigned-8 unsigned-16 unsigned-24 unsigned-32 unsigned-40 unsigned-48 unsigned-56 unsigned-64)
            (build-int-store #f type base index offset (ptr->integer value (type->width type)))]
           [(fixnum)
            `(inline ,(make-info-load ptr-type #f) ,%store
               ,base ,index (immediate ,offset) ,(build-unfix value))]
           [else (sorry! who "unrecognized type ~s" type)])]))
    (define-who build-swap-object-set!
      (case-lambda
        [(type base offset-expr value)
         (let-values ([(index offset) (offset-expr->index+offset offset-expr)])
           (build-swap-object-set! type base index offset value))]
        [(type base index offset value)
         (case type
           ; only on 64-bit machines
           [(double-float)
            `(inline ,(make-info-load 'unsigned-64 #t) ,%store
               ,base ,index (immediate ,offset)
               (raw ,(%mref ,value ,(constant flonum-data-disp))))]
           ; 40-bit+ only on 64-bit machines
           [(integer-8 integer-16 integer-24 integer-32 integer-40 integer-48 integer-56 integer-64
             unsigned-8 unsigned-16 unsigned-24 unsigned-32 unsigned-40 unsigned-48 unsigned-56 unsigned-64)
            (build-int-store #t type base index offset (ptr->integer value (type->width type)))]
           [(fixnum)
            `(inline ,(make-info-load ptr-type #t) ,%store ,base ,index (immediate ,offset)
               ,(build-unfix value))]
           [else (sorry! who "unrecognized type ~s" type)])]))
    (define extract-unsigned-bitfield
      (lambda (raw? start end arg)
        (let* ([left (fx- (if raw? (constant ptr-bits) (constant fixnum-bits)) end)]
               [right (if raw? (fx- (fx+ left start) (constant fixnum-offset)) (fx+ left start))]
               [body (%inline srl
                        ,(if (fx= left 0)
                             arg
                             (%inline sll ,arg (immediate ,left)))
                        (immediate ,right))])
          (if (fx= start 0)
              body
              (%inline logand ,body (immediate ,(- (constant fixnum-factor))))))))
    (define extract-signed-bitfield
      (lambda (raw? start end arg)
        (let* ([left (fx- (if raw? (constant ptr-bits) (constant fixnum-bits)) end)]
               [right (if raw? (fx- (fx+ left start) (constant fixnum-offset)) (fx+ left start))])
          (let ([body (if (fx= left 0) arg (%inline sll ,arg (immediate ,left)))])
            (let ([body (if (fx= right 0) body (%inline sra ,body (immediate ,right)))])
              (if (fx= start 0)
                  body
                  (%inline logand ,body (immediate ,(- (constant fixnum-factor))))))))))
    (define insert-bitfield
      (lambda (raw? start end bf-width arg val)
        (if raw?
            (cond
              [(fx= start 0)
               (%inline logor
                  ,(%inline sll
                    ,(%inline srl ,arg (immediate ,end))
                    (immediate ,end))
                  ,(%inline srl
                    ,(%inline sll ,val (immediate ,(fx- (constant fixnum-bits) end)))
                    (immediate ,(fx- (constant ptr-bits) end))))]
              [(fx= end bf-width)
               (%inline logor
                  ,(%inline srl
                    ,(%inline sll ,arg
                      (immediate ,(fx- (constant ptr-bits) start)))
                    (immediate ,(fx- (constant ptr-bits) start)))
                  ,(cond
                     [(fx< start (constant fixnum-offset))
                      (%inline srl ,val
                         (immediate ,(fx- (constant fixnum-offset) start)))]
                     [(fx> start (constant fixnum-offset))
                      (%inline sll ,val
                         (immediate ,(fx- start (constant fixnum-offset))))]
                     [else val]))]
              [else
               (%inline logor
                  ,(%inline logand ,arg
                    (immediate ,(lognot (ash (- (expt 2 (fx- end start)) 1) start))))
                  ,(%inline srl
                    ,(if (fx= (fx- end start) (constant fixnum-bits))
                         val
                         (%inline sll ,val
                            (immediate ,(fx- (constant fixnum-bits) (fx- end start)))))
                    (immediate ,(fx- (constant ptr-bits) end))))])
            (cond
              [(fx= start 0)
               (%inline logor
                  ,(%inline sll
                    ,(%inline srl ,arg (immediate ,(fx+ end (constant fixnum-offset))))
                    (immediate ,(fx+ end (constant fixnum-offset))))
                  ,(%inline srl
                    ,(%inline sll ,val (immediate ,(fx- (constant fixnum-bits) end)))
                    (immediate ,(fx- (constant fixnum-bits) end))))]
              #;[(fx= end (constant fixnum-bits)) ---] ; end < fixnum-bits
              [else
               (%inline logor
                  ,(%inline logand ,arg
                    (immediate ,(lognot (ash (- (expt 2 (fx- end start)) 1)
                                             (fx+ start (constant fixnum-offset))))))
                  ,(%inline srl
                    ,(%inline sll ,val
                      (immediate ,(fx- (constant fixnum-bits) (fx- end start))))
                    (immediate ,(fx- (constant fixnum-bits) end))))]))))
    (define translate
      (lambda (e current-shift target-shift)
        (let ([delta (fx- current-shift target-shift)])
          (if (fx= delta 0)
              e
              (if (fx< delta 0)
                  (%inline sll ,e (immediate ,(fx- delta)))
                  (let ([shift-e (%inline srl ,e (immediate ,delta))])
                    (if (fx= target-shift (constant fixnum-offset))
                        shift-e
                        `(raw ,shift-e))))))))
    (define extract-length
      (lambda (t/l length-offset)
        (%inline logand
          ,(translate t/l length-offset (constant fixnum-offset))
          (immediate ,(- (constant fixnum-factor))))))
    (define build-type/length
      (lambda (e type current-shift target-shift)
        (let ([e (translate e current-shift target-shift)])
          (if (eqv? type 0)
              e
              (%inline logor ,e (immediate ,type))))))
    (define-syntax build-ref-check
      (syntax-rules ()
        [(_ type-disp maximum-length length-offset type mask immutable-flag)
         (lambda (e-v e-i maybe-e-new)
           ; NB: caller must bind e-v, e-i, and maybe-e-new
           (safe-assert (no-need-to-bind? #t e-v))
           (safe-assert (no-need-to-bind? #t e-i))
           (safe-assert (or (not maybe-e-new) (no-need-to-bind? #t maybe-e-new)))
           (build-and
             (%type-check mask-typed-object type-typed-object ,e-v)
             (bind #t ([t (%mref ,e-v ,(constant type-disp))])
               (cond
                 [(expr->index e-i 1 (constant maximum-length)) =>
                  (lambda (index)
                    (let ([e (%inline u<
                               (immediate ,(logor (ash index (constant length-offset)) (constant type) (constant immutable-flag)))
                               ,t)])
                      (if (and (eqv? (constant type) (constant type-fixnum))
                               (eqv? (constant mask) (constant mask-fixnum)))
                          (build-and e (build-fixnums? (if maybe-e-new (list t maybe-e-new) (list t))))
                          (build-and
                            (%type-check mask type ,t)
                            (if maybe-e-new (build-and e (build-fixnums? (list maybe-e-new))) e)))))]
                 [else
                  (let ([e (%inline u< ,e-i ,(extract-length t (constant length-offset)))])
                    (if (and (eqv? (constant type) (constant type-fixnum))
                             (eqv? (constant mask) (constant mask-fixnum)))
                        (build-and e (build-fixnums? (if maybe-e-new (list e-i t maybe-e-new) (list e-i t))))
                        (build-and
                          (%type-check mask type ,t)
                          (build-and
                            (build-fixnums? (if maybe-e-new (list e-i maybe-e-new) (list e-i)))
                            e))))]))))]))
    (define-syntax build-set-immutable!
      (syntax-rules ()
        [(_ type-disp immutable-flag)
         (lambda (e-v)
           (bind #t (e-v)
                 `(set! ,(%mref ,e-v ,(constant type-disp))
                   ,(%inline logor
                             ,(%mref ,e-v ,(constant type-disp))
                             (immediate ,(constant immutable-flag))))))]))
    (define inline-args-limit (constant inline-args-limit))
    (define reduce-equality
      (lambda (src sexpr moi e1 e2 e*)
        (and (fx<= (length e*) (fx- inline-args-limit 2))
             (bind #t (e1)
               (bind #f (e2)
                 (list-bind #f (e*)
                   (let compare ([src src] [e2 e2] [e* e*])
                     (if (null? e*)
                         (moi src sexpr (list e1 e2))
                         `(if ,(moi src sexpr (list e1 e2))
                              ,(compare #f (car e*) (cdr e*))
                              (quote #f))))))))))
    (define reduce-inequality
      (lambda (src sexpr moi e1 e2 e*)
        (and (fx<= (length e*) (fx- inline-args-limit 2))
             (let f ([e2 e2] [e* e*] [re* '()])
               (if (null? e*)
                   (bind #f ([e2 e2])
                     (let compare ([src src] [e* (cons e1 (reverse (cons e2 re*)))])
                       (let ([more-args (cddr e*)])
                         (if (null? more-args)
                             (moi src sexpr e*)
                             `(if ,(moi src sexpr (list (car e*) (cadr e*)))
                                  ,(compare #f (cdr e*))
                                  (quote #f))))))
                   (bind #t ([e2 e2]) (f (car e*) (cdr e*) (cons e2 re*))))))))
    (define reduce ; left associative as required for, e.g., fx-
      (lambda (src sexpr moi e e*)
        (and (fx<= (length e*) (fx- inline-args-limit 1))
             (bind #f (e)
               (list-bind #f ([e* e*])
                 (let reduce ([src src] [e e] [e* e*])
                   (if (null? e*)
                       e
                       (reduce #f (moi src sexpr (list e (car e*))) (cdr e*)))))))))
    (define reduce-fp-compare ; suitable for arguments known or assumed to produce flonums
      (lambda (reduce)
        (lambda (src sexpr moi e1 e2 e*)
          (and (fx<= (length e*) (fx- inline-args-limit 2))
               (bind #t fp (e1)
                 (bind #f fp (e2)
                   (list-bind #f fp (e*)
                      (reduce src sexpr moi e1 e2 e*))))))))
    (define reduce-fp ; specialized reducer supports unboxing for nesting
      (lambda (src sexpr level name e e*)
        (and (fx<= (length e*) (fx- inline-args-limit 1))
             (let ([pr (lookup-primref level name)])
               (let reduce ([e e] [src src] [sexpr sexpr] [e* e*])
                 (if (null? e*)
                     e
                     (reduce `(call ,(make-info-call src sexpr #f #f #f) #f ,pr ,e ,(car e*))
                             #f #f (cdr e*))))))))
    (module (relop-length RELOP< RELOP<= RELOP= RELOP>= RELOP>)
      (define RELOP< -2)
      (define RELOP<= -1)
      (define RELOP= 0)
      (define RELOP>= 1)
      (define RELOP> 2)
      (define (mirror op) (fx- op))
      (define go
        (lambda (op e n)
          (let f ([n n] [e e])
            (if (fx= n 0)
                (cond
                  [(or (eqv? op RELOP=) (eqv? op RELOP<=)) (build-null? e)]
                  [(eqv? op RELOP<) `(seq ,e (quote #f))]
                  [(eqv? op RELOP>) (build-not (build-null? e))]
                  [(eqv? op RELOP>=) `(seq ,e (quote #t))]
                  [else (sorry! 'relop-length "unexpected op ~s" op)])
                (cond
                  [(or (eqv? op RELOP=) (eqv? op RELOP>))
                   (bind #t (e)
                     (build-and
                       (build-not (build-null? e))
                       (f (fx- n 1) (build-cdr e))))]
                  [(eqv? op RELOP<)
                   (if (fx= n 1)
                       (build-null? e)
                       (bind #t (e)
                         (build-simple-or
                           (build-null? e)
                           (f (fx- n 1) (build-cdr e)))))]
                  [(eqv? op RELOP<=)
                   (bind #t (e)
                     (build-simple-or
                       (build-null? e)
                       (f (fx- n 1) (build-cdr e))))]
                  [(eqv? op RELOP>=)
                   (if (fx= n 1)
                       (build-not (build-null? e))
                       (bind #t (e)
                         (build-and
                           (build-not (build-null? e))
                           (f (fx- n 1) (build-cdr e)))))]
                  [else (sorry! 'relop-length "unexpected op ~s" op)])))))
      (define relop-length1
        (lambda (op e n)
          (nanopass-case (L7 Expr) e
            [(call ,info ,mdcl ,pr ,e)
             (guard (and (eq? (primref-name pr) 'length) (all-set? (prim-mask unsafe) (primref-flags pr))))
             (go op e n)]
            [else #f])))
      (define relop-length2
        (lambda (op e1 e2)
          (nanopass-case (L7 Expr) e2
            [(quote ,d) (and (fixnum? d) (fx<= 0 d 4) (relop-length1 op e1 d))]
            [else #f])))
      (define relop-length
        (case-lambda
          [(op e) (relop-length1 op e 0)]
          [(op e1 e2) (or (relop-length2 op e1 e2) (relop-length2 (mirror op) e2 e1))])))
    (define make-ftype-pointer-equal?
      (lambda (e1 e2)
        (bind #f (e1 e2)
          (%inline eq? ;; raw operands safe given bind
            ,(%mref ,e1 ,(constant record-data-disp))
            ,(%mref ,e2 ,(constant record-data-disp))))))
    (define make-ftype-pointer-null?
      (lambda (e)
        (%inline eq?
          ,(%mref ,e ,(constant record-data-disp))
          (immediate 0))))
    (define eqvop-null-fptr
      (lambda (e1 e2)
        (nanopass-case (L7 Expr) e1
          [(call ,info ,mdcl ,pr ,e1)
           (and
             (eq? (primref-name pr) 'ftype-pointer-address)
             (all-set? (prim-mask unsafe) (primref-flags pr))
             (nanopass-case (L7 Expr) e2
               [(quote ,d)
                (and (eqv? d 0) (make-ftype-pointer-null? e1))]
               [(call ,info ,mdcl ,pr ,e2)
                (and (eq? (primref-name pr) 'ftype-pointer-address)
                     (all-set? (prim-mask unsafe) (primref-flags pr))
                     (make-ftype-pointer-equal? e1 e2))]
               [else #f]))]
          [(quote ,d)
           (and (eqv? d 0)
                (nanopass-case (L7 Expr) e2
                  [(call ,info ,mdcl ,pr ,e2)
                   (and (eq? (primref-name pr) 'ftype-pointer-address)
                        (all-set? (prim-mask unsafe) (primref-flags pr))
                        (make-ftype-pointer-null? e2))]
                  [else #f]))]
          [else #f])))
    (define-inline 2 values
      [(e) (ensure-single-valued e)]
      [e* `(values ,(make-info-call src sexpr #f #f #f) ,e* ...)])
    (define-inline 2 $value
      [(e) (ensure-single-valued e #f)])
    (define-inline 2 eq?
      [(e1 e2)
       (or (eqvop-null-fptr e1 e2)
           (relop-length RELOP= e1 e2)
           (%inline eq? ,e1 ,e2))])
    (define-inline 2 keep-live
      [(e) (%seq ,(%inline keep-live ,e) ,(%constant svoid))])
    (let ()
      (define (zgo src sexpr e e1 e2 r6rs?)
        (build-simple-or
          (%inline eq? ,e (immediate 0))
          `(if ,(build-fixnums? (list e))
               ,(%constant sfalse)
               ,(if r6rs?
                    (build-libcall #t src sexpr fx=? e1 e2)
                    (build-libcall #t src sexpr fx= e1 e2)))))
      (define (go src sexpr e1 e2 r6rs?)
        (or (relop-length RELOP= e1 e2)
            (cond
              [(constant? (lambda (x) (eqv? x 0)) e1)
               (bind #t (e2) (zgo src sexpr e2 e1 e2 r6rs?))]
              [(constant? (lambda (x) (eqv? x 0)) e2)
               (bind #t (e1) (zgo src sexpr e1 e1 e2 r6rs?))]
              [else (bind #t (e1 e2)
                      `(if ,(build-fixnums? (list e1 e2))
                           ,(%inline eq? ,e1 ,e2)
                           ,(if r6rs?
                                (build-libcall #t src sexpr fx=? e1 e2)
                                (build-libcall #t src sexpr fx= e1 e2))))])))
      (define-inline 2 fx=
        [(e1 e2) (go src sexpr e1 e2 #f)]
        [(e1 . e*) #f])
      (define-inline 2 fx=?
        [(e1 e2) (go src sexpr e1 e2 #t)]
        [(e1 e2 . e*) #f]))
    (let () ; level 2 fx<, fx<?, etc.
      (define-syntax fx-pred
        (syntax-rules ()
          [(_ op r6rs:op length-op inline-op)
           (let ()
             (define (go src sexpr e1 e2 r6rs?)
               (or (relop-length length-op e1 e2)
                   (bind #t (e1 e2)
                     `(if ,(build-fixnums? (list e1 e2))
                          ,(%inline inline-op ,e1 ,e2)
                          ,(if r6rs?
                               (build-libcall #t src sexpr r6rs:op e1 e2)
                               (build-libcall #t src sexpr op e1 e2))))))
             (define-inline 2 op
               [(e1 e2) (go src sexpr e1 e2 #f)]
               ; TODO: 3-operand case requires 3-operand library routine
               #;[(e1 e2 e3) (go3 src sexpr e1 e2 e3 #f)]
               [(e1 . e*) #f])
             (define-inline 2 r6rs:op
               [(e1 e2) (go src sexpr e1 e2 #t)]
               ; TODO: 3-operand case requires 3-operand library routine
               #; [(e1 e2 e3) (go3 src sexpr e1 e2 e3 #t)]
               [(e1 e2 . e*) #f]))]))
      (fx-pred fx< fx<? RELOP< <)
      (fx-pred fx<= fx<=? RELOP<= <=)
      (fx-pred fx>= fx>=? RELOP>= >=)
      (fx-pred fx> fx>? RELOP> >))
    (let () ; level 3 fx=, fx=?, etc.
      (define-syntax fx-pred
        (syntax-rules ()
          [(_ op r6rs:op length-op inline-op)
           (let ()
             (define (go e1 e2)
               (or (relop-length length-op e1 e2)
                   (%inline inline-op ,e1 ,e2)))
             (define reducer
               (if (eq? 'inline-op 'eq?)
                   reduce-equality
                   reduce-inequality))
             (define-inline 3 op
               [(e) `(seq ,(ensure-single-valued e) ,(%constant strue))]
               [(e1 e2) (go e1 e2)]
               [(e1 e2 . e*) (reducer src sexpr moi e1 e2 e*)])
             (define-inline 3 r6rs:op
               [(e1 e2) (go e1 e2)]
               [(e1 e2 . e*) (reducer src sexpr moi e1 e2 e*)]))]))
      (fx-pred fx< fx<? RELOP< <)
      (fx-pred fx<= fx<=? RELOP<= <=)
      (fx-pred fx= fx=? RELOP= eq?)
      (fx-pred fx>= fx>=? RELOP>= >=)
      (fx-pred fx> fx>? RELOP> >))
    (let () ; level 3 fxlogand, ...
      (define-syntax fxlogop
        (syntax-rules ()
          [(_ op inline-op base)
           (define-inline 3 op
             [() `(immediate ,(fix base))]
             [(e) (ensure-single-valued e)]
             [(e1 e2) (%inline inline-op ,e1 ,e2)]
             [(e1 . e*) (reduce src sexpr moi e1 e*)])]))
      (fxlogop fxlogand logand -1)
      (fxlogop fxand logand -1)
      (fxlogop fxlogor logor 0)
      (fxlogop fxlogior logor 0)
      (fxlogop fxior logor 0)
      (fxlogop fxlogxor logxor 0)
      (fxlogop fxxor logxor 0))
    (let ()
      (define log-partition
        (lambda (p base e*)
          (let loop ([e* e*] [n base] [nc* '()])
            (if (null? e*)
                (if (and (fixnum? n) (fx= n base) (not (null? nc*)))
                    (values (car nc*) (cdr nc*) nc*)
                    (values `(immediate ,(fix n)) nc* nc*))
                (let ([e (car e*)])
                  (if (fixnum-constant? e)
                      (let ([m (constant-value e)])
                        (loop (cdr e*) (if n (p n m) m) nc*))
                      (loop (cdr e*) n (cons e nc*))))))))
      (let () ; level 2 fxlogor, fxlogior, fxor
        (define-syntax fxlogorop
          (syntax-rules ()
            [(_ op)
             (let ()
               (define (go src sexpr e*)
                 (and (fx<= (length e*) inline-args-limit)
                      (list-bind #t (e*)
                        (let-values ([(e e* nc*) (log-partition logor 0 e*)])
                          (bind #t ([t (fold-left (lambda (e1 e2) (%inline logor ,e1 ,e2)) e e*)])
                            `(if ,(%type-check mask-fixnum type-fixnum ,t)
                                 ,t
                                 ,(case (length nc*)
                                    [(1) (build-libcall #t src sexpr op (car nc*) `(immediate ,(fix 0)))]
                                    [(2) (build-libcall #t src sexpr op (car nc*) (cadr nc*))]
                                    ; TODO: need fxargerr library routine w/who arg & rest interface
                                    [else `(call ,(make-info-call src sexpr #f #t #t) #f ,(Symref 'op) ,nc* (... ...))]))))))) ; NB: should be error call---but is it?
               (define-inline 2 op
                 [() `(immediate ,(fix 0))]
                 [e* (go src sexpr e*)]))]))
        (fxlogorop fxlogor)
        (fxlogorop fxlogior)
        (fxlogorop fxior))
      (let () ; level 2 fxlogand, ...
        (define-syntax fxlogop
          (syntax-rules ()
            [(_ op inline-op base)
             (define-inline 2 op
               [() `(immediate ,(fix base))]
               [e* (and (fx<= (length e*) (fx- inline-args-limit 1))
                        (list-bind #t (e*)
                          ;; NB: using inline-op here because it works when target's
                          ;; NB: fixnum range is larger than the host's fixnum range
                          ;; NB: during cross compile
                          (let-values ([(e e* nc*) (log-partition inline-op base e*)])
                            `(if ,(build-fixnums? nc*)
                                 ,(fold-left (lambda (e1 e2) (%inline inline-op ,e1 ,e2)) e e*)
                                 ; TODO: need fxargerr library routine w/who arg & rest interface
                                 ,(case (length nc*)
                                    [(1) (build-libcall #t src sexpr op (car nc*) `(immediate ,(fix 0)))]
                                    [(2) (build-libcall #t src sexpr op (car nc*) (cadr nc*))]
                                    ; TODO: need fxargerr library routine w/who arg & rest interface
                                    [else `(call ,(make-info-call src sexpr #f #t #t) #f ,(Symref 'op) ,nc* (... ...))])))))])])) ; NB: should be error call---but is it?
        (fxlogop fxlogand logand -1)
        (fxlogop fxand logand -1)
        (fxlogop fxlogxor logxor 0)
        (fxlogop fxxor logxor 0)))
    (define-inline 3 fxlogtest
      [(e1 e2) (%inline logtest ,e1 ,e2)])
    (define-inline 2 fxlogtest
      [(e1 e2)
       (bind #t (e1 e2)
         `(if ,(build-fixnums? (list e1 e2))
              ,(%inline logtest ,e1 ,e2)
              ,(build-libcall #t src sexpr fxlogtest e1 e2)))])
    (let ()
      (define xorbits (lognot (constant mask-fixnum)))
      (define-syntax fxlognotop
        (syntax-rules ()
          [(_ name)
           (begin
             (define-inline 3 name
               [(e) (%inline logxor ,e (immediate ,xorbits))])
             (define-inline 2 name
               [(e) (bind #t (e)
                      `(if ,(%type-check mask-fixnum type-fixnum ,e)
                           ,(%inline logxor ,e (immediate ,xorbits))
                           ,(build-libcall #t src sexpr name e)))]))]))
      (fxlognotop fxlognot)
      (fxlognotop fxnot))
    (define-inline 3 $fxu<
      [(e1 e2) (or (relop-length RELOP< e1 e2)
                   (%inline u< ,e1 ,e2))])
    (define-inline 3 fx+
      [() `(immediate 0)]
      [(e) (ensure-single-valued e)]
      [(e1 e2) (%inline + ,e1 ,e2)]
      [(e1 . e*) (reduce src sexpr moi e1 e*)])
    (define-inline 3 r6rs:fx+ ; limited to two arguments
      [(e1 e2) (%inline + ,e1 ,e2)])
    (define-inline 3 fx+/wraparound
      [(e1 e2) (%inline + ,e1 ,e2)])
    (define-inline 3 fx1+
      [(e) (%inline + ,e (immediate ,(fix 1)))])
    (define-inline 2 $fx+?
      [(e1 e2)
       (let ([Lfalse (make-local-label 'Lfalse)])
         (bind #t (e1 e2)
           `(if ,(build-fixnums? (list e1 e2))
                ,(bind #f ([t (%inline +/ovfl ,e1 ,e2)])
                   `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                        (label ,Lfalse ,(%constant sfalse))
                        ,t))
                (goto ,Lfalse))))])
    (let ()
      (define (go src sexpr e1 e2)
        (let ([Llib (make-local-label 'Llib)])
          (bind #t (e1 e2)
            `(if ,(build-fixnums? (list e1 e2))
                 ,(bind #f ([t (%inline +/ovfl ,e1 ,e2)])
                    `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                         (label ,Llib ,(build-libcall #t src sexpr fx+ e1 e2))
                         ,t))
                 (goto ,Llib)))))
      (define-inline 2 fx+
        [() `(immediate 0)]
        [(e)
         (bind #t (e)
           `(if ,(%type-check mask-fixnum type-fixnum ,e)
                ,e
                ,(build-libcall #t #f sexpr fx+ e `(immediate ,(fix 0)))))]
        [(e1 e2) (go src sexpr e1 e2)]
        ; TODO: 3-operand case requires 3-operand library routine
        #;[(e1 e2 e3)
         (let ([Llib (make-local-label 'Llib)])
           (bind #t (e1 e2 e3)
             `(if ,(build-fixnums? (list e1 e2 e3))
                  ,(bind #t ([t (%inline +/ovfl ,e1 ,e2)])
                     `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                          (label ,Llib ,(build-libcall #t src sexpr fx+ e1 e2 e3))
                          ,(bind #t ([t (%inline +/ovfl ,t ,e3)])
                             `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                                  (goto ,Llib)
                                  ,t))))
                  (goto ,Llib))))]
        [(e1 . e*) #f])
      (define-inline 2 r6rs:fx+ ; limited to two arguments
        [(e1 e2) (go src sexpr e1 e2)])
      (define-inline 2 fx+/wraparound
        [(e1 e2)
          (bind #t (e1 e2)
            `(if ,(build-fixnums? (list e1 e2))
                 ,(%inline + ,e1 ,e2)
                 ,(build-libcall #t src sexpr fx+/wraparound e1 e2)))]))

    (define-inline 3 fx-
      [(e) (%inline - (immediate 0) ,e)]
      [(e1 e2) (%inline - ,e1 ,e2)]
      [(e1 . e*) (reduce src sexpr moi e1 e*)])
    (define-inline 3 r6rs:fx- ; limited to one or two arguments
      [(e) (%inline - (immediate 0) ,e)]
      [(e1 e2) (%inline - ,e1 ,e2)])
    (define-inline 3 fx-/wraparound
      [(e) (%inline - (immediate 0) ,e)]
      [(e1 e2) (%inline - ,e1 ,e2)])
    (define-inline 3 fx1-
      [(e) (%inline - ,e (immediate ,(fix 1)))])
    (define-inline 2 $fx-?
      [(e1 e2)
       (let ([Lfalse (make-local-label 'Lfalse)])
         (bind #t (e1 e2)
           `(if ,(build-fixnums? (list e1 e2))
                ,(bind #f ([t (%inline -/ovfl ,e1 ,e2)])
                   `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                        (label ,Lfalse ,(%constant sfalse))
                        ,t))
                (goto ,Lfalse))))])
    (let ()
      (define (go src sexpr e1 e2)
        (let ([Llib (make-local-label 'Llib)])
          (bind #t (e1 e2)
            `(if ,(build-fixnums? (list e1 e2))
                 ,(bind #t ([t (%inline -/ovfl ,e1 ,e2)])
                    `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                         (label ,Llib ,(build-libcall #t src sexpr fx- e1 e2))
                         ,t))
                 (goto ,Llib)))))
      (define-inline 2 fx-
        [(e) (go src sexpr `(immediate ,(fix 0)) e)]
        [(e1 e2) (go src sexpr e1 e2)]
        ; TODO: 3-operand case requires 3-operand library routine
        #;[(e1 e2 e3)
         (let ([Llib (make-local-label 'Llib)])
           (bind #t (e1 e2 e3)
             `(if ,(build-fixnums? (list e1 e2 e3))
                  ,(bind #t ([t (%inline -/ovfl ,e1 ,e2)])
                     `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                          (label ,Llib ,(build-libcall #t src sexpr fx- e1 e2 e3))
                          ,(bind #t ([t (%inline -/ovfl ,t ,e3)])
                             `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                                  (goto ,Llib)
                                  ,t))))
                  (goto ,Llib))))]
        [(e1 . e*) #f])
      (define-inline 2 r6rs:fx- ; limited to one or two arguments
        [(e) (go src sexpr `(immediate ,(fix 0)) e)]
        [(e1 e2) (go src sexpr e1 e2)])
      (define-inline 2 fx-/wraparound
        [(e)
         (bind #t (e)
           `(if ,(build-fixnums? (list e))
                ,(%inline - (immediate 0) ,e)
                ,(build-libcall #t src sexpr fx-/wraparound `(immediate 0) e)))]
        [(e1 e2)
         (bind #t (e1 e2)
           `(if ,(build-fixnums? (list e1 e2))
                ,(%inline - ,e1 ,e2)
                ,(build-libcall #t src sexpr fx-/wraparound e1 e2)))]))
    (define-inline 2 fx1-
      [(e) (let ([Llib (make-local-label 'Llib)])
             (bind #t (e)
               `(if ,(build-fixnums? (list e))
                    ,(bind #t ([t (%inline -/ovfl ,e (immediate ,(fix 1)))])
                       `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                            (label ,Llib ,(build-libcall #t src sexpr fx1- e))
                            ,t))
                    (goto ,Llib))))])
    (define-inline 2 fx1+
      [(e) (let ([Llib (make-local-label 'Llib)])
             (bind #t (e)
               `(if ,(build-fixnums? (list e))
                    ,(bind #f ([t (%inline +/ovfl ,e (immediate ,(fix 1)))])
                       `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                            (label ,Llib ,(build-libcall #t src sexpr fx1+ e))
                            ,t))
                    (goto ,Llib))))])

    (let ()
      (define-inline 3 fxdiv
        [(e1 e2)
         (nanopass-case (L7 Expr) e2
           [(quote ,d)
            (let ([n (target-fixnum-power-of-two d)])
              (and n
                   (%inline logand
                      ,(%inline sra ,e1 (immediate ,n))
                      (immediate ,(- (constant fixnum-factor))))))]
           [else #f])])
      (define-inline 3 fxmod
        [(e1 e2)
         (nanopass-case (L7 Expr) e2
           [(quote ,d)
            (and (target-fixnum-power-of-two d)
                 (%inline logand ,e1 (immediate ,(fix (- d 1)))))]
           [else #f])])
      (let ()
        (define (build-fx* e1 e2 ovfl?)
          (define (fx*-constant e n)
            (if ovfl?
                (%inline */ovfl ,e (immediate ,n))
                (cond
                  [(eqv? n 1) e]
                  [(eqv? n -1) (%inline - (immediate 0) ,e)]
                  [(eqv? n 2) (%inline sll ,e (immediate 1))]
                  [(eqv? n 3)
                   (bind #t (e)
                     (%inline +
                       ,(%inline + ,e ,e)
                       ,e))]
                  [(eqv? n 10)
                   (bind #t (e)
                     (%inline +
                       ,(%inline +
                          ,(%inline sll ,e (immediate 3))
                          ,e)
                       ,e))]
                  [(target-fixnum-power-of-two n) =>
                   (lambda (i) (%inline sll ,e (immediate ,i)))]
                  [else (%inline * ,e (immediate ,n))])))
          (nanopass-case (L7 Expr) e2
            [(quote ,d) (guard (target-fixnum? d)) (fx*-constant e1 d)]
            [else
             (nanopass-case (L7 Expr) e1
               [(quote ,d) (guard (target-fixnum? d)) (fx*-constant e2 d)]
               [else
                (let ([t (make-tmp 't 'uptr)])
                  `(let ([,t ,(build-unfix e2)])
                     ,(if ovfl?
                          (%inline */ovfl ,e1 ,t)
                          (%inline * ,e1 ,t))))])]))
        (define-inline 3 fx*
          [() `(immediate ,(fix 1))]
          [(e) (ensure-single-valued e)]
          [(e1 e2) (build-fx* e1 e2 #f)]
          [(e1 . e*) (reduce src sexpr moi e1 e*)])
        (define-inline 3 r6rs:fx* ; limited to two arguments
          [(e1 e2) (build-fx* e1 e2 #f)])
        (define-inline 3 fx*/wraparound
          [(e1 e2) (build-fx* e1 e2 #f)])
        (let ()
          (define (go src sexpr e1 e2)
            (let ([Llib (make-local-label 'Llib)])
              (bind #t (e1 e2)
                `(if ,(build-fixnums? (list e1 e2))
                     ,(bind #t ([t (build-fx* e1 e2 #t)])
                        `(if (inline ,(make-info-condition-code 'multiply-overflow #f #t) ,%condition-code)
                             (label ,Llib ,(build-libcall #t src sexpr fx* e1 e2))
                             ,t))
                     (goto ,Llib)))))
          (define-inline 2 fx*
            [() `(immediate ,(fix 1))]
            [(e)
             (bind #t (e)
               `(if ,(%type-check mask-fixnum type-fixnum ,e)
                    ,e
                    ,(build-libcall #t src sexpr fx* e `(immediate ,(fix 0)))))]
            [(e1 e2) (go src sexpr e1 e2)]
            ; TODO: 3-operand case requires 3-operand library routine
            #;[(e1 e2 e3)
             (let ([Llib (make-local-label 'Llib)])
               (bind #t (e1 e2 e3)
                 `(if ,(build-fixnums? (list e1 e2 e3))
                      ,(bind #t ([t (build-fx* e1 e2 #t)])
                         `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                              (label ,Llib ,(build-libcall #t src sexpr fx* e1 e2 e3))
                              ,(bind #t ([t (build-fx* t e3 #t)])
                                 `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                                      (goto ,Llib)
                                      ,t))))
                      (goto ,Llib))))]
            [(e1 . e*) #f])
          (define-inline 2 r6rs:fx* ; limited to two arguments
            [(e1 e2) (go src sexpr e1 e2)])
          (define-inline 2 fx*/wraparound
            [(e1 e2)
             (bind #t (e1 e2)
               `(if ,(build-fixnums? (list e1 e2))
                    ,(build-fx* e1 e2 #f)
                    ,(build-libcall #t src sexpr fx*/wraparound e1 e2)))]))
        (let ()
          (define build-fx/p2
            (lambda (e1 p2)
              (bind #t (e1)
                (build-fix
                  (%inline sra
                     ,(%inline + ,e1
                       ,(%inline srl
                         ,(if (fx= p2 1)
                              e1
                              (%inline sra ,e1 (immediate ,(fx- p2 1))))
                         (immediate ,(fx- (constant fixnum-bits) p2))))
                     (immediate ,(fx+ p2 (constant fixnum-offset))))))))

          (define build-fx/
            (lambda (src sexpr e1 e2)
              (or (nanopass-case (L7 Expr) e2
                    [(quote ,d)
                     (let ([i (target-fixnum-power-of-two d)])
                       (and i (build-fx/p2 e1 i)))]
                    [else #f])
                  (if (constant integer-divide-instruction)
                      (build-fix (%inline / ,e1 ,e2))
                      `(call ,(make-info-call src sexpr #f #f #f) #f
                         ,(lookup-primref 3 '$fx/)
                         ,e1 ,e2)))))

          (define-inline 3 fx/
            [(e) (build-fx/ src sexpr `(quote 1) e)]
            [(e1 e2) (build-fx/ src sexpr e1 e2)]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 fxquotient
            [(e) (build-fx/ src sexpr `(quote 1) e)]
            [(e1 e2) (build-fx/ src sexpr e1 e2)]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 fxremainder
            [(e1 e2)
             (bind #t (e1 e2)
               (%inline - ,e1
                  ,(build-fx*
                     (build-fx/ src sexpr e1 e2)
                     e2 #f)))]))
        (let ()
          (define-syntax build-fx
            (lambda (x)
              (syntax-case x ()
                [(_ op a1 a2)
                 #`(%inline op
                            #,(if (number? (syntax->datum #'a1))
                                  #`(immediate a1)
                                  #`,a1)
                            #,(if (number? (syntax->datum #'a2))
                                  #`(immediate a2)
                                  #`,a2))])))
          (define (build-popcount16 e)
            (constant-case popcount-instruction
              [(#t) (build-fix (%inline popcount ,e))] ; no unfix needed, since not specialized to 16-bit
              [else
               (let ([x (make-tmp 'x 'uptr)]
                     [x2 (make-tmp 'x2 'uptr)]
                     [x3 (make-tmp 'x3 'uptr)]
                     [x4 (make-tmp 'x4 'uptr)])
                 `(let ([,x ,(build-unfix e)])
                    (let ([,x2 ,(build-fx - x (build-fx logand (build-fx srl x 1) #x5555))])
                      (let ([,x3 ,(build-fx + (build-fx logand x2 #x3333) (build-fx logand (build-fx srl x2 2) #x3333))])
                        (let ([,x4 ,(build-fx logand (build-fx + x3 (build-fx srl x3 4)) #x0f0f)])
                          ,(build-fix (build-fx logand (build-fx + x4 (build-fx srl x4 8)) #x1f)))))))]))
          (define (build-popcount32 e)
            (constant-case popcount-instruction
              [(#t) (build-fix (%inline popcount ,e))] ; no unfix needed, since not specialized to 32-bit
              [else
               (let ([x (make-tmp 'x 'uptr)]
                     [x2 (make-tmp 'x2 'uptr)]
                     [x3 (make-tmp 'x3 'uptr)]
                     [x4 (make-tmp 'x4 'uptr)])
                 `(let ([,x ,(build-unfix e)])
                    (let ([,x2 ,(build-fx - x (build-fx logand (build-fx srl x 1) #x55555555))])
                      (let ([,x3 ,(build-fx + (build-fx logand x2 #x33333333) (build-fx logand (build-fx srl x2 2) #x33333333))])
                        (let ([,x4 ,(build-fx logand (build-fx + x3 (build-fx srl x3 4)) #x0f0f0f0f)])
                          ,(build-fix (build-fx logand (build-fx srl (build-fx * x4 #x01010101) 24) #x3f)))))))]))
          (define (build-popcount e)
            (constant-case popcount-instruction
              [(#t) (build-fix (%inline popcount ,e))] ; no unfix needed
              [else
               (constant-case ptr-bits
                 [(32) (build-popcount32 e)]
                 [(64)
                  (let ([x (make-tmp 'x 'uptr)]
                        [x2 (make-tmp 'x2 'uptr)]
                        [x3 (make-tmp 'x3 'uptr)]
                        [x4 (make-tmp 'x4 'uptr)]
                        [x5 (make-tmp 'x5 'uptr)])
                    `(let ([,x ,e]) ; no unfix needed
                       (let ([,x2 ,(build-fx - x (build-fx logand (build-fx srl x 1) #x5555555555555555))])
                         (let ([,x3 ,(build-fx + (build-fx logand x2 #x3333333333333333) (build-fx logand (build-fx srl x2 2) #x3333333333333333))])
                           (let ([,x4 ,(build-fx logand (build-fx + x3 (build-fx srl x3 4)) #x0f0f0f0f0f0f0f0f)])
                             (let ([,x5 ,(build-fx logand (build-fx + x4 (build-fx srl x4 8)) #x00ff00ff00ff00ff)])
                               ,(build-fix (build-fx logand (build-fx srl (build-fx * x5 #x0101010101010101) 56) #x7f))))))))])]))
          (define-inline 3 fxpopcount
            [(e)
             (bind #f (e)
               (build-popcount e))])
          (define-inline 2 fxpopcount
            [(e)
             (bind #t (e)
               `(if ,(build-and
                      (%type-check mask-fixnum type-fixnum ,e)
                      (%inline >= ,e (immediate ,0)))
                    ,(build-popcount e)
                    ,(build-libcall #t #f sexpr fxpopcount e)))])
          (define-inline 3 fxpopcount32
            [(e)
             (bind #f (e)
               (build-popcount32 e))])
          (define-inline 2 fxpopcount32
            [(e)
             (bind #t (e)
                   `(if ,(constant-case ptr-bits
                           [(32)
                            (build-and (%type-check mask-fixnum type-fixnum ,e)
                                       (%inline >= ,e (immediate ,0)))]
                           [(64)
                            (build-and (%type-check mask-fixnum type-fixnum ,e)
                                       (%inline u< ,e (immediate ,(fix #x100000000))))])
                        ,(build-popcount32 e)
                        ,(build-libcall #t #f sexpr fxpopcount32 e)))])
          (define-inline 3 fxpopcount16
            [(e)
             (bind #f (e)
               (build-popcount16 e))])
          (define-inline 2 fxpopcount16
            [(e)
             (bind #f (e)
                   `(if ,(build-and
                          (%type-check mask-fixnum type-fixnum ,e)
                          (%inline u< ,e (immediate ,(fix #x10000))))
                        ,(build-popcount16 e)
                        ,(build-libcall #t #f sexpr fxpopcount16 e)))]))))
    (let ()
      (define do-fxsll
        (lambda (e1 e2)
          (nanopass-case (L7 Expr) e2
            [(quote ,d)
             (%inline sll ,e1 (immediate ,d))]
            [else
             ; TODO: bind-uptr might be handy here and also a make-unfix
             (let ([t (make-tmp 't 'uptr)])
               `(let ([,t ,(build-unfix e2)])
                  ,(%inline sll ,e1 ,t)))])))
      (define-inline 3 fxsll
        [(e1 e2) (do-fxsll e1 e2)])
      (define-inline 3 fxarithmetic-shift-left
        [(e1 e2) (do-fxsll e1 e2)])
      (define-inline 3 fxsll/wraparound
        [(e1 e2) (do-fxsll e1 e2)]))
    (define-inline 3 fxsrl
      [(e1 e2)
       (%inline logand
          ,(nanopass-case (L7 Expr) e2
             [(quote ,d)
              (%inline srl ,e1 (immediate ,d))]
             [else
              (let ([t (make-tmp 't 'uptr)])
                `(let ([,t ,(build-unfix e2)])
                   ,(%inline srl ,e1 ,t)))])
          (immediate ,(fx- (constant fixnum-factor))))])
    (let ()
      (define do-fxsra
        (lambda (e1 e2)
          (%inline logand
             ,(nanopass-case (L7 Expr) e2
                [(quote ,d)
                 (%inline sra ,e1 (immediate ,d))]
                [else
                 (let ([t (make-tmp 't 'uptr)])
                   `(let ([,t ,(build-unfix e2)])
                      ,(%inline sra ,e1 ,t)))])
             (immediate ,(fx- (constant fixnum-factor))))))
      (define-inline 3 fxsra
        [(e1 e2) (do-fxsra e1 e2)])
      (define-inline 3 fxarithmetic-shift-right
        [(e1 e2) (do-fxsra e1 e2)]))
    (let ()
      (define-syntax %safe-shift
        (syntax-rules ()
          [(_ src sexpr op libcall e1 e2 ?size)
           (let ([size ?size])
             (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x (fx- size 1)))) e2)
                 (bind #t (e1 e2)
                   `(if ,(build-fixnums? (list e1))
                        ,(%inline logand
                          ,(%inline op ,e1 (immediate ,(constant-value e2)))
                          (immediate ,(- (constant fixnum-factor))))
                        ,(build-libcall #t src sexpr libcall e1 e2)))
                 (bind #t (e1 e2)
                   `(if ,(build-and
                           (build-fixnums? (list e1 e2))
                           (%inline u< ,e2 (immediate ,(fix size))))
                        ,(%inline logand
                          ,(%inline op ,e1 ,(build-unfix e2))
                          (immediate ,(- (constant fixnum-factor))))
                        ,(build-libcall #t src sexpr libcall e1 e2)))))]))
      (define-inline 2 fxsrl
        [(e1 e2) (%safe-shift src sexpr srl fxsrl e1 e2 (+ (constant fixnum-bits) 1))])
      (define-inline 2 fxsra
        [(e1 e2) (%safe-shift src sexpr sra fxsra e1 e2 (+ (constant fixnum-bits) 1))])
      (define-inline 2 fxarithmetic-shift-right
        [(e1 e2) (%safe-shift src sexpr sra fxarithmetic-shift-right e1 e2 (constant fixnum-bits))]))
    (define-inline 3 fxarithmetic-shift
      [(e1 e2)
       (or (nanopass-case (L7 Expr) e2
             [(quote ,d)
              (and (fixnum? d)
                   (if ($fxu< d (constant fixnum-bits))
                       (%inline sll ,e1 (immediate ,d))
                       (and (fx< (fx- (constant fixnum-bits)) d 0)
                            (%inline logand
                               ,(%inline sra ,e1 (immediate ,(fx- d)))
                               (immediate ,(- (constant fixnum-factor)))))))]
             [else #f])
           (build-libcall #f src sexpr fxarithmetic-shift e1 e2))])
    (define-inline 2 fxarithmetic-shift
      [(e1 e2)
       (or (nanopass-case (L7 Expr) e2
             [(quote ,d)
              (guard (fixnum? d) (fx< (fx- (constant fixnum-bits)) d 0))
              (bind #t (e1)
                `(if ,(build-fixnums? (list e1))
                     ,(%inline logand
                        ,(%inline sra ,e1 (immediate ,(fx- d)))
                        (immediate ,(- (constant fixnum-factor))))
                     ,(build-libcall #t src sexpr fxarithmetic-shift e1 e2)))]
             [else #f])
           (build-libcall #f src sexpr fxarithmetic-shift e1 e2))])
    (let ()
      (define dofxlogbit0
        (lambda (e1 e2)
          (if (constant? (lambda (x) (and (fixnum? x) ($fxu< x (fx- (constant fixnum-bits) 1)))) e2)
              (%inline logand ,e1
                (immediate ,(fix (lognot (ash 1 (constant-value e2))))))
              (%inline logand ,e1
                ,(%inline lognot
                   ,(%inline sll (immediate ,(fix 1))
                      ,(build-unfix e2)))))))
      (define dofxlogbit1
        (lambda (e1 e2)
          (if (constant? (lambda (x) (and (fixnum? x) ($fxu< x (fx- (constant fixnum-bits) 1)))) e2)
              (%inline logor ,e1
                 (immediate ,(fix (ash 1 (constant-value e2)))))
              (%inline logor ,e1
                 ,(%inline sll (immediate ,(fix 1))
                   ,(build-unfix e2))))))
      (define-inline 3 fxlogbit0
        [(e1 e2) (dofxlogbit0 e2 e1)])
      (define-inline 3 fxlogbit1
        [(e1 e2) (dofxlogbit1 e2 e1)])
      (define-inline 3 fxcopy-bit
        [(e1 e2 e3)
         ;; NB: even in the case where e3 is not known to be 0 or 1, seems like we could do better here.
         (and (fixnum-constant? e3)
              (case (constant-value e3)
                [(0) (dofxlogbit0 e1 e2)]
                [(1) (dofxlogbit1 e1 e2)]
                [else #f]))]))
    (let ()
      (define dofxlogbit0
        (lambda (e1 e2 libcall)
          (if (constant? (lambda (x) (and (fixnum? x) ($fxu< x (fx- (constant fixnum-bits) 1)))) e2)
              (bind #t (e1)
                `(if ,(build-fixnums? (list e1))
                     ,(%inline logand ,e1
                        (immediate ,(fix (lognot (ash 1 (constant-value e2))))))
                     ,(libcall e1 e2)))
              (bind #t (e1 e2)
                `(if ,(build-and
                        (build-fixnums? (list e1 e2))
                        (%inline u< ,e2 (immediate ,(fix (fx- (constant fixnum-bits) 1)))))
                     ,(%inline logand ,e1
                       ,(%inline lognot
                         ,(%inline sll (immediate ,(fix 1))
                           ,(build-unfix e2))))
                     ,(libcall e1 e2))))))
      (define dofxlogbit1
        (lambda (e1 e2 libcall)
          (if (constant? (lambda (x) (and (fixnum? x) ($fxu< x (fx- (constant fixnum-bits) 1)))) e2)
              (bind #t (e1)
                `(if ,(build-fixnums? (list e1))
                     ,(%inline logor ,e1
                        (immediate ,(fix (ash 1 (constant-value e2)))))
                     ,(libcall e1 e2)))
              (bind #t (e1 e2)
                `(if ,(build-and
                        (build-fixnums? (list e1 e2))
                        (%inline u< ,e2 (immediate ,(fix (fx- (constant fixnum-bits) 1)))))
                     ,(%inline logor ,e1
                        ,(%inline sll (immediate ,(fix 1))
                           ,(build-unfix e2)))
                     ,(libcall e1 e2))))))
      (define-inline 2 fxlogbit0
        [(e1 e2) (dofxlogbit0 e2 e1
                   (lambda (e2 e1)
                     (build-libcall #t src sexpr fxlogbit0 e1 e2)))])
      (define-inline 2 fxlogbit1
        [(e1 e2) (dofxlogbit1 e2 e1
                   (lambda (e2 e1)
                     (build-libcall #t src sexpr fxlogbit1 e1 e2)))])
      (define-inline 2 fxcopy-bit
        [(e1 e2 e3)
         (and (fixnum-constant? e3)
              (case (constant-value e3)
                [(0) (dofxlogbit0 e1 e2
                       (lambda (e1 e2)
                         (build-libcall #t src sexpr fxcopy-bit e1 e2)))]
                [(1) (dofxlogbit1 e1 e2
                       (lambda (e1 e2)
                         (build-libcall #t src sexpr fxcopy-bit e1 e2)))]
                [else #f]))]))
    (define-inline 3 fxzero?
      [(e) (or (relop-length RELOP= e) (%inline eq? ,e (immediate 0)))])
    (define-inline 3 fxpositive?
      [(e) (or (relop-length RELOP> e) (%inline > ,e (immediate 0)))])
    (define-inline 3 fxnonnegative?
      [(e) (or (relop-length RELOP>= e) (%inline >= ,e (immediate 0)))])
    (define-inline 3 fxnegative?
      [(e) (or (relop-length RELOP< e) (%inline < ,e (immediate 0)))])
    (define-inline 3 fxnonpositive?
      [(e) (or (relop-length RELOP<= e) (%inline <= ,e (immediate 0)))])
    (define-inline 3 fxeven?
      [(e) (%inline eq?
              ,(%inline logand ,e (immediate ,(fix 1)))
              (immediate ,(fix 0)))])
    (define-inline 3 fxodd?
      [(e) (%inline eq?
              ,(%inline logand ,e (immediate ,(fix 1)))
              (immediate ,(fix 1)))])

    (define-inline 2 fxzero?
      [(e) (or (relop-length RELOP= e)
               (bind #t (e)
                 (build-simple-or
                   (%inline eq? ,e (immediate 0))
                   `(if ,(build-fixnums? (list e))
                        ,(%constant sfalse)
                        ,(build-libcall #t src sexpr fxzero? e)))))])
    (define-inline 2 fxpositive?
      [(e) (or (relop-length RELOP> e)
               (bind #t (e)
                 `(if ,(build-fixnums? (list e))
                      ,(%inline > ,e (immediate 0))
                      ,(build-libcall #t src sexpr fxpositive? e))))])
    (define-inline 2 fxnonnegative?
      [(e) (or (relop-length RELOP>= e)
               (bind #t (e)
                 `(if ,(build-fixnums? (list e))
                      ,(%inline >= ,e (immediate 0))
                      ,(build-libcall #t src sexpr fxnonnegative? e))))])
    (define-inline 2 fxnegative?
      [(e) (or (relop-length RELOP< e)
               (bind #t (e)
                 `(if ,(build-fixnums? (list e))
                      ,(%inline < ,e (immediate 0))
                      ,(build-libcall #t src sexpr fxnegative? e))))])
    (define-inline 2 fxnonpositive?
      [(e) (or (relop-length RELOP<= e)
               (bind #t (e)
                 `(if ,(build-fixnums? (list e))
                      ,(%inline <= ,e (immediate 0))
                      ,(build-libcall #t src sexpr fxnonpositive? e))))])
    (define-inline 2 fxeven?
      [(e) (bind #t (e)
             `(if ,(build-fixnums? (list e))
                  ,(%inline eq?
                     ,(%inline logand ,e (immediate ,(fix 1)))
                     (immediate ,(fix 0)))
                  ,(build-libcall #t src sexpr fxeven? e)))])
    (define-inline 2 fxodd?
      [(e) (bind #t (e)
             `(if ,(build-fixnums? (list e))
                  ,(%inline eq?
                    ,(%inline logand ,e (immediate ,(fix 1)))
                    (immediate ,(fix 1)))
                  ,(build-libcall #t src sexpr fxodd? e)))])
    (let ()
      (define dofxlogbit?
        (lambda (e1 e2)
          (cond
            [(constant? (lambda (x) (and (fixnum? x) (fx<= 0 x (fx- (constant fixnum-bits) 2)))) e1)
             (%inline logtest ,e2 (immediate ,(fix (ash 1 (constant-value e1)))))]
            [(constant? (lambda (x) (and (target-fixnum? x) (> x (fx- (constant fixnum-bits) 2)))) e1)
             (%inline < ,e2 (immediate ,(fix 0)))]
            [(fixnum-constant? e2)
             (bind #t (e1)
               `(if ,(%inline < (immediate ,(fix (fx- (constant fixnum-bits) 2))) ,e1)
                    ,(if (< (constant-value e2) 0) (%constant strue) (%constant sfalse))
                    ,(%inline logtest
                       ,(%inline sra ,e2 ,(build-unfix e1))
                       (immediate ,(fix 1)))))]
            [else
             (bind #t (e1 e2)
               `(if ,(%inline < (immediate ,(fix (fx- (constant fixnum-bits) 2))) ,e1)
                    ,(%inline < ,e2 (immediate ,(fix 0)))
                    ,(%inline logtest
                       ,(%inline sra ,e2 ,(build-unfix e1))
                       (immediate ,(fix 1)))))])))

      (define-inline 3 fxbit-set?
        [(e1 e2) (dofxlogbit? e2 e1)])

      (define-inline 3 fxlogbit?
        [(e1 e2) (dofxlogbit? e1 e2)]))

    (let ()
      (define dofxlogbit?
        (lambda (e1 e2 libcall)
          (cond
            [(constant? (lambda (x) (and (fixnum? x) (fx<= 0 x (fx- (constant fixnum-bits) 2)))) e1)
             (bind #t (e2)
               `(if ,(build-fixnums? (list e2))
                    ,(%inline logtest ,e2
                       (immediate ,(fix (ash 1 (constant-value e1)))))
                    ,(libcall e1 e2)))]
            [(constant? (lambda (x) (and (target-fixnum? x) (> x (fx- (constant fixnum-bits) 2)))) e1)
             (bind #t (e2)
               `(if ,(build-fixnums? (list e2))
                    ,(%inline < ,e2 (immediate ,(fix 0)))
                    ,(libcall e1 e2)))]
            [else
             (bind #t (e1 e2)
               `(if ,(build-and
                       (build-fixnums? (list e1 e2))
                       (%inline u< ,e1 (immediate ,(fix (constant fixnum-bits)))))
                    ,(%inline logtest
                       ,(%inline sra ,e2 ,(build-unfix e1))
                       (immediate ,(fix 1)))
                    ,(libcall e1 e2)))])))

      (define-inline 2 fxbit-set?
        [(e1 e2) (dofxlogbit? e2 e1
                   (lambda (e2 e1)
                     (build-libcall #t src sexpr fxbit-set? e1 e2)))])
      (define-inline 2 fxlogbit?
        [(e1 e2) (dofxlogbit? e1 e2
                   (lambda (e1 e2)
                     (build-libcall #t src sexpr fxlogbit? e1 e2)))]))

    ; can avoid if in fxabs with:
    ;   t = sra(x, k)     ; where k is ptr-bits - 1
    ;                     ; t is now -1 if x's sign bit set, otherwise 0
    ;   x = xor(x, t)     ; logical not if x negative, otherwise leave x alone
    ;   x = x - t         ; add 1 to complete two's complement negation if
    ;                     ; x was negative, otherwise leave x alone
    ; tests on i3le indicate that the if is actually faster, even in a loop
    ; where input alternates between positive and negative to defeat branch
    ; prediction.
    (define-inline 3 fxabs
      [(e) (bind #t (e)
             `(if ,(%inline < ,e (immediate ,(fix 0)))
                  ,(%inline - (immediate ,(fix 0)) ,e)
                  ,e))])

    ;(define-inline 3 min ; needs library min
    ;  ; must take care to be inexactness-preserving
    ;   [(e0) e0]
    ;   [(e0 e1)
    ;    (bind #t (e0 e1)
    ;      `(if ,(build-fixnums? (list e0 e1))
    ;           (if ,(%inline < ,e0 ,e1) ,e0 ,e1)
    ;           ,(build-libcall #t src sexpr min e0 e1)))]
    ;   [(e0 . e*) (reduce src sexpr moi e1 e*)])
    ;
    ;(define-inline 3 max ; needs library max
    ;  ; must take care to be inexactness-preserving
    ;   [(e0) e0]
    ;   [(e0 e1)
    ;    (bind #t (e0 e1)
    ;      `(if ,(build-fixnums? (list e0 e1))
    ;           (if ,(%inline < ,e0 ,e1) ,e0 ,e1)
    ;           ,(build-libcall #t src sexpr max e0 e1)))]
    ;   [(e1 . e*) (reduce src sexpr moi e1 e*)])

    (define-inline 3 fxmin
      [(e) (ensure-single-valued e)]
      [(e1 e2) (bind #t (e1 e2)
                 `(if ,(%inline < ,e1 ,e2)
                      ,e1
                      ,e2))]
      [(e1 . e*) (reduce src sexpr moi e1 e*)])

    (define-inline 3 fxmax
      [(e) (ensure-single-valued e)]
      [(e1 e2) (bind #t (e1 e2)
                 `(if ,(%inline < ,e2 ,e1)
                      ,e1
                      ,e2))]
      [(e1 . e*) (reduce src sexpr moi e1 e*)])

    (define-inline 3 fxif
      [(e1 e2 e3)
       (bind #t (e1)
         (%inline logor
            ,(%inline logand ,e2 ,e1)
            ,(%inline logand ,e3
              ,(%inline lognot ,e1))))])

    (define-inline 3 fxbit-field
      [(e1 e2 e3)
       (and (constant? fixnum? e2) (constant? fixnum? e3)
            (let ([start (constant-value e2)] [end (constant-value e3)])
              (if (fx= end start)
                  (%seq ,e1 (immediate ,(fix 0)))
                  (and (and (fx>= start 0) (fx> end start) (fx< end (constant fixnum-bits)))
                       (extract-unsigned-bitfield #f start end e1)))))])

    (define-inline 3 fxcopy-bit-field
      [(e1 e2 e3 e4)
       (and (constant? fixnum? e2) (constant? fixnum? e3)
            (let ([start (constant-value e2)] [end (constant-value e3)])
              (if (fx= end start)
                  e1
                  (and (and (fx>= start 0) (fx> end start) (fx< end (constant fixnum-bits)))
                       (insert-bitfield #f start end (constant fixnum-bits) e1 e4)))))])

    ;; could be done with one mutable variable instead of two, but this seems to generate
    ;; the same code as the existing compiler
    (define-inline 3 fxlength
      [(e)
       (let ([t (make-assigned-tmp 't 'uptr)] [result (make-assigned-tmp 'result)])
         `(let ([,t ,(build-unfix e)])
            (seq
              (if ,(%inline < ,t (immediate 0))
                  (set! ,t ,(%inline lognot ,t))
                  ,(%constant svoid))
              (let ([,result (immediate ,(fix 0))])
                ,((lambda (body)
                    (constant-case fixnum-bits
                      [(30) body]
                      [(61)
                       `(seq
                          (if ,(%inline < ,t (immediate  #x100000000))
                              ,(%constant svoid)
                              (seq
                                (set! ,t ,(%inline srl ,t (immediate 32)))
                                (set! ,result
                                  ,(%inline + ,result (immediate ,(fix 32))))))
                          ,body)]))
                  (%seq
                    (if ,(%inline < ,t (immediate #x10000))
                        ,(%constant svoid)
                        (seq
                          (set! ,t ,(%inline srl ,t (immediate 16)))
                          (set! ,result ,(%inline + ,result (immediate ,(fix 16))))))
                    (if ,(%inline < ,t (immediate #x100))
                        ,(%constant svoid)
                        (seq
                          (set! ,t ,(%inline srl ,t (immediate 8)))
                          (set! ,result ,(%inline + ,result (immediate ,(fix 8))))))
                    ,(%inline + ,result
                      (inline ,(make-info-load 'unsigned-8 #f) ,%load
                        ,(%tc-ref fxlength-bv) ,t
                        ,(%constant bytevector-data-disp)))))))))])

    (define-inline 3 fxfirst-bit-set
      [(e)
       (let ([t (make-assigned-tmp 't 'uptr)] [result (make-assigned-tmp 'result)])
         (bind #t (e)
           `(if ,(%inline eq? ,e (immediate ,(fix 0)))
                (immediate ,(fix -1))
                (let ([,t ,(build-unfix e)] [,result (immediate ,(fix 0))])
                  ,((lambda (body)
                      (constant-case fixnum-bits
                        [(30) body]
                        [(61)
                         `(seq
                            (if ,(%inline logtest ,t (immediate #xffffffff))
                                ,(%constant svoid)
                                (seq
                                  (set! ,t ,(%inline srl ,t (immediate 32)))
                                  (set! ,result ,(%inline + ,result (immediate ,(fix 32))))))
                            ,body)]))
                    (%seq
                      (if ,(%inline logtest ,t (immediate #xffff))
                          ,(%constant svoid)
                          (seq
                            (set! ,t ,(%inline srl ,t (immediate 16)))
                            (set! ,result ,(%inline + ,result (immediate ,(fix 16))))))
                      (if ,(%inline logtest ,t (immediate #xff))
                          ,(%constant svoid)
                          (seq
                            (set! ,t ,(%inline srl ,t (immediate 8)))
                            (set! ,result ,(%inline + ,result (immediate ,(fix 8))))))
                      ,(%inline + ,result
                        (inline ,(make-info-load 'unsigned-8 #f) ,%load
                          ,(%tc-ref fxfirst-bit-set-bv)
                          ,(%inline logand ,t (immediate #xff))
                          ,(%constant bytevector-data-disp)))))))))])

    (let ()
      (define-syntax type-pred
        (syntax-rules ()
          [(_ name? mask type)
           (define-inline 2 name?
             [(e) (%type-check mask type ,e)])]))
      (define-syntax typed-object-pred
        (syntax-rules ()
          [(_ name? mask type)
           (define-inline 2 name?
             [(e)
              (bind #t (e)
                (%typed-object-check mask type ,e))])]))
      (type-pred boolean? mask-boolean type-boolean)
      (type-pred bwp-object? mask-bwp sbwp)
      (type-pred char? mask-char type-char)
      (type-pred eof-object? mask-eof seof)
      (type-pred fixnum? mask-fixnum type-fixnum)
      (type-pred flonum? mask-flonum type-flonum)
      (type-pred null? mask-nil snil)
      (type-pred pair? mask-pair type-pair)
      (type-pred procedure? mask-closure type-closure)
      (type-pred symbol? mask-symbol type-symbol)
      (type-pred $unbound-object? mask-unbound sunbound)
      (typed-object-pred bignum? mask-bignum type-bignum)
      (typed-object-pred box? mask-box type-box)
      (typed-object-pred mutable-box? mask-mutable-box type-mutable-box)
      (typed-object-pred immutable-box? mask-mutable-box type-immutable-box)
      (typed-object-pred bytevector? mask-bytevector type-bytevector)
      (typed-object-pred mutable-bytevector? mask-mutable-bytevector type-mutable-bytevector)
      (typed-object-pred immutable-bytevector? mask-mutable-bytevector type-immutable-bytevector)
      (typed-object-pred $code? mask-code type-code)
      (typed-object-pred $exactnum? mask-exactnum type-exactnum)
      (typed-object-pred fxvector? mask-fxvector type-fxvector)
      (typed-object-pred flvector? mask-flvector type-flvector)
      (typed-object-pred $inexactnum? mask-inexactnum type-inexactnum)
      (typed-object-pred $rtd-counts? mask-rtd-counts type-rtd-counts)
      (typed-object-pred phantom-bytevector? mask-phantom type-phantom)
      (typed-object-pred input-port? mask-input-port type-input-port)
      (typed-object-pred output-port? mask-output-port type-output-port)
      (typed-object-pred port? mask-port type-port)
      (typed-object-pred ratnum? mask-ratnum type-ratnum)
      (typed-object-pred $record? mask-record type-record)
      (typed-object-pred string? mask-string type-string)
      (typed-object-pred mutable-string? mask-mutable-string type-mutable-string)
      (typed-object-pred immutable-string? mask-mutable-string type-immutable-string)
      (typed-object-pred $system-code? mask-system-code type-system-code)
      (typed-object-pred $tlc? mask-tlc type-tlc)
      (typed-object-pred vector? mask-vector type-vector)
      (typed-object-pred mutable-vector? mask-mutable-vector type-mutable-vector)
      (typed-object-pred immutable-vector? mask-mutable-vector type-immutable-vector)
      (typed-object-pred stencil-vector? mask-stencil-vector type-stencil-vector)
      (typed-object-pred $stencil-vector? mask-any-stencil-vector type-any-stencil-vector)
      (typed-object-pred $system-stencil-vector? mask-sys-stencil-vector type-sys-stencil-vector)
      (typed-object-pred thread? mask-thread type-thread))
    (define-inline 3 $bigpositive?
      [(e) (%type-check mask-signed-bignum type-positive-bignum
             ,(%mref ,e ,(constant bignum-type-disp)))])
    (define-inline 3 csv7:record-field-accessible?
      [(e1 e2) (%seq ,e1 ,e2 ,(%constant strue))])

    (define-inline 2 cflonum?
      [(e) (bind #t (e)
             `(if ,(%type-check mask-flonum type-flonum ,e)
                  ,(%constant strue)
                  ,(%typed-object-check mask-inexactnum type-inexactnum ,e)))])
    (define-inline 2 $immediate?
      [(e) (bind #t (e) (%type-check mask-immediate type-immediate ,e))])
    (define-inline 3 $fixmediate
      [(e) e])

    (define-inline 3 $inexactnum-real-part
      [(e) (build-$inexactnum-real-part e)])
    (define-inline 3 $inexactnum-imag-part
      [(e) (build-$inexactnum-imag-part e)])

    (define-inline 3 cfl-real-part
      [(e) (bind #t (e)
             `(if ,(%type-check mask-flonum type-flonum ,e)
                  ,e
                  ,(build-$inexactnum-real-part e)))])

    (define-inline 3 cfl-imag-part
      [(e) (bind #t (e)
             `(if ,(%type-check mask-flonum type-flonum ,e)
                  (quote 0.0)
                  ,(build-$inexactnum-imag-part e)))])

    (define-inline 3 $closure-ref
      [(e-v e-i)
       (nanopass-case (L7 Expr) e-i
         [(quote ,d)
          (guard (target-fixnum? d))
          (%mref ,e-v ,(+ (fix d) (constant closure-data-disp)))]
         [else (%mref ,e-v ,e-i ,(constant closure-data-disp))])])
    (define-inline 3 $closure-set!
      [(e-v e-i e-new)
       (nanopass-case (L7 Expr) e-i
         [(quote ,d)
          (guard (target-fixnum? d))
          (build-dirty-store e-v (+ (fix d) (constant closure-data-disp)) e-new)]
         [else (build-dirty-store e-v e-i (constant closure-data-disp) e-new)])])
    (define-inline 3 $closure-code
      [(e) (%inline -
              ,(%mref ,e ,(constant closure-code-disp))
              ,(%constant code-data-disp))])
    (define-inline 3 $code-free-count
      [(e) (build-fix (%mref ,e ,(constant code-closure-length-disp)))])
    (define-inline 3 $code-mutable-closure?
      [(e) (%typed-object-check mask-code-mutable-closure type-code-mutable-closure ,e)])
    (define-inline 3 $code-arity-in-closure?
      [(e) (%typed-object-check mask-code-arity-in-closure type-code-arity-in-closure ,e)])
    (define-inline 3 $code-single-valued?
      [(e) (%typed-object-check mask-code-single-valued type-code-single-valued ,e)])
    (define-inline 2 $unbound-object
      [() `(quote ,($unbound-object))])
    (define-inline 2 void
      [() `(quote ,(void))])
    (define-inline 2 eof-object
      [() `(quote #!eof)])
    (define-inline 2 cons
      [(e1 e2)
       (bind #f (e1 e2)
         (bind #t ([t (%constant-alloc type-pair (constant size-pair))])
           (%seq
             (set! ,(%mref ,t ,(constant pair-car-disp)) ,e1)
             (set! ,(%mref ,t ,(constant pair-cdr-disp)) ,e2)
             ,t)))])
    (define-inline 2 box
      [(e)
       (bind #f (e)
         (bind #t ([t (%constant-alloc type-typed-object (constant size-box))])
           (%seq
             (set! ,(%mref ,t ,(constant box-type-disp)) ,(%constant type-box))
             (set! ,(%mref ,t ,(constant box-ref-disp)) ,e)
             ,t)))])
    (define-inline 2 box-immutable
      [(e)
       (bind #f (e)
         (bind #t ([t (%constant-alloc type-typed-object (constant size-box))])
           (%seq
             (set! ,(%mref ,t ,(constant box-type-disp)) ,(%constant type-immutable-box))
             (set! ,(%mref ,t ,(constant box-ref-disp)) ,e)
             ,t)))])
    (define-inline 3 $make-tlc
      [(e-ht e-keyval e-next)
       (bind #f (e-ht e-keyval e-next)
         (bind #t ([t (%constant-alloc type-typed-object (constant size-tlc))])
           (%seq
             (set! ,(%mref ,t ,(constant tlc-type-disp)) ,(%constant type-tlc))
             (set! ,(%mref ,t ,(constant tlc-ht-disp)) ,e-ht)
             (set! ,(%mref ,t ,(constant tlc-keyval-disp)) ,e-keyval)
             (set! ,(%mref ,t ,(constant tlc-next-disp)) ,e-next)
             ,t)))])
    (define-inline 2 list
      [e* (build-list e*)])
    (let ()
      (define (go e e*)
        (bind #f (e)
          (list-bind #f (e*)
            (bind #t ([t (%constant-alloc type-pair (fx* (constant size-pair) (length e*)))])
              (let loop ([e e] [e* e*] [i 0])
                (let ([e2 (car e*)] [e* (cdr e*)])
                  `(seq
                     (set! ,(%mref ,t ,(fx+ i (constant pair-car-disp))) ,e)
                     ,(if (null? e*)
                          `(seq
                             (set! ,(%mref ,t ,(fx+ i (constant pair-cdr-disp))) ,e2)
                             ,t)
                          (let ([next-i (fx+ i (constant size-pair))])
                            `(seq
                               (set! ,(%mref ,t ,(fx+ i (constant pair-cdr-disp)))
                                 ,(%inline + ,t (immediate ,next-i)))
                               ,(loop e2 e* next-i)))))))))))
      (define-inline 2 list*
        [(e) (ensure-single-valued e)]
        [(e . e*) (go e e*)])
      (define-inline 2 cons*
        [(e) (ensure-single-valued e)]
        [(e . e*) (go e e*)]))
    (define-inline 2 vector
      [() `(quote #())]
      [e*
       (let ([n (length e*)])
         (list-bind #f (e*)
           (bind #t ([t (%constant-alloc type-typed-object
                          (fx+ (constant header-size-vector) (fx* n (constant ptr-bytes))))])
             (let loop ([e* e*] [i 0])
               (if (null? e*)
                   `(seq
                      (set! ,(%mref ,t ,(constant vector-type-disp))
                        (immediate ,(+ (fx* n (constant vector-length-factor))
                                       (constant type-vector))))
                      ,t)
                   `(seq
                      (set! ,(%mref ,t ,(fx+ i (constant vector-data-disp))) ,(car e*))
                      ,(loop (cdr e*) (fx+ i (constant ptr-bytes)))))))))])
    (let ()
      (define (go e*)
        (let ([n (length e*)])
          (list-bind #f (e*)
            (bind #t ([t (%constant-alloc type-typed-object
                           (fx+ (constant header-size-fxvector) (fx* n (constant ptr-bytes))))])
              (let loop ([e* e*] [i 0])
                (if (null? e*)
                    `(seq
                       (set! ,(%mref ,t ,(constant fxvector-type-disp))
                         (immediate ,(+ (fx* n (constant fxvector-length-factor))
                                        (constant type-fxvector))))
                       ,t)
                    `(seq
                       (set! ,(%mref ,t ,(fx+ i (constant fxvector-data-disp))) ,(car e*))
                       ,(loop (cdr e*) (fx+ i (constant ptr-bytes))))))))))
      (define-inline 2 fxvector
        [() `(quote #vfx())]
        [e* (and (andmap (lambda (x) (constant? target-fixnum? x)) e*) (go e*))])
      (define-inline 3 fxvector
        [() `(quote #vfx())]
        [e* (go e*)]))
    (let ()
      (define (go e*)
        (let ([n (length e*)])
          (list-bind #f (e*)
            (bind #t ([t (%constant-alloc type-typed-object
                           (fx+ (constant header-size-flvector) (fx* n (constant flonum-bytes))))])
              (let loop ([e* e*] [i 0])
                (if (null? e*)
                    `(seq
                       (set! ,(%mref ,t ,(constant flvector-type-disp))
                         (immediate ,(+ (fx* n (constant flvector-length-factor))
                                        (constant type-flvector))))
                       ,t)
                    `(seq
                       (set! ,(%mref ,t ,%zero ,(fx+ i (constant flvector-data-disp)) fp) ,(car e*))
                       ,(loop (cdr e*) (fx+ i (constant flonum-bytes))))))))))
      (define-inline 2 flvector
        [() `(quote #vfl())]
        [e* (and (andmap (lambda (x) (constant? flonum? x)) e*) (go e*))])
      (define-inline 3 flvector
        [() `(quote #vfl())]
        [e* (go e*)]))
    (let ()
      (define (go e*)
        (let ([n (length e*)])
          (list-bind #f (e*)
            (bind #t ([t (%constant-alloc type-typed-object
                           (fx+ (constant header-size-string) (fx* n (constant string-char-bytes))))])
              (let loop ([e* e*] [i 0])
                (if (null? e*)
                    `(seq
                       (set! ,(%mref ,t ,(constant string-type-disp))
                         (immediate ,(+ (fx* n (constant string-length-factor))
                                           (constant type-string))))
                       ,t)
                    `(seq
                       (inline ,(make-info-load (string-char-type) #f) ,%store ,t ,%zero
                         (immediate ,(fx+ i (constant string-data-disp)))
                         ,(car e*))
                       ,(loop (cdr e*) (fx+ i (constant string-char-bytes))))))))))
      (define-inline 2 string
        [() `(quote "")]
        [e* (and (andmap (lambda (x) (constant? char? x)) e*) (go e*))])
      (define-inline 3 string
        [() `(quote "")]
        [e* (go e*)]))
    (let () ; level 2 car, cdr, caar, etc.
      (define-syntax def-c..r*
        (lambda (x)
          (define (go ad*)
            (let ([id (datum->syntax #'* (string->symbol (format "c~{~a~}r" ad*)))])
               #`(define-inline 2 #,id
                   [(e) (let ([Lerr (make-local-label 'Lerr)])
                            #,(let f ([ad* ad*])
                                (let ([builder (if (char=? (car ad*) #\a) #'build-car #'build-cdr)]
                                      [ad* (cdr ad*)])
                                  (if (null? ad*)
                                      #`(bind #t (e)
                                          `(if ,(build-pair? e)
                                               ,(#,builder e)
                                               (label ,Lerr ,(build-libcall #t src sexpr #,id e))))
                                      #`(bind #t ([t #,(f ad*)])
                                          `(if ,(build-pair? t)
                                               ,(#,builder t)
                                               (goto ,Lerr)))))))])))
          (let f ([n 4] [ad* '()])
            (let ([f (lambda (ad*)
                       (let ([defn (go ad*)])
                         (if (fx= n 1)
                             defn
                             #`(begin #,defn #,(f (fx- n 1) ad*)))))])
              #`(begin
                  #,(f (cons #\a ad*))
                  #,(f (cons #\d ad*)))))))
      def-c..r*)
    (let () ; level 3 car, cdr, caar, etc.
      (define-syntax def-c..r*
        (lambda (x)
          (define (go ad*)
            (let ([id (datum->syntax #'* (string->symbol (format "c~{~a~}r" ad*)))])
               #`(define-inline 3 #,id
                   [(e) #,(let f ([ad* ad*])
                            (let ([builder (if (char=? (car ad*) #\a) #'build-car #'build-cdr)]
                                  [ad* (cdr ad*)])
                              (if (null? ad*)
                                  #`(#,builder e)
                                  #`(#,builder #,(f ad*)))))])))
          (let f ([n 4] [ad* '()])
            (let ([f (lambda (ad*)
                       (let ([defn (go ad*)])
                         (if (fx= n 1)
                             defn
                             #`(begin #,defn #,(f (fx- n 1) ad*)))))])
              #`(begin
                  #,(f (cons #\a ad*))
                  #,(f (cons #\d ad*)))))))
      def-c..r*)
    (let () ; level 3 simple accessors, e.g., unbox, vector-length
      (define-syntax inline-accessor
        (syntax-rules ()
          [(_ prim disp)
           (define-inline 3 prim
             [(e) (%mref ,e ,(constant disp))])]))
      (inline-accessor unbox box-ref-disp)
      (inline-accessor $symbol-name symbol-name-disp)
      (inline-accessor $symbol-property-list symbol-plist-disp)
      (inline-accessor $system-property-list symbol-splist-disp)
      (inline-accessor $symbol-hash symbol-hash-disp)
      (inline-accessor $ratio-numerator ratnum-numerator-disp)
      (inline-accessor $ratio-denominator ratnum-denominator-disp)
      (inline-accessor $exactnum-real-part exactnum-real-disp)
      (inline-accessor $exactnum-imag-part exactnum-imag-disp)
      (inline-accessor binary-port-input-buffer port-ibuffer-disp)
      (inline-accessor textual-port-input-buffer port-ibuffer-disp)
      (inline-accessor binary-port-output-buffer port-obuffer-disp)
      (inline-accessor textual-port-output-buffer port-obuffer-disp)
      (inline-accessor $code-name code-name-disp)
      (inline-accessor $code-arity-mask code-arity-mask-disp)
      (inline-accessor $code-info code-info-disp)
      (inline-accessor $code-pinfo* code-pinfo*-disp)
      (inline-accessor $continuation-link continuation-link-disp)
      (inline-accessor $continuation-winders continuation-winders-disp)
      (inline-accessor $continuation-attachments continuation-attachments-disp)
      (inline-accessor csv7:record-type-descriptor record-type-disp)
      (inline-accessor $record-type-descriptor record-type-disp)
      (inline-accessor record-rtd record-type-disp)
      (inline-accessor record-type-uid record-type-uid-disp)
      (inline-accessor $port-handler port-handler-disp)
      (inline-accessor $port-info port-info-disp)
      (inline-accessor port-name port-name-disp)
      (inline-accessor $thread-tc thread-tc-disp)
      )
    (constant-case architecture
      [(pb)
       ;; Don't try to inline seginfo access, because the C pointer size used
       ;; in the table may not match the 64-bit `ptr` size
       (void)]
      [else
       (let ()
         (define (build-seginfo maybe? object? e)
           (let ([ptr (make-tmp 'ptr)]
                 [seginfo (make-tmp 'seginfo)])
             (define (build-level-3 seginfo k)
               (constant-case segment-table-levels
                 [(3)
                  (let ([s3 (make-assigned-tmp 's3)])
                    `(let ([,s3 ,(%mref ,seginfo
                                        ,(%inline sll ,(%inline srl ,ptr (immediate ,(+ (constant segment-t1-bits)
                                                                                        (constant segment-t2-bits))))
                                                  (immediate ,(constant log2-ptr-bytes)))
                                        ,0)])
                       ,(if maybe?
                            `(if ,(%inline eq? ,s3 (immediate 0))
                                 (immediate 0)
                                 ,(k s3))
                            (k s3))))]
                 [else (k seginfo)]))
             (define (build-level-2 s3 k)
               (constant-case segment-table-levels
                 [(2 3)
                  (let ([s2 (make-tmp 's2)])
                    `(let ([,s2 ,(%mref ,s3 ,(%inline logand
                                                      ,(%inline srl ,ptr (immediate ,(fx- (constant segment-t1-bits)
                                                                                          (constant log2-ptr-bytes))))
                                                      (immediate ,(fxsll (fx- (fxsll 1 (constant segment-t2-bits)) 1)
                                                                         (constant log2-ptr-bytes))))
                                        0)])
                       ,(if maybe?
                            `(if ,(%inline eq? ,s2 (immediate 0))
                                 (immediate 0)
                                 ,(k s2))
                            (k s2))))]
                 [else (k s3)]))
             `(let ([,ptr ,(%inline srl ,(if object?
                                             (%inline + ,e (immediate ,(fx- (constant typemod) 1)))
                                             e)
                                    (immediate ,(constant segment-offset-bits)))])
                (let ([,seginfo (literal ,(make-info-literal #f 'entry (lookup-c-entry segment-info) 0))])
                  ,(build-level-3 seginfo
                     (lambda (s3)
                       (build-level-2 s3
                         (lambda (s2)
                           (%mref ,s2 ,(%inline sll ,(%inline logand ,ptr
                                                              (immediate ,(fx- (fxsll 1 (constant segment-t1-bits)) 1)))
                                                (immediate ,(constant log2-ptr-bytes)))
                                  0)))))))))
         (define (build-space-test e space)
           `(if ,(%type-check mask-fixnum type-fixnum ,e)
                ,(%constant sfalse)
                (if ,(%type-check mask-immediate type-immediate ,e)
                    ,(%constant sfalse)
                    ,(let ([s-e (build-seginfo #t #t e)]
                           [si (make-tmp 'si)])
                       `(let ([,si ,s-e])
                          (if ,(%inline eq? ,si (immediate 0))
                              ,(%constant sfalse)
                              ,(let ([s `(inline ,(make-info-load 'unsigned-8 #f) ,%load ,si ,%zero (immediate 0))])
                                 (%inline eq? (immediate ,space) ,s))))))))
   
         (define-inline 2 $maybe-seginfo
           [(e)
            (bind #t (e)
              `(if ,(%type-check mask-fixnum type-fixnum ,e)
                   ,(%constant sfalse)
                   (if ,(%type-check mask-immediate type-immediate ,e)
                       ,(%constant sfalse)
                       ,(let ([s-e (build-seginfo #t #t e)]
                              [si (make-tmp 'si)])
                          `(let ([,si ,s-e])
                             (if ,(%inline eq? ,si (immediate 0))
                                 ,(%constant sfalse)
                                 ,si))))))])
         (define-inline 2 $seginfo
           [(e)
            (bind #t (e) (build-seginfo #f #t e))])
         (define-inline 2 $seginfo-generation
           [(e)
            (bind #f (e) (build-object-ref #f 'unsigned-8 e %zero (constant seginfo-generation-disp)))])
         (define-inline 2 $seginfo-space
           [(e)
            (bind #f (e)
                  (build-object-ref #f 'unsigned-8 e %zero (constant seginfo-space-disp)))])
         (define-inline 2 $list-bits-ref
           [(e)
            (bind #t (e)
                  (let ([si (make-tmp 'si)]
                        [list-bits (make-tmp 'list-bits)]
                        [offset (make-tmp 'offset)]
                        [byte (make-tmp 'byte)])
                    `(let ([,si ,(build-seginfo #f #t e)])
                       (let ([,list-bits ,(%mref ,si ,(constant seginfo-list-bits-disp))])
                         (if ,(%inline eq? ,list-bits (immediate 0))
                             (immediate 0)
                             (let ([,offset ,(%inline srl ,(%inline logand ,(%inline + ,e (immediate ,(fx- (constant typemod) 1)))
                                                                    (immediate ,(fx- (constant bytes-per-segment) 1)))
                                                      (immediate ,(constant log2-ptr-bytes)))])
                               (let ([,byte (inline ,(make-info-load 'unsigned-8 #f) ,%load ,list-bits ,%zero ,(%inline srl ,offset (immediate 3)))])
                                 ,(build-fix (%inline logand ,(%inline srl ,byte ,(%inline logand ,offset (immediate 7)))
                                                      (immediate ,(constant list-bits-mask)))))))))))])
         (define-inline 2 $generation
           [(e)
            (bind #t (e)
              `(if ,(%type-check mask-fixnum type-fixnum ,e)
                   ,(%constant sfalse)
                   ,(let ([s-e (build-seginfo #t #t e)]
                          [si (make-tmp 'si)])
                      `(let ([,si ,s-e])
                         (if ,(%inline eq? ,si (immediate 0))
                             ,(%constant sfalse)
                             ,(build-object-ref #f 'unsigned-8 si %zero 1))))))])
         (define-inline 2 weak-pair?
           [(e) (bind #t (e) (build-space-test e (constant space-weakpair)))])
         (define-inline 2 ephemeron-pair?
           [(e) (bind #t (e) (build-space-test e (constant space-ephemeron)))])
         (define-inline 2 reference-bytevector?
           [(e) (bind #t (e) (build-space-test e (constant space-reference-array)))]))])

    (define-inline 2 unbox
      [(e)
       (bind #t (e)
         `(if ,(%typed-object-check mask-box type-box ,e)
              ,(%mref ,e ,(constant box-ref-disp))
              ,(build-libcall #t src sexpr unbox e)))])
    (let ()
      (define-syntax def-len
        (syntax-rules ()
          [(_ prim type-disp length-offset)
           (define-inline 3 prim
             [(e) (extract-length (%mref ,e ,(constant type-disp)) (constant length-offset))])]))
      (def-len vector-length vector-type-disp vector-length-offset)
      (def-len fxvector-length fxvector-type-disp fxvector-length-offset)
      (def-len flvector-length flvector-type-disp flvector-length-offset)
      (def-len string-length string-type-disp string-length-offset)
      (def-len bytevector-length bytevector-type-disp bytevector-length-offset)
      (def-len $bignum-length bignum-type-disp bignum-length-offset)
      (def-len stencil-vector-mask stencil-vector-type-disp stencil-vector-mask-offset)
      (def-len $stencil-vector-mask stencil-vector-type-disp stencil-vector-mask-offset))
    (let ()
      (define-syntax def-len
        (syntax-rules ()
          [(_ prim mask type type-disp length-offset)
           (define-inline 2 prim
             [(e) (let ([Lerr (make-local-label 'Lerr)])
                    (bind #t (e)
                      `(if ,(%type-check mask-typed-object type-typed-object ,e)
                           ,(bind #t ([t/l (%mref ,e ,(constant type-disp))])
                              `(if ,(%type-check mask type ,t/l)
                                   ,(extract-length t/l (constant length-offset))
                                   (goto ,Lerr)))
                           (label ,Lerr ,(build-libcall #t #f sexpr prim e)))))])]))
      (def-len vector-length mask-vector type-vector vector-type-disp vector-length-offset)
      (def-len fxvector-length mask-fxvector type-fxvector fxvector-type-disp fxvector-length-offset)
      (def-len flvector-length mask-flvector type-flvector flvector-type-disp flvector-length-offset)
      (def-len string-length mask-string type-string string-type-disp string-length-offset)
      (def-len bytevector-length mask-bytevector type-bytevector bytevector-type-disp bytevector-length-offset)
      (def-len stencil-vector-mask mask-stencil-vector type-stencil-vector stencil-vector-type-disp stencil-vector-mask-offset)
      (def-len $stencil-vector-mask mask-any-stencil-vector type-any-stencil-vector stencil-vector-type-disp stencil-vector-mask-offset))
    ; TODO: consider adding integer-valued?, rational?, rational-valued?,
    ; real?, and real-valued?
    (define-inline 2 integer?
      [(e) (bind #t (e)
             (build-simple-or
               (%type-check mask-fixnum type-fixnum ,e)
               (build-simple-or
                 (%typed-object-check mask-bignum type-bignum ,e)
                 (build-and
                   (%type-check mask-flonum type-flonum ,e)
                   `(call ,(make-info-call src sexpr #f #f #f) #f ,(lookup-primref 3 'flinteger?) ,e)))))])
    (let ()
      (define build-number?
        (lambda (e)
          (bind #t (e)
            (build-simple-or
              (%type-check mask-fixnum type-fixnum ,e)
              (build-simple-or
                (%type-check mask-flonum type-flonum ,e)
                (build-and
                  (%type-check mask-typed-object type-typed-object ,e)
                  (%type-check mask-other-number type-other-number
                    ,(%mref ,e ,(constant bignum-type-disp)))))))))
      (define-inline 2 number?
        [(e) (build-number? e)])
      (define-inline 2 complex?
        [(e) (build-number? e)]))
    (define-inline 3 set-car!
      [(e1 e2) (build-dirty-store e1 (constant pair-car-disp) e2)])
    (define-inline 3 set-cdr!
      [(e1 e2) (build-dirty-store e1 (constant pair-cdr-disp) e2)])
    (define-inline 3 set-box!
      [(e1 e2) (build-dirty-store e1 (constant box-ref-disp) e2)])
    (define-inline 3 box-cas!
      [(e1 e2 e3)
       (bind #t (e2)
         (build-dirty-store e1 %zero (constant box-ref-disp) e3 (make-build-cas e2) build-cas-seq))])
    (define-inline 3 $set-symbol-name!
      [(e1 e2) (build-dirty-store e1 (constant symbol-name-disp) e2)])
    (define-inline 3 $set-symbol-property-list!
      [(e1 e2) (build-dirty-store e1 (constant symbol-plist-disp) e2)])
    (define-inline 3 $set-system-property-list!
      [(e1 e2) (build-dirty-store e1 (constant symbol-splist-disp) e2)])
    (define-inline 3 $set-port-info!
      [(e1 e2) (build-dirty-store e1 (constant port-info-disp) e2)])
    (define-inline 3 set-port-name!
      [(e1 e2) (build-dirty-store e1 (constant port-name-disp) e2)])
    (define-inline 2 set-box!
      [(e-box e-new)
       (bind #t (e-box)
         (dirty-store-bind #t (e-new)
           `(if ,(%typed-object-check mask-mutable-box type-mutable-box ,e-box)
                ,(build-dirty-store e-box (constant box-ref-disp) e-new)
                ,(build-libcall #t src sexpr set-box! e-box e-new))))])
    (define-inline 2 box-cas!
      [(e-box e-old e-new)
       (bind #t (e-box e-old)
         (dirty-store-bind #t (e-new)
           `(if ,(%typed-object-check mask-mutable-box type-mutable-box ,e-box)
                ,(build-dirty-store e-box %zero (constant box-ref-disp) e-new (make-build-cas e-old) build-cas-seq)
                ,(build-libcall #t src sexpr box-cas! e-box e-old e-new))))])
    (define-inline 2 set-car!
      [(e-pair e-new)
       (bind #t (e-pair)
         (dirty-store-bind #t (e-new)
           `(if ,(%type-check mask-pair type-pair ,e-pair)
                ,(build-dirty-store e-pair (constant pair-car-disp) e-new)
                ,(build-libcall #t src sexpr set-car! e-pair e-new))))])
    (define-inline 2 set-cdr!
      [(e-pair e-new)
       (bind #t (e-pair)
         (dirty-store-bind #t (e-new)
           `(if ,(%type-check mask-pair type-pair ,e-pair)
                ,(build-dirty-store e-pair (constant pair-cdr-disp) e-new)
                ,(build-libcall #t src sexpr set-cdr! e-pair e-new))))])
    (define-inline 3 $set-symbol-hash!
      ; no need for dirty store---e2 should be a fixnum
      [(e1 e2) `(set! ,(%mref ,e1 ,(constant symbol-hash-disp)) ,e2)])
    (define-inline 2 memory-order-acquire
      [() (if-feature pthreads
            (constant-case architecture
	          [(arm32 arm64 riscv64 loongarch64 pb) (%seq ,(%inline acquire-fence) (quote ,(void)))]
              [else `(quote ,(void))])
            `(quote ,(void)))])
    (define-inline 2 memory-order-release
      [() (if-feature pthreads
            (constant-case architecture
	          [(arm32 arm64 riscv64 loongarch64 pb) (%seq ,(%inline release-fence) (quote ,(void)))]
              [else `(quote ,(void))])
            `(quote ,(void)))])
    (let ()
      (define-syntax define-tlc-parameter
        (syntax-rules ()
          [(_ name disp)
           (define-inline 3 name
             [(e-x) (%mref ,e-x ,(constant disp))])]
          [(_ name name! disp)
           (begin
             (define-tlc-parameter name disp)
             (define-inline 3 name!
               [(e-x e-new) (build-dirty-store e-x (constant disp) e-new)]))]))
      (define-tlc-parameter $tlc-keyval tlc-keyval-disp)
      (define-tlc-parameter $tlc-ht tlc-ht-disp)
      (define-tlc-parameter $tlc-next $set-tlc-next! tlc-next-disp))
    (define-inline 2 $top-level-value
      [(e) (nanopass-case (L7 Expr) e
             [(quote ,d)
              (guard (symbol? d))
              (if (any-set? (prim-mask (or primitive system)) ($sgetprop d '*flags* 0))
                  (Symref d)
                  (bind #t (e)
                    (bind #t ([t (%mref ,e ,(constant symbol-value-disp))])
                      `(if ,(%type-check mask-unbound sunbound ,t)
                           ,(build-libcall #t #f sexpr $top-level-value e)
                           ,t))))]
             [else
              (bind #t (e)
                (let ([Lfail (make-local-label 'tlv-fail)])
                  `(if ,(%type-check mask-symbol type-symbol ,e)
                       ,(bind #t ([t (%mref ,e ,(constant symbol-value-disp))])
                          `(if ,(%type-check mask-unbound sunbound ,t)
                               (goto ,Lfail)
                               ,t))
                       (label ,Lfail ,(build-libcall #t #f sexpr $top-level-value e)))))])])
    (define-inline 3 $top-level-value
      [(e) (nanopass-case (L7 Expr) e
             [(quote ,d) (guard (symbol? d)) (Symref d)]
             [else (%mref ,e ,(constant symbol-value-disp))])])
    (let ()
      (define (go e-sym e-value)
        (bind #t (e-sym)
          `(seq
             ,(build-dirty-store e-sym (constant symbol-value-disp) e-value)
             (set! ,(%mref ,e-sym ,(constant symbol-pvalue-disp))
               (literal
                 ,(make-info-literal #f 'library
                    (lookup-libspec nonprocedure-code)
                    (constant code-data-disp)))))))
      (define-inline 3 $set-top-level-value!
        [(e-sym e-value) (go e-sym e-value)])
      (define-inline 2 $set-top-level-value!
        [(e-sym e-value) (and (constant? symbol? e-sym) (go e-sym e-value))]))
    (define-inline 3 $top-level-bound?
      [(e-sym)
       (build-not
         (%type-check mask-unbound sunbound
           ,(nanopass-case (L7 Expr) e-sym
              [(quote ,d) (guard (symbol? d)) (Symref d)]
              [else (%mref ,e-sym ,(constant symbol-value-disp))])))])
    (let ()
      (define parse-format
        (lambda (who src cntl-arg args)
          (nanopass-case (L7 Expr) cntl-arg
            [(quote ,d)
             (guard (c [(and (assertion-violation? c)
                             (format-condition? c)
                             (message-condition? c)
                             (irritants-condition? c))
                        ($source-warning 'compile
                          src #t
                          "~? in call to ~s"
                          (condition-message c)
                          (condition-irritants c)
                          who)
                        #f])
               (#%$parse-format-string who d (length args)))]
            [else #f])))
      (define fmt->expr
        ($make-fmt->expr
          (lambda (d) `(quote ,d))
          (lambda (e1 e2) `(seq ,e1 ,e2))
          (lambda (src sexpr prim arg*)
            `(call ,(make-info-call src sexpr #f #f #f) #f
               ,(lookup-primref 3 prim)
               ,arg* ...))))
      (define build-format
        (lambda (who src sexpr op-arg cntl-arg arg*)
          (let ([x (parse-format who src cntl-arg arg*)])
            (and x
                 (cond
                   [(and (fx= (length x) 1)
                         (string? (car x))
                         (nanopass-case (L7 Expr) op-arg
                           [(quote ,d) (eq? d #f)]
                           [else #f]))
                    (%primcall src sexpr string-copy (quote ,(car x)))]
                   [(and (nanopass-case (L7 Expr) op-arg
                           [(quote ,d) (not (eq? d #f))]
                           [else #t])
                         (let-values ([(op-arg dobind) (binder #t 'ptr op-arg)]
                                      [(arg* dobind*) (list-binder #t 'ptr arg*)])
                           (let ([e (fmt->expr src sexpr x op-arg arg*)])
                             (and e (dobind (dobind* e))))))]
                   [else
                    (%primcall src sexpr $dofmt (quote ,who) ,op-arg ,cntl-arg
                      (quote ,x)
                      ,(build-list arg*))])))))
      (define-inline 2 errorf
        [(e-who e-str . e*)
         (parse-format 'errorf src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #t) #f ,(Symref 'errorf) ,e-who ,e-str ,e* ...))])
      (define-inline 2 assertion-violationf
        [(e-who e-str . e*)
         (parse-format 'assertion-violationf src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #t) #f ,(Symref 'assertion-violationf) ,e-who ,e-str ,e* ...))])
      (define-inline 2 $oops
        [(e-who e-str . e*)
         (parse-format '$oops src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #t) #f ,(Symref '$oops) ,e-who ,e-str ,e* ...))])
      (define-inline 2 $impoops
        [(e-who e-str . e*)
         (parse-format '$impoops src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #t) #f ,(Symref '$impoops) ,e-who ,e-str ,e* ...))])
      (define-inline 2 warningf
        [(e-who e-str . e*)
         (parse-format 'warningf src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #f) #f ,(Symref 'warningf) ,e-who ,e-str ,e* ...))])
      (define-inline 2 $source-violation
        [(e-who e-src e-start? e-str . e*)
         (parse-format '$source-violation src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #t) #f ,(Symref '$source-violation)
                             ,e-who ,e-src ,e-start? ,e-str ,e* ...))])
      (define-inline 2 $source-warning
        [(e-who e-src e-start? e-str . e*)
         (parse-format '$source-warning src e-str e*)
         `(seq (pariah) (call ,(make-info-call src sexpr #f #t #f) #f ,(Symref '$source-warning)
                             ,e-who ,e-src ,e-start? ,e-str ,e* ...))])
      (define-inline 2 fprintf
        [(e-op e-str . e*)
         (parse-format 'fprintf src e-str e*)
         #f])
      (define-inline 3 fprintf
        [(e-op e-str . e*) (build-format 'fprintf src sexpr e-op e-str e*)])
      (define-inline 2 printf
        [(e-str . e*)
         (build-format 'printf src sexpr (%tc-ref current-output) e-str e*)])
      (define-inline 2 format
        [(e . e*)
         (nanopass-case (L7 Expr) e
           [(quote ,d)
            (if (string? d)
                (build-format 'format src sexpr `(quote #f) e e*)
                (and (not (null? e*))
                     (cond
                       [(eq? d #f) (build-format 'format src sexpr e (car e*) (cdr e*))]
                       [(eq? d #t) (build-format 'format src sexpr
                                     (%tc-ref current-output)
                                     (car e*) (cdr e*))]
                       [else #f])))]
           [else #f])]))
    (let ()
      (define hand-coded-closure?
        (lambda (name)
          (not (memq name '(nuate nonprocedure-code error-invoke invoke
                                  $wrapper-apply wrapper-apply arity-wrapper-apply
                                  popcount-slow cpu-features)))))
      (define-inline 2 $hand-coded
        [(name)
         (nanopass-case (L7 Expr) name
           [(quote ,d)
            (guard (symbol? d))
            (let ([l (make-local-label 'hcl)])
              (set! new-l* (cons l new-l*))
              (set! new-le* (cons (with-output-language (L9 CaseLambdaExpr) `(hand-coded ,d)) new-le*))
              (if (hand-coded-closure? d)
                  `(literal ,(make-info-literal #f 'closure l 0))
                  `(label-ref ,l 0)))]
           [(seq (profile ,src) ,[e]) `(seq (profile ,src) ,e)]
           [else ($oops '$hand-coded "~s is not a quoted symbol" name)])]))
    (define-inline 2 $tc
      [() %tc])
    (define-inline 3 $tc-field
      [(e-fld e-tc)
       (nanopass-case (L7 Expr) e-fld
         [(quote ,d)
          (let ()
            (define-syntax a
              (lambda (x)
                #`(case d
                    #,@(fold-left
                         (lambda (ls field)
                           (apply
                             (lambda (name type disp len)
                               (if (eq? type 'ptr)
                                   (cons
                                     (with-syntax ([name (datum->syntax #'* name)])
                                       #'[(name) (%tc-ref ,e-tc name)])
                                     ls)
                                   ls))
                             field))
                         '() (getprop 'tc '*fields* '()))
                    [else #f])))
            a)]
         [else #f])]
      [(e-fld e-tc e-val)
       (nanopass-case (L7 Expr) e-fld
         [(quote ,d)
          (let ()
            (define-syntax a
              (lambda (x)
                #`(case d
                    #,@(fold-left
                         (lambda (ls field)
                           (apply
                             (lambda (name type disp len)
                               (if (eq? type 'ptr)
                                   (cons
                                     (with-syntax ([name (datum->syntax #'* name)])
                                       #'[(name) `(set! ,(%tc-ref ,e-tc name) ,e-val)])
                                     ls)
                                   ls))
                             field))
                         '() (getprop 'tc '*fields* '()))
                    [else #f])))
            a)]
         [else #f])])
    (let ()
      (define-syntax define-tc-parameter
        (syntax-rules ()
          [(_ name tc-name)
           (begin
             (define-inline 2 name
               [() (%tc-ref tc-name)]
               [(x) #f])
             (define-inline 3 name
               [() (%tc-ref tc-name)]
               [(x) `(set! ,(%tc-ref tc-name) ,x)]))]))

      (define-tc-parameter current-input-port current-input)
      (define-tc-parameter current-output-port current-output)
      (define-tc-parameter current-error-port current-error)
      (define-tc-parameter generate-inspector-information generate-inspector-information)
      (define-tc-parameter generate-procedure-source-information generate-procedure-source-information)
      (define-tc-parameter generate-profile-forms generate-profile-forms)
      (define-tc-parameter $compile-profile compile-profile)
      (define-tc-parameter optimize-level optimize-level)
      (define-tc-parameter subset-mode subset-mode)
      (define-tc-parameter $suppress-primitive-inlining suppress-primitive-inlining)
      (define-tc-parameter $block-counter block-counter)
      (define-tc-parameter $sfd sfd)
      (define-tc-parameter $current-mso current-mso)
      (define-tc-parameter $target-machine target-machine)
      (define-tc-parameter $current-stack-link stack-link)
      (define-tc-parameter $current-winders winders)
      (define-tc-parameter $current-attachments attachments)
      (define-tc-parameter $current-handler-stack handler-stack)
      (define-tc-parameter default-record-equal-procedure default-record-equal-procedure)
      (define-tc-parameter default-record-hash-procedure default-record-hash-procedure)
      )

    (let ()
      (define (make-wrapper-closure-alloc e-proc e-arity-mask e-data libspec)
        (bind #t ([c (%constant-alloc type-closure (fx* (if e-data 4 3) (constant ptr-bytes)))])
          (%seq
            (set! ,(%mref ,c ,(constant closure-code-disp))
                  (literal ,(make-info-literal #f 'library libspec (constant code-data-disp))))
            (set! ,(%mref ,c ,(constant closure-data-disp)) ,e-proc)
            (set! ,(%mref ,c ,(fx+ (constant ptr-bytes) (constant closure-data-disp))) ,e-arity-mask)
            ,(if e-data
                 (%seq
                   (set! ,(%mref ,c ,(fx+ (fx* (constant ptr-bytes) 2) (constant closure-data-disp))) ,e-data)
                   ,c)
                 c))))
      (define-inline 3 $make-wrapper-procedure
        [(e-proc e-arity-mask)
         (bind #f (e-proc e-arity-mask)
           (make-wrapper-closure-alloc e-proc e-arity-mask #f (lookup-libspec $wrapper-apply)))])
      (define-inline 3 make-wrapper-procedure
        [(e-proc e-arity-mask e-data)
         (bind #f (e-proc e-arity-mask e-data)
           (make-wrapper-closure-alloc e-proc e-arity-mask e-data (lookup-libspec wrapper-apply)))])
      (define-inline 3 make-arity-wrapper-procedure
        [(e-proc e-arity-mask e-data)
         (bind #f (e-proc e-arity-mask e-data)
           (make-wrapper-closure-alloc e-proc e-arity-mask e-data (lookup-libspec arity-wrapper-apply)))]))

    (define-inline 3 $install-guardian
      [(e-obj e-rep e-tconc ordered?)
       (bind #f (e-obj e-rep e-tconc ordered?)
         (bind #t ([t (%constant-alloc type-untyped (constant size-guardian-entry))])
           (%seq
             (set! ,(%mref ,t ,(constant guardian-entry-obj-disp)) ,e-obj)
             (set! ,(%mref ,t ,(constant guardian-entry-rep-disp)) ,e-rep)
             (set! ,(%mref ,t ,(constant guardian-entry-tconc-disp)) ,e-tconc)
             (set! ,(%mref ,t ,(constant guardian-entry-next-disp)) ,(%tc-ref guardian-entries))
             (set! ,(%mref ,t ,(constant guardian-entry-ordered?-disp)) ,ordered?)
             (set! ,(%mref ,t ,(constant guardian-entry-pending-disp)) ,(%constant snil))
             (set! ,(%tc-ref guardian-entries) ,t))))])

    (define-inline 3 $install-ftype-guardian
      [(e-obj e-tconc)
       (bind #f (e-obj e-tconc)
         (bind #t ([t (%constant-alloc type-untyped (constant size-guardian-entry))])
           (%seq
             (set! ,(%mref ,t ,(constant guardian-entry-obj-disp)) ,e-obj)
             (set! ,(%mref ,t ,(constant guardian-entry-rep-disp)) (immediate ,(constant ftype-guardian-rep)))
             (set! ,(%mref ,t ,(constant guardian-entry-tconc-disp)) ,e-tconc)
             (set! ,(%mref ,t ,(constant guardian-entry-next-disp)) ,(%tc-ref guardian-entries))
             (set! ,(%mref ,t ,(constant guardian-entry-ordered?-disp)) ,(%constant sfalse))
             (set! ,(%mref ,t ,(constant guardian-entry-pending-disp)) ,(%constant snil))
             (set! ,(%tc-ref guardian-entries) ,t))))])

    (define-inline 2 guardian?
      [(e)
       (bind #t (e)
         (build-and
           (%type-check mask-closure type-closure ,e)
           (%type-check mask-guardian-code type-guardian-code
             ,(%mref
                ,(%inline -
                  ,(%mref ,e ,(constant closure-code-disp))
                  ,(%constant code-data-disp))
                ,(constant code-type-disp)))))])

    (define-inline 3 $make-phantom-bytevector
      [()
       (bind #f ()
         (bind #t ([t (%constant-alloc type-typed-object (constant size-phantom))])
               (%seq
                (set! ,(%mref ,t ,(constant phantom-type-disp))
                      ,(%constant type-phantom))
                (set! ,(%mref ,t ,(constant phantom-length-disp))
                      (immediate 0))
                ,t)))])

    (define-inline 3 phantom-bytevector-length
      [(e-ph)
       (bind #f (e-ph)
         (unsigned->ptr (%mref ,e-ph ,(constant phantom-length-disp))
                        (constant ptr-bits)))])

    (define-inline 2 virtual-register-count
      [() `(quote ,(constant virtual-register-count))])
    (let ()
      (define constant-ref
        (lambda (e-idx)
          (nanopass-case (L7 Expr) e-idx
            [(quote ,d)
             (guard (and (fixnum? d) ($fxu< d (constant virtual-register-count))))
             (%mref ,%tc ,(fx+ (constant tc-virtual-registers-disp) (fx* d (constant ptr-bytes))))]
            [else #f])))
      (define constant-set
        (lambda (e-idx e-val)
          (let ([ref (constant-ref e-idx)])
            (and ref `(set! ,ref ,e-val)))))
      (define index-check
        (lambda (e-idx libcall e)
          `(if (if ,(%type-check mask-fixnum type-fixnum ,e-idx)
                   ,(%inline u< ,e-idx (immediate ,(fix (constant virtual-register-count))))
                   ,(%constant sfalse))
               ,e
               ,libcall)))
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (define-inline 3 virtual-register
        [(e-idx)
         (or (constant-ref e-idx)
             (%mref ,%tc ,e-idx ,(constant tc-virtual-registers-disp)))])
      (define-inline 2 virtual-register
        [(e-idx)
         (or (constant-ref e-idx)
             (bind #t (e-idx)
               (index-check e-idx
                 (build-libcall #t src sexpr virtual-register e-idx)
                 (%mref ,%tc ,e-idx ,(constant tc-virtual-registers-disp)))))])
      (define-inline 3 set-virtual-register!
        [(e-idx e-val)
         (or (constant-set e-idx e-val)
             `(set! ,(%mref ,%tc ,e-idx ,(constant tc-virtual-registers-disp)) ,e-val))])
      (define-inline 2 set-virtual-register!
        [(e-idx e-val)
         (or (constant-set e-idx e-val)
             (bind #t (e-idx)
               (bind #f (e-val)
               (index-check e-idx
                 (build-libcall #t src sexpr set-virtual-register! e-idx)
                 `(set! ,(%mref ,%tc ,e-idx ,(constant tc-virtual-registers-disp)) ,e-val)))))]))

    (define-inline 2 $thread-list
      [() `(literal ,(make-info-literal #t 'entry (lookup-c-entry thread-list) 0))])
    (when-feature pthreads
      (define-inline 2 $raw-tc-mutex
        [() `(literal ,(make-info-literal #f 'entry (lookup-c-entry raw-tc-mutex) 0))])
      (define-inline 2 $raw-terminated-cond
        [() `(literal ,(make-info-literal #f 'entry (lookup-c-entry raw-terminated-cond) 0))])
      (define-inline 2 $raw-collect-cond
        [() `(literal ,(make-info-literal #f 'entry (lookup-c-entry raw-collect-cond) 0))])
      (define-inline 2 $raw-collect-thread0-cond
        [() `(literal ,(make-info-literal #f 'entry (lookup-c-entry raw-collect-thread0-cond) 0))]))
    (define-inline 2 not
      [(e) `(if ,e ,(%constant sfalse) ,(%constant strue))])
    (define-inline 2 most-negative-fixnum
      [() `(quote ,(constant most-negative-fixnum))])
    (define-inline 2 most-positive-fixnum
      [() `(quote ,(constant most-positive-fixnum))])
    (define-inline 2 least-fixnum
      [() `(quote ,(constant most-negative-fixnum))])
    (define-inline 2 greatest-fixnum
      [() `(quote ,(constant most-positive-fixnum))])
    (define-inline 2 fixnum-width
      [() `(quote ,(constant fixnum-bits))])
    (constant-case native-endianness
      [(unknown) (void)]
      [else
       (define-inline 2 native-endianness
         [() `(quote ,(constant native-endianness))])])
    (constant-case architecture
      [(pb) (void)]
      [else
       (define-inline 2 directory-separator
         [() `(quote ,(if-feature windows #\\ #\/))])])
    (let () ; level 2 char=?, r6rs:char=?, etc.
      (define-syntax char-pred
        (syntax-rules ()
          [(_ op r6rs:op inline-op)
           (let ()
             (define (go2 src sexpr e1 e2)
               (bind #t (e1 e2)
                 `(if ,(build-chars? e1 e2)
                      ,(%inline inline-op ,e1 ,e2)
                      ,(build-libcall #t src sexpr op e1 e2))))
             (define (go3 src sexpr e1 e2 e3)
               (and (constant? char? e1)
                    (constant? char? e3)
                    (bind #t (e2)
                      `(if ,(%type-check mask-char type-char ,e2)
                           ,(build-and
                              (%inline inline-op ,e1 ,e2)
                              (%inline inline-op ,e2 ,e3))
                           ; could also pass e2 and e3:
                           ,(build-libcall #t src sexpr op e1 e2)))))
             (define-inline 2 op
               [(e1 e2) (go2 src sexpr e1 e2)]
               [(e1 e2 e3) (go3 src sexpr e1 e2 e3)]
               [(e1 . e*) #f])
             (define-inline 2 r6rs:op
               [(e1 e2) (go2 src sexpr e1 e2)]
               [(e1 e2 e3) (go3 src sexpr e1 e2 e3)]
               [(e1 e2 . e*) #f]))]))
      (char-pred char<? r6rs:char<? <)
      (char-pred char<=? r6rs:char<=? <=)
      (char-pred char=? r6rs:char=? eq?)
      (char-pred char>=? r6rs:char>=? >=)
      (char-pred char>? r6rs:char>? >))
    (let () ; level 3 char=?, r6rs:char=?, etc.
      (define-syntax char-pred
        (syntax-rules ()
          [(_ op r6rs:op inline-op)
           (let ()
             (define (go2 e1 e2)
               (%inline inline-op ,e1 ,e2))
             (define (go3 e1 e2 e3)
               (bind #t (e2)
                 (bind #f (e3)
                   (build-and
                     (go2 e1 e2)
                     (go2 e2 e3)))))
             (define-inline 3 op
               [(e) `(seq ,e ,(%constant strue))]
               [(e1 e2) (go2 e1 e2)]
               [(e1 e2 e3) (go3 e1 e2 e3)]
               [(e1 . e*) #f])
             (define-inline 3 r6rs:op
               [(e1 e2) (go2 e1 e2)]
               [(e1 e2 e3) (go3 e1 e2 e3)]
               [(e1 e2 . e*) #f]))]))
      (char-pred char<? r6rs:char<? <)
      (char-pred char<=? r6rs:char<=? <=)
      (char-pred char=? r6rs:char=? eq?)
      (char-pred char>=? r6rs:char>=? >=)
      (char-pred char>? r6rs:char>? >))
    (define-inline 3 char-grapheme-step
      [(e-ch e-state)
       (bind #t (e-ch e-state)
         ;; Handle ASCII non-control charaters inline when the state is simple enough:
         `(if ,(build-and
                (%inline > ,e-ch (immediate ,(+ (constant type-char)
                                                ;; last low-ASCII control character:
                                                (fxsll #x1f (constant char-data-offset)))))
                (%inline < ,e-ch (immediate ,(+ (constant type-char)
                                                ;; first high-ASCII control character:
                                                (fxsll #x7f (constant char-data-offset))))))
              (if ,(%inline eq? ,e-state (immediate ,(fix ($char-grapheme-other-state))))
                  ,(%primcall src sexpr values ,(%constant strue) (immediate ,(fix ($char-grapheme-other-state))))
                  (if ,(%inline eq? ,e-state (immediate ,(fix 0)))
                      ,(%primcall src sexpr values ,(%constant sfalse) (immediate ,(fix ($char-grapheme-other-state))))
                      ,(%primcall src sexpr $char-grapheme-step ,e-ch ,e-state)))
              ,(%primcall src sexpr $char-grapheme-step ,e-ch ,e-state)))])
    (define-inline 3 map
      [(e-proc e-ls)
       (or (nanopass-case (L7 Expr) e-proc
             [,pr
              (and (all-set? (prim-mask unsafe) (primref-flags pr))
                   (let ([name (primref-name pr)])
                     (or (and (eq? name 'car) (build-libcall #f src sexpr map-car e-ls))
                         (and (eq? name 'cdr) (build-libcall #f src sexpr map-cdr e-ls)))))]
             [else #f])
           (build-libcall #f src sexpr map1 e-proc e-ls))]
      [(e-proc e-ls1 e-ls2)
       (or (nanopass-case (L7 Expr) e-proc
             [,pr
              (and (eq? (primref-name pr) 'cons)
                   (build-libcall #f src sexpr map-cons e-ls1 e-ls2))]
             [else #f])
           (build-libcall #f src sexpr map2 e-proc e-ls1 e-ls2))]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 andmap
      [(e-proc e-ls) (build-libcall #f src sexpr andmap1 e-proc e-ls)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 for-all
      [(e-proc e-ls) (build-libcall #f src sexpr andmap1 e-proc e-ls)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 ormap
      [(e-proc e-ls) (build-libcall #f src sexpr ormap1 e-proc e-ls)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 exists
      [(e-proc e-ls) (build-libcall #f src sexpr ormap1 e-proc e-ls)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 fold-left
      [(e-proc e-base e-ls) (build-libcall #f src sexpr fold-left1 e-proc e-base e-ls)]
      [(e-proc e-base e-ls1 e-ls2) (build-libcall #f src sexpr fold-left2 e-proc e-base e-ls1 e-ls2)]
      [(e-proc e-base e-ls . e-ls*) #f])
    (define-inline 3 fold-right
      [(e-proc e-base e-ls) (build-libcall #f src sexpr fold-right1 e-proc e-base e-ls)]
      [(e-proc e-base e-ls1 e-ls2) (build-libcall #f src sexpr fold-right2 e-proc e-base e-ls1 e-ls2)]
      [(e-proc e-base e-ls . e-ls*) #f])
    (define-inline 3 for-each
      [(e-proc e-ls) (build-libcall #f src sexpr for-each1 e-proc e-ls)]
      [(e-proc e-ls1 e-ls2) (build-libcall #f src sexpr for-each2 e-proc e-ls1 e-ls2)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 vector-map
      [(e-proc e-ls) (build-libcall #f src sexpr vector-map1 e-proc e-ls)]
      [(e-proc e-ls1 e-ls2) (build-libcall #f src sexpr vector-map2 e-proc e-ls1 e-ls2)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 vector-for-each
      [(e-proc e-ls) (build-libcall #f src sexpr vector-for-each1 e-proc e-ls)]
      [(e-proc e-ls1 e-ls2) (build-libcall #f src sexpr vector-for-each2 e-proc e-ls1 e-ls2)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 string-for-each
      [(e-proc e-ls) (build-libcall #f src sexpr string-for-each1 e-proc e-ls)]
      [(e-proc e-ls1 e-ls2) (build-libcall #f src sexpr string-for-each2 e-proc e-ls1 e-ls2)]
      [(e-proc e-ls . e-ls*) #f])
    (define-inline 3 reverse
      [(e) (build-libcall #f src sexpr reverse e)])
    (let ()
      (define inline-getprop
        (lambda (plist-offset e-sym e-key e-dflt)
          (let ([t-ls (make-assigned-tmp 't-ls)] [t-cdr (make-tmp 't-cdr)] [Ltop (make-local-label 'Ltop)])
            (bind #t (e-key e-dflt)
              ; indirect symbol after evaluating e-key and e-dflt
              `(let ([,t-ls ,(%mref ,e-sym ,plist-offset)])
                 (label ,Ltop
                   (if ,(%inline eq? ,t-ls ,(%constant snil))
                       ,e-dflt
                       (let ([,t-cdr ,(%mref ,t-ls ,(constant pair-cdr-disp))])
                         (if ,(%inline eq? ,(%mref ,t-ls ,(constant pair-car-disp)) ,e-key)
                             ,(%mref ,t-cdr ,(constant pair-car-disp))
                             (seq
                               (set! ,t-ls ,(%mref ,t-cdr ,(constant pair-cdr-disp)))
                               (goto ,Ltop)))))))))))
      (define-inline 3 getprop
        [(e-sym e-key) (inline-getprop (constant symbol-plist-disp) e-sym e-key (%constant sfalse))]
        [(e-sym e-key e-dflt) (inline-getprop (constant symbol-plist-disp) e-sym e-key e-dflt)])
      (define-inline 3 $sgetprop
        [(e-sym e-key e-dflt) (inline-getprop (constant symbol-splist-disp) e-sym e-key e-dflt)]))
    (define-inline 3 assq
      [(e-key e-ls)
       (let ([t-ls (make-assigned-tmp 't-ls)] [Ltop (make-local-label 'Ltop)])
         (bind #t (e-key)
           `(let ([,t-ls ,e-ls])
              (label ,Ltop
                (if ,(%inline eq? ,t-ls ,(%constant snil))
                    ,(%constant sfalse)
                    ,(bind #t ([t-a (%mref ,t-ls ,(constant pair-car-disp))])
                       `(if ,(%inline eq? ,(%mref ,t-a ,(constant pair-car-disp)) ,e-key)
                            ,t-a
                            (seq
                              (set! ,t-ls ,(%mref ,t-ls ,(constant pair-cdr-disp)))
                              (goto ,Ltop)))))))))])
    (define-inline 3 length
      [(e-ls)
       (let ([t-ls (make-assigned-tmp 't-ls)]
             [t-n (make-assigned-tmp 't-n)]
             [Ltop (make-local-label 'Ltop)])
         (bind #t (e-ls)
           `(if ,(%inline eq? ,e-ls ,(%constant snil))
                (immediate ,(fix 0))
                (let ([,t-ls ,e-ls] [,t-n (immediate ,(fix 0))])
                  (label ,Ltop
                    ,(%seq
                       (set! ,t-ls ,(%mref ,t-ls ,(constant pair-cdr-disp)))
                       (set! ,t-n ,(%inline + ,t-n (immediate ,(fix 1))))
                       (if ,(%inline eq? ,t-ls ,(%constant snil))
                           ,t-n
                           (goto ,Ltop))))))))])
    (define-inline 3 append
      ; TODO: hand-coded library routine that allocates the new pairs in a block
      [() (%constant snil)]
      [(e-ls) e-ls]
      [(e-ls1 e-ls2) (build-libcall #f src sexpr append e-ls1 e-ls2)]
      [(e-ls1 e-ls2 e-ls3)
       (build-libcall #f src sexpr append e-ls1
         (build-libcall #f #f sexpr append e-ls2 e-ls3))]
      [(e-ls . e-ls*) #f])
    (define-inline 3 apply
      [(e0 e1) (build-libcall #f src sexpr apply0 e0 e1)]
      [(e0 e1 e2) (build-libcall #f src sexpr apply1 e0 e1 e2)]
      [(e0 e1 e2 e3) (build-libcall #f src sexpr apply2 e0 e1 e2 e3)]
      [(e0 e1 e2 e3 e4) (build-libcall #f src sexpr apply3 e0 e1 e2 e3 e4)]
      [(e0 e1 . e*) #f])
    (define-inline 2 fxsll
      [(e0 e1) (build-libcall #f src sexpr fxsll e0 e1)])
    (define-inline 2 fxarithmetic-shift-left
      [(e0 e1) (build-libcall #f src sexpr fxarithmetic-shift-left e0 e1)])
    (define-inline 2 fxsll/wraparound
      [(e1 e2)
       (bind #t (e1 e2)
         `(if ,(nanopass-case (L7 Expr) e2
                 [(quote ,d)
                  (guard (target-fixnum? d)
                         ($fxu< d (fx+ 1 (constant fixnum-bits))))
                  (build-fixnums? (list e1 e2))]
                 [else
                  (build-and (build-fixnums? (list e1 e2))
                             (%inline u< ,e2 (immediate ,(fix (fx+ 1 (constant fixnum-bits))))))])
              ,(%inline sll ,e1 ,(build-unfix e2))
              ,(build-libcall #t src sexpr fxsll/wraparound e1 e2)))])
    (define-inline 3 display-string
      [(e-s) (build-libcall #f src sexpr display-string e-s (%tc-ref current-output))]
      [(e-s e-op) (build-libcall #f src sexpr display-string e-s e-op)])
    (define-inline 3 call-with-current-continuation
      [(e) (build-libcall #f src sexpr callcc e)])
    (define-inline 3 call/cc
      [(e) (build-libcall #f src sexpr callcc e)])
    (define-inline 3 call/1cc
      [(e) (build-libcall #f src sexpr call1cc e)])
    (define-inline 2 $event
      [() (build-libcall #f src sexpr event)])
    (define-inline 2 $event-trap-check
      [() `(call ,(make-info-call src sexpr #f #f #f) ,(make-trap-check-label '$event-trap-check) #f)])
    (define-inline 3 eq-hashtable-ref
      [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-ref e1 e2 e3)])
	(define-inline 3 eq-hashtable-ref-cell
	  [(e1 e2) (build-libcall #f src sexpr eq-hashtable-ref-cell e1 e2)])
    (define-inline 3 eq-hashtable-contains?
      [(e1 e2) (build-libcall #f src sexpr eq-hashtable-contains? e1 e2)])
    (define-inline 3 eq-hashtable-set!
      [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-set! e1 e2 e3)])
    (define-inline 3 eq-hashtable-update!
      [(e1 e2 e3 e4) (build-libcall #f src sexpr eq-hashtable-update! e1 e2 e3 e4)])
    (define-inline 3 eq-hashtable-cell
      [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-cell e1 e2 e3)])
    (define-inline 3 eq-hashtable-try-atomic-cell
      [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-try-atomic-cell e1 e2 e3)])
    (define-inline 3 eq-hashtable-delete!
      [(e1 e2) (build-libcall #f src sexpr eq-hashtable-delete! e1 e2)])
    (define-inline 3 symbol-hashtable-ref
      [(e1 e2 e3) (build-libcall #f src sexpr symbol-hashtable-ref e1 e2 e3)])
	(define-inline 3 symbol-hashtable-ref-cell
      [(e1 e2) (build-libcall #f src sexpr symbol-hashtable-ref-cell e1 e2)])
    (define-inline 3 symbol-hashtable-contains?
      [(e1 e2) (build-libcall #f src sexpr symbol-hashtable-contains? e1 e2)])
    (define-inline 3 symbol-hashtable-set!
      [(e1 e2 e3) (build-libcall #f src sexpr symbol-hashtable-set! e1 e2 e3)])
    (define-inline 3 symbol-hashtable-update!
      [(e1 e2 e3 e4) (build-libcall #f src sexpr symbol-hashtable-update! e1 e2 e3 e4)])
    (define-inline 3 symbol-hashtable-cell
      [(e1 e2 e3) (build-libcall #f src sexpr symbol-hashtable-cell e1 e2 e3)])
    (define-inline 3 symbol-hashtable-delete!
      [(e1 e2) (build-libcall #f src sexpr symbol-hashtable-delete! e1 e2)])
    (define-inline 2 bytevector-s8-set!
      [(e1 e2 e3) (build-libcall #f src sexpr bytevector-s8-set! e1 e2 e3)])
    (define-inline 2 bytevector-u8-set!
      [(e1 e2 e3) (build-libcall #f src sexpr bytevector-u8-set! e1 e2 e3)])
    (define-inline 3 bytevector=?
      [(e1 e2) (build-libcall #f src sexpr bytevector=? e1 e2)])
    (let ()
      (define eqvop-flonum
        (lambda (e1 e2)
          (nanopass-case (L7 Expr) e1
            [(quote ,d) (and (flonum? d)
                             (bind #t (e2)
                               (build-and
                                (%type-check mask-flonum type-flonum ,e2)
                                (if ($nan? d)
                                    ;; NaN: invert `fl=` on self
                                    (bind #t (e2)
                                      (build-not (build-fl= e2 e2)))
                                    ;; Non-NaN: compare bits
                                    (constant-case ptr-bits
                                      [(32)
                                       (safe-assert (not (eq? (constant native-endianness) 'unknown)))
                                       (let ([d0 (if (eq? (constant native-endianness) (native-endianness)) 0 4)])
                                         (let ([word1 ($object-ref 'integer-32 d (fx+ (constant flonum-data-disp) d0))]
                                               [word2 ($object-ref 'integer-32 d (fx+ (constant flonum-data-disp) (fx- 4 d0)))])
                                           (build-and
                                            (%inline eq?
                                                     ,(%mref ,e2 ,(constant flonum-data-disp))
                                                     (immediate ,word1))
                                            (%inline eq?
                                                     ,(%mref ,e2 ,(fx+ (constant flonum-data-disp) 4))
                                                     (immediate ,word2)))))]
                                      [(64)
                                       (let ([word ($object-ref 'integer-64 d (constant flonum-data-disp))])
                                         (%inline eq?
                                                  ,(%mref ,e2 ,(constant flonum-data-disp))
                                                  (immediate ,word)))]
                                      [else ($oops 'compiler-internal
                                                   "eqv doesn't handle ptr-bits = ~s"
                                                   (constant ptr-bits))])))))]
          [else #f])))
      (define eqok-help?
        (lambda (obj)
          (or (symbol? obj)
              (char? obj)
              (target-fixnum? obj)
              (null? obj)
              (boolean? obj)
              (eqv? obj "")
              (eqv? obj (string->immutable-string ""))
              (eqv? obj '#())
              (eqv? obj (vector->immutable-vector '#()))
              (eqv? obj '#vu8())
              (eqv? obj (bytevector->immutable-bytevector '#vu8()))
              (eqv? obj '#0=#0#)
              (eq? obj (void))
              (eof-object? obj)
              (bwp-object? obj)
              ($unbound-object? obj)
              (eqv? obj '#vfx())
              (eqv? obj '#vfl()))))
      (define eqvok-help? number?)
      (define eqvnever-help? (lambda (obj) (not (number? obj))))
      (define e*ok?
        (lambda (e*ok-help?)
          (lambda (e)
            (nanopass-case (L7 Expr) e
              [(quote ,d) (e*ok-help? d)]
              [else #f]))))
      (define eqok? (e*ok? eqok-help?))
      (define eqvok? (e*ok? eqvok-help?))
      (define eqvnever? (e*ok? eqvnever-help?))
      (define-inline 2 eqv?
        [(e1 e2) (or (eqvop-null-fptr e1 e2)
                     (relop-length RELOP= e1 e2)
                     (eqvop-flonum e1 e2)
                     (eqvop-flonum e2 e1)
                     (if (or (eqok? e1) (eqok? e2)
                             (eqvnever? e1) (eqvnever? e2))
                         (build-eq? e1 e2)
                         (build-eqv? src sexpr e1 e2)))])
      (let ()
        (define xform-equal?
          (lambda (src sexpr e1 e2)
            (nanopass-case (L7 Expr) e1
              [(quote ,d1)
               (let xform ([d1 d1] [e2 e2] [n 3] [k (lambda (e n) e)])
                 (if (eqok-help? d1)
                     (k (build-eq? `(quote ,d1) e2) n)
                     (if (eqvok-help? d1)
                         (k (build-eqv? src sexpr `(quote ,d1) e2) n)
                         (and (fx> n 0)
                              (pair? d1)
                              (let-values ([(e2 dobind) (binder #t 'ptr e2)])
                                (xform (car d1) (build-car e2) (fx- n 1)
                                  (lambda (a n)
                                    (xform (cdr d1) (build-cdr e2) n
                                      (lambda (d n)
                                        (k (dobind
                                             (build-and
                                               (build-pair? e2)
                                               (build-and a d)))
                                          n))))))))))]
              [else #f])))
        (define-inline 2 equal?
          [(e1 e2) (or (eqvop-null-fptr e1 e2)
                       (relop-length RELOP= e1 e2)
                       (xform-equal? src sexpr e1 e2)
                       (xform-equal? src sexpr e2 e1))]))
      (let ()
        (define mem*ok?
          (lambda (e*ok-help?)
            (lambda (x)
              (nanopass-case (L7 Expr) x
                [(quote ,d)
                  (and (list? d)
                       (let f ([d d])
                         (or (null? d)
                             (and (e*ok-help? (car d))
                                  (f (cdr d))))))]
                [else #f]))))
        (define memqok? (mem*ok? eqok-help?))
        (define memvok? (mem*ok? eqvok-help?))
        (define mem*->e*?s
          (lambda (build-e*? limit)
            (lambda (e-key e-ls)
            (nanopass-case (L7 Expr) e-ls
              [(quote ,d)
               (and (let f ([d d] [n 0])
                      (or (null? d)
                          (and (pair? d)
                               (fx< n limit)
                               (f (cdr d) (fx1+ n)))))
                    (bind #t (e-key)
                      (let f ([ls d])
                        (if (null? ls)
                            `(quote #f)
                            `(if ,(build-e*? e-key `(quote ,(car ls)))
                                 (quote ,ls)
                                 ,(f (cdr ls)))))))]
              [else #f]))))
        (define memq->eq?s (mem*->e*?s build-eq? 8))
        (define (memv->eqv?s src sexpr) (mem*->e*?s (make-build-eqv? src sexpr) 4))
        (define do-memq
          (lambda (src sexpr e-key e-ls)
            (or (memq->eq?s e-key e-ls)
                (let ([t-ls (make-assigned-tmp 't-ls)] [Ltop (make-local-label 'Ltop)])
                  (bind #t (e-key)
                    `(let ([,t-ls ,e-ls])
                       (label ,Ltop
                         (if ,(%inline eq? ,t-ls ,(%constant snil))
                             ,(%constant sfalse)
                             (if ,(%inline eq? ,(%mref ,t-ls ,(constant pair-car-disp)) ,e-key)
                                 ,t-ls
                                 (seq
                                   (set! ,t-ls ,(%mref ,t-ls ,(constant pair-cdr-disp)))
                                   (goto ,Ltop)))))))))))
        (define do-memv
          (lambda (src sexpr e-key e-ls)
            (or ((memv->eqv?s src sexpr) e-key e-ls)
                (build-libcall #f src sexpr memv e-key e-ls))))
        (define-inline 3 memq
          [(e-key e-ls) (do-memq src sexpr e-key e-ls)])
        (define-inline 3 memv
          [(e-key e-ls)
           (if (or (eqok? e-key) (memqok? e-ls))
               (do-memq src sexpr e-key e-ls)
               (do-memv src sexpr e-key e-ls))])
        (define-inline 3 member
          [(e-key e-ls)
           (if (or (eqok? e-key) (memqok? e-ls))
               (do-memq src sexpr e-key e-ls)
               (and (or (eqvok? e-key) (memvok? e-ls))
                    (do-memv src sexpr e-key e-ls)))])
        (define-inline 2 memq
          [(e-key e-ls) (memq->eq?s e-key e-ls)])
        (define-inline 2 memv
          [(e-key e-ls) (or (and (memqok? e-ls) (memq->eq?s e-key e-ls))
                            ((memv->eqv?s src sexpr) e-key e-ls))])
        (define-inline 2 member
          [(e-key e-ls) (or (and (memqok? e-ls) (memq->eq?s e-key e-ls))
                            (and (memvok? e-ls) ((memv->eqv?s src sexpr) e-key e-ls)))])))
    ; NB: for all of the I/O routines, consider putting optimize-level 2 code out-of-line
    ; w/o going all the way to the port handler, i.e., always defer to library routine but
    ; have library routine do the checks and run the optimize-level 3 version...this could
    ; save a lot of code
    ; NB: verify that the inline checks don't always fail, i.e., don't always send us to the
    ; library routine
    (let ()
      (define (go src sexpr e-p check? update? do-libcall)
        (let ([Llib (and check? (make-local-label 'Llib))])
          (define maybe-add-port-check
            (lambda (e-p body)
              (if Llib
                  `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                           ,(%type-check mask-binary-input-port type-binary-input-port
                              ,(%mref ,e-p ,(constant typed-object-type-disp)))
                           ,(%constant sfalse))
                       ,body
                       (goto ,Llib))
                  body)))
          (define maybe-add-update
            (lambda (t0 e-icount body)
              (if update?
                  `(seq
                     (set! ,e-icount ,(%inline + ,t0 (immediate 1)))
                     ,body)
                  body)))
          (bind #t (e-p)
            (let ([e-icount (%mref ,e-p ,(constant port-icount-disp))])
              (maybe-add-port-check e-p
                (bind #t ([t0 e-icount])
                  `(if ,(%inline eq? ,t0 (immediate 0))
                       ,(maybe-add-label Llib (do-libcall src sexpr e-p))
                       ,(maybe-add-update t0 e-icount
                          ; TODO: this doesn't completely fall away when used in effect context
                          (build-fix
                            `(inline ,(make-info-load 'unsigned-8 #f) ,%load
                               ,t0
                               ,(%mref ,e-p ,(constant port-ilast-disp))
                               (immediate 0)))))))))))
      (define (unsafe-lookahead-u8-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-lookahead-u8 e-p))
      (define (safe-lookahead-u8-libcall src sexpr e-p) (build-libcall #t src sexpr safe-lookahead-u8 e-p))
      (define (unsafe-get-u8-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-get-u8 e-p))
      (define (safe-get-u8-libcall src sexpr e-p) (build-libcall #t src sexpr safe-get-u8 e-p))
      (define-inline 3 lookahead-u8
        [(e-p) (go src sexpr e-p #f #f unsafe-lookahead-u8-libcall)])
      (define-inline 2 lookahead-u8
        [(e-p) (go src sexpr e-p #t #f safe-lookahead-u8-libcall)])
      (define-inline 3 get-u8
        [(e-p) (go src sexpr e-p #f #t unsafe-get-u8-libcall)])
      (define-inline 2 get-u8
        [(e-p) (go src sexpr e-p #t #t safe-get-u8-libcall)]))
    (let ()
      (define (go src sexpr e-p check? update? do-libcall)
        (let ([Llib (and check? (make-local-label 'Llib))])
          (define maybe-add-port-check
            (lambda (e-p body)
              (if Llib
                  `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                           ,(%type-check mask-textual-input-port type-textual-input-port
                              ,(%mref ,e-p ,(constant typed-object-type-disp)))
                           ,(%constant sfalse))
                       ,body
                       (goto ,Llib))
                  body)))
          (define maybe-add-update
            (lambda (t0 e-icount body)
              (if update?
                  `(seq
                     (set! ,e-icount ,(%inline + ,t0 ,(%constant string-char-bytes)))
                     ,body)
                  body)))
          (bind #t (e-p)
            (let ([e-icount (%mref ,e-p ,(constant port-icount-disp))])
              (maybe-add-port-check e-p
                (bind #t ([t0 e-icount])
                  `(if ,(%inline eq? ,t0 (immediate 0))
                       ,(maybe-add-label Llib (do-libcall src sexpr e-p))
                       ,(maybe-add-update t0 e-icount
                          ; TODO: this doesn't completely fall away when used in effect context
                          `(inline ,(make-info-load (string-char-type) #f) ,%load
                             ,t0
                             ,(%mref ,e-p ,(constant port-ilast-disp))
                             (immediate 0))))))))))
      (define (unsafe-lookahead-char-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-lookahead-char e-p))
      (define (safe-lookahead-char-libcall src sexpr e-p) (build-libcall #t src sexpr safe-lookahead-char e-p))
      (define (unsafe-peek-char-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-peek-char e-p))
      (define (safe-peek-char-libcall src sexpr e-p) (build-libcall #t src sexpr safe-peek-char e-p))
      (define (unsafe-get-char-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-get-char e-p))
      (define (safe-get-char-libcall src sexpr e-p) (build-libcall #t src sexpr safe-get-char e-p))
      (define (unsafe-read-char-libcall src sexpr e-p) (build-libcall #t src sexpr unsafe-read-char e-p))
      (define (safe-read-char-libcall src sexpr e-p) (build-libcall #t src sexpr safe-read-char e-p))
      (define-inline 3 lookahead-char
        [(e-p) (go src sexpr e-p #f #f unsafe-lookahead-char-libcall)])
      (define-inline 2 lookahead-char
        [(e-p) (go src sexpr e-p #t #f safe-lookahead-char-libcall)])
      (define-inline 3 peek-char
        [() (go src sexpr (%tc-ref current-input) #f #f unsafe-peek-char-libcall)]
        [(e-p) (go src sexpr e-p #f #f unsafe-peek-char-libcall)])
      (define-inline 2 peek-char
        [() (go src sexpr (%tc-ref current-input) #f #f unsafe-peek-char-libcall)]
        [(e-p) (go src sexpr e-p #t #f safe-peek-char-libcall)])
      (define-inline 3 get-char
        [(e-p) (go src sexpr e-p #f #t unsafe-get-char-libcall)])
      (define-inline 2 get-char
        [(e-p) (go src sexpr e-p #t #t safe-get-char-libcall)])
      (define-inline 3 read-char
        [() (go src sexpr (%tc-ref current-input) #f #t unsafe-read-char-libcall)]
        [(e-p) (go src sexpr e-p #f #t unsafe-read-char-libcall)])
      (define-inline 2 read-char
        [() (go src sexpr (%tc-ref current-input) #f #t unsafe-read-char-libcall)]
        [(e-p) (go src sexpr e-p #t #t safe-read-char-libcall)]))
    (let ()
      (define (go src sexpr e-p e-c check-port? check-char? do-libcall)
        (let ([const-char? (constant? char? e-c)])
          (let ([Llib (and (or check-char? check-port? (not const-char?)) (make-local-label 'Llib))])
            (define maybe-add-port-check
              (lambda (e-p body)
                (if check-port?
                    `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                             ,(%type-check mask-textual-input-port type-textual-input-port
                                ,(%mref ,e-p ,(constant typed-object-type-disp)))
                             ,(%constant sfalse))
                         ,body
                         (goto ,Llib))
                    body)))
            (define maybe-add-eof-check
              (lambda (e-c body)
                (if const-char?
                    body
                    `(if ,(%inline eq? ,e-c ,(%constant seof))
                         (goto ,Llib)
                         ,body))))
            (define maybe-add-char-check
              (lambda (e-c body)
                (if check-char?
                    `(if ,(%type-check mask-char type-char ,e-c)
                         ,body
                         (goto ,Llib))
                    body)))
            (bind #t (e-c e-p)
              (let ([e-icount (%mref ,e-p ,(constant port-icount-disp))])
                (maybe-add-port-check e-p
                  (maybe-add-eof-check e-c
                    (maybe-add-char-check e-c
                      (bind #t ([t0 e-icount])
                        `(if ,(%inline eq? ,t0
                                  ,(%inline -
                                    ,(%inline +
                                      ,(%mref ,e-p ,(constant port-ibuffer-disp))
                                      ,(%constant string-data-disp))
                                    ,(%mref ,e-p ,(constant port-ilast-disp))))
                             ,(maybe-add-label Llib (do-libcall src sexpr e-p e-c))
                             (set! ,e-icount ,(%inline - ,t0 ,(%constant string-char-bytes)))))))))))))
      (define (unsafe-unget-char-libcall src sexpr e-p e-c) (build-libcall #t src sexpr unsafe-unget-char e-p e-c))
      (define (safe-unget-char-libcall src sexpr e-p e-c) (build-libcall #t src sexpr safe-unget-char e-p e-c))
      (define (unsafe-unread-char-libcall src sexpr e-p e-c) (build-libcall #t src sexpr unsafe-unread-char e-c e-p))
      (define (safe-unread-char-libcall src sexpr e-p e-c) (build-libcall #t src sexpr safe-unread-char e-c e-p))
      (define-inline 3 unget-char
        [(e-p e-c) (go src sexpr e-p e-c #f #f unsafe-unget-char-libcall)])
      (define-inline 2 unget-char
        [(e-p e-c) (go src sexpr e-p e-c #t (not (constant? char? e-c)) safe-unget-char-libcall)])
      (define-inline 3 unread-char
        [(e-c) (go src sexpr (%tc-ref current-input) e-c #f #f unsafe-unread-char-libcall)]
        [(e-c e-p) (go src sexpr e-p e-c #f #f unsafe-unread-char-libcall)])
      (define-inline 2 unread-char
        [(e-c) (if (constant? char? e-c)
                   (go src sexpr (%tc-ref current-input) e-c #f #f unsafe-unread-char-libcall)
                   (go src sexpr (%tc-ref current-input) e-c #f #t safe-unread-char-libcall))]
        [(e-c e-p) (go src sexpr e-p e-c #t (not (constant? char? e-c)) safe-unread-char-libcall)]))
    (let ()
      (define octet?
        (lambda (x)
          (and (fixnum? x) (fx<= 0 x 255))))
      (define maybe-add-octet-check
        (lambda (check-octet? Llib e-o body)
          (if check-octet?
              `(if ,(%type-check mask-octet type-octet ,e-o)
                   ,body
                   (goto ,Llib))
              body)))
      (let ()
        (define (go src sexpr e-p e-o check-port? check-octet? do-libcall)
          (let ([const-octet? (constant? octet? e-o)])
            (let ([Llib (and (or check-octet? check-port? (not const-octet?)) (make-local-label 'Llib))])
              (define maybe-add-port-check
                (lambda (e-p body)
                  (if check-port?
                      `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                               ,(%type-check mask-binary-input-port type-binary-input-port
                                  ,(%mref ,e-p ,(constant typed-object-type-disp)))
                               ,(%constant sfalse))
                           ,body
                           (goto ,Llib))
                      body)))
              (define maybe-add-eof-check
                (lambda (e-o body)
                  (if const-octet?
                      body
                      `(if ,(%inline eq? ,e-o ,(%constant seof))
                              (goto ,Llib)
                              ,body))))
              (bind #t (e-o e-p)
                (let ([e-icount (%mref ,e-p ,(constant port-icount-disp))])
                  (maybe-add-port-check e-p
                    (maybe-add-eof-check e-o
                      (maybe-add-octet-check check-octet? Llib e-o
                        (bind #t ([t0 e-icount])
                          `(if ,(%inline eq? ,t0
                                    ,(%inline -
                                      ,(%inline +
                                        ,(%mref ,e-p ,(constant port-ibuffer-disp))
                                        ,(%constant bytevector-data-disp))
                                      ,(%mref ,e-p ,(constant port-ilast-disp))))
                               ,(maybe-add-label Llib (do-libcall src sexpr e-p e-o))
                               (set! ,e-icount ,(%inline - ,t0 (immediate 1)))))))))))))
        (define (unsafe-unget-u8-libcall src sexpr e-p e-o) (build-libcall #t src sexpr unsafe-unget-u8 e-p e-o))
        (define (safe-unget-u8-libcall src sexpr e-p e-o) (build-libcall #t src sexpr safe-unget-u8 e-p e-o))
        (define-inline 3 unget-u8
          [(e-p e-o) (go src sexpr e-p e-o #f #f unsafe-unget-u8-libcall)])
        (define-inline 2 unget-u8
          [(e-p e-o) (go src sexpr e-p e-o #t (not (constant? octet? e-o)) safe-unget-u8-libcall)]))
      (let ()
        (define (go src sexpr e-p e-o check-port? check-octet? do-libcall)
          (let ([Llib (and (or check-octet? check-port?) (make-local-label 'Llib))])
            (define maybe-add-port-check
              (lambda (e-p body)
                (if check-port?
                    `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                             ,(%type-check mask-binary-output-port type-binary-output-port
                                ,(%mref ,e-p ,(constant typed-object-type-disp)))
                             ,(%constant sfalse))
                         ,body
                         (goto ,Llib))
                    body)))
            (define add-update
              (lambda (t0 e-ocount body)
                `(seq
                   (set! ,e-ocount ,(%inline + ,t0 (immediate 1)))
                   ,body)))
            (bind check-octet? (e-o)
              (bind #t (e-p)
                (let ([e-ocount (%mref ,e-p ,(constant port-ocount-disp))])
                  (maybe-add-octet-check check-octet? Llib e-o
                    (maybe-add-port-check e-p
                      (bind #t ([t0 e-ocount])
                        `(if ,(%inline eq? ,t0 (immediate 0))
                             ,(maybe-add-label Llib (do-libcall src sexpr e-o e-p))
                             ,(add-update t0 e-ocount
                                `(inline ,(make-info-load 'unsigned-8 #f) ,%store
                                   ,t0
                                   ,(%mref ,e-p ,(constant port-olast-disp))
                                   (immediate 0)
                                   ,(build-unfix e-o))))))))))))
        (define (unsafe-put-u8-libcall src sexpr e-o e-p) (build-libcall #t src sexpr unsafe-put-u8 e-p e-o))
        (define (safe-put-u8-libcall src sexpr e-o e-p) (build-libcall #t src sexpr safe-put-u8 e-p e-o))
        (define-inline 3 put-u8
          [(e-p e-o) (go src sexpr e-p e-o #f #f unsafe-put-u8-libcall)])
        (define-inline 2 put-u8
          [(e-p e-o) (go src sexpr e-p e-o #t (not (constant? octet? e-o)) safe-put-u8-libcall)])))
    (let ()
      (define (go src sexpr e-p e-c check-port? check-char? do-libcall)
        (let ([Llib (and (or check-char? check-port?) (make-local-label 'Llib))])
          (define maybe-add-char-check
            (lambda (e-c body)
              (if check-char?
                  `(if ,(%type-check mask-char type-char ,e-c)
                       ,body
                       (goto ,Llib))
                  body)))
          (define maybe-add-port-check
            (lambda (e-p body)
              (if check-port?
                  `(if (if ,(%type-check mask-typed-object type-typed-object ,e-p)
                           ,(%type-check mask-textual-output-port type-textual-output-port
                              ,(%mref ,e-p ,(constant typed-object-type-disp)))
                           ,(%constant sfalse))
                       ,body
                       (goto ,Llib))
                  body)))
          (define add-update
            (lambda (t0 e-ocount body)
              `(seq
                 (set! ,e-ocount ,(%inline + ,t0 ,(%constant string-char-bytes)))
                 ,body)))
          (bind check-char? (e-c)
            (bind #t (e-p)
              (let ([e-ocount (%mref ,e-p ,(constant port-ocount-disp))])
                (maybe-add-char-check e-c
                  (maybe-add-port-check e-p
                    (bind #t ([t0 e-ocount])
                      `(if ,(%inline eq? ,t0 (immediate 0))
                           ,(maybe-add-label Llib (do-libcall src sexpr e-c e-p))
                           ,(add-update t0 e-ocount
                              `(inline ,(make-info-load (string-char-type) #f) ,%store
                                 ,t0
                                 ,(%mref ,e-p ,(constant port-olast-disp))
                                 (immediate 0)
                                 ,e-c)))))))))))
      (define (unsafe-put-char-libcall src sexpr e-c e-p) (build-libcall #t src sexpr unsafe-put-char e-p e-c))
      (define (safe-put-char-libcall src sexpr e-c e-p) (build-libcall #t src sexpr safe-put-char e-p e-c))
      (define (unsafe-write-char-libcall src sexpr e-c e-p) (build-libcall #t src sexpr unsafe-write-char e-c e-p))
      (define (safe-write-char-libcall src sexpr e-c e-p) (build-libcall #t src sexpr safe-write-char e-c e-p))
      (define (unsafe-newline-libcall src sexpr e-c e-p) (build-libcall #t src sexpr unsafe-newline e-p))
      (define (safe-newline-libcall src sexpr e-c e-p) (build-libcall #t src sexpr safe-newline e-p))
      (define-inline 3 put-char
        [(e-p e-c) (go src sexpr e-p e-c #f #f unsafe-put-char-libcall)])
      (define-inline 2 put-char
        [(e-p e-c) (go src sexpr e-p e-c #t (not (constant? char? e-c)) safe-put-char-libcall)])
      (define-inline 3 write-char
        [(e-c) (go src sexpr (%tc-ref current-output) e-c #f #f unsafe-write-char-libcall)]
        [(e-c e-p) (go src sexpr e-p e-c #f #f unsafe-write-char-libcall)])
      (define-inline 2 write-char
        [(e-c) (if (constant? char? e-c)
                   (go src sexpr (%tc-ref current-output) e-c #f #f unsafe-write-char-libcall)
                   (go src sexpr (%tc-ref current-output) e-c #f #t safe-write-char-libcall))]
        [(e-c e-p) (go src sexpr e-p e-c #t (not (constant? char? e-c)) safe-write-char-libcall)])
      (define-inline 3 newline
        [() (go src sexpr (%tc-ref current-output) `(quote #\newline) #f #f unsafe-newline-libcall)]
        [(e-p) (go src sexpr e-p `(quote #\newline) #f #f unsafe-newline-libcall)])
      (define-inline 2 newline
        [() (go src sexpr (%tc-ref current-output) `(quote #\newline) #f #f unsafe-newline-libcall)]
        [(e-p) (go src sexpr e-p `(quote #\newline) #t #f safe-newline-libcall)]))
    (let ()
      (define build-fxop?
        (lambda (op overflow-flag e1 e2 adjust k)
          (let ([Lfail (make-local-label 'Lfail)])
            (bind #t (e1 e2)
              `(if ,(build-fixnums? (list e1 e2))
                   ,(bind #f ([t `(inline ,null-info ,op ,e1 ,(adjust e2))])
                      `(if (inline ,(make-info-condition-code overflow-flag #f #t) ,%condition-code)
                           (label ,Lfail ,(k e1 e2))
                           ,t))
                   (goto ,Lfail))))))
      (define-inline 2 +
        [() `(immediate ,(fix 0))]
        [(e) (build-fxop? %+/ovfl 'overflow e `(quote 0) values (lambda (e1 e2) (build-libcall #t src sexpr + e1 e2)))]
        [(e1 e2) (build-fxop? %+/ovfl 'overflow e1 e2 values (lambda (e1 e2) (build-libcall #t src sexpr + e1 e2)))]
        ; TODO: handle 3-operand case ala fx+, w/3-operand library +
        [(e1 . e*) #f])
      (define-inline 2 *
        [() `(immediate ,(fix 1))]
        [(e) (build-fxop? %*/ovfl 'multiply-overflow e `(quote 1) build-unfix (lambda (e1 e2) (build-libcall #t src sexpr * e1 e2)))]
        ; TODO: swap e1 & e2 if e1 is constant
        [(e1 e2) (build-fxop? %*/ovfl 'multiply-overflow e1 e2 build-unfix (lambda (e1 e2) (build-libcall #t src sexpr * e1 e2)))]
        ; TODO: handle 3-operand case ala fx+, w/3-operand library *
        [(e1 . e*) #f])
      (define-inline 2 -
        [(e) (build-fxop? %-/ovfl 'overflow `(quote 0) e values (lambda (e1 e2) (build-libcall #t src sexpr - e1 e2)))]
        [(e1 e2) (build-fxop? %-/ovfl 'overflow e1 e2 values (lambda (e1 e2) (build-libcall #t src sexpr - e1 e2)))]
        ; TODO: handle 3-operand case ala fx+, w/3-operand library -
        [(e1 e2 . e*) #f]))
    (let ()
      (define build-fxop?
        (lambda (op e k)
          (let ([Lfail (make-local-label 'Lfail)])
            (bind #t (e)
              `(if ,(%type-check mask-fixnum type-fixnum ,e)
                   ,(bind #f ([t `(inline ,null-info ,op ,e (immediate ,(fix 1)))])
                      `(if (inline ,(make-info-condition-code 'overflow #f #t) ,%condition-code)
                           (label ,Lfail ,(k e))
                           ,t))
                   (goto ,Lfail))))))

      (define-syntax define-inline-1op
        (syntax-rules ()
          [(_ op name)
           (define-inline 2 name
             [(e) (build-fxop? op e (lambda (e) (build-libcall #t src sexpr name e)))])]))

      (define-inline-1op %-/ovfl 1-)
      (define-inline-1op %-/ovfl -1+)
      (define-inline-1op %-/ovfl sub1)
      (define-inline-1op %+/ovfl 1+)
      (define-inline-1op %+/ovfl add1))

    (define-inline 2 /
      [(e) (build-libcall #f src sexpr / `(immediate ,(fix 1)) e)]
      [(e1 e2) (build-libcall #f src sexpr / e1 e2)]
      [(e1 . e*) #f])

    (let ()
      (define (zgo src sexpr e e1 e2)
        (build-simple-or
          (%inline eq? ,e (immediate 0))
          `(if ,(build-fixnums? (list e))
               ,(%constant sfalse)
               ,(build-libcall #t src sexpr = e1 e2))))
      (define (go src sexpr e1 e2)
        (or (eqvop-null-fptr e1 e2)
            (relop-length RELOP= e1 e2)
            (cond
              [(constant? (lambda (x) (eqv? x 0)) e1)
               (bind #t (e2) (zgo src sexpr e2 e1 e2))]
              [(constant? (lambda (x) (eqv? x 0)) e2)
               (bind #t (e1) (zgo src sexpr e1 e1 e2))]
              [else (bind #t (e1 e2)
                      `(if ,(build-fixnums? (list e1 e2))
                           ,(%inline eq? ,e1 ,e2)
                           ,(build-libcall #t src sexpr = e1 e2)))])))
      (define-inline 2 =
        [(e1 e2) (go src sexpr e1 e2)]
        [(e1 . e*) #f])
      (define-inline 2 r6rs:=
        [(e1 e2) (go src sexpr e1 e2)]
        [(e1 e2 . e*) #f]))
    (let ()
      (define-syntax define-relop-inline
        (syntax-rules ()
          [(_ name r6rs:name relop op)
           (let ()
             (define builder
               (lambda (e1 e2 libcall)
                 (or (relop-length relop e1 e2)
                     (bind #t (e1 e2)
                       `(if ,(build-fixnums? (list e1 e2))
                            ,(%inline op ,e1 ,e2)
                            ,(libcall e1 e2))))))
             (define-inline 2 name
               [(e1 e2)
                (builder e1 e2
                  (lambda (e1 e2) (build-libcall #t src sexpr name e1 e2)))]
               ; TODO: handle 3-operand case w/3-operand library routine
               [(e1 . e*) #f])
             (define-inline 2 r6rs:name
               [(e1 e2)
                (builder e1 e2
                  (lambda (e1 e2) (build-libcall #t src sexpr name e1 e2)))]
               ; TODO: handle 3-operand case w/3-operand library routine
               [(e1 e2 . e*) #f]))]))
      (define-relop-inline < r6rs:< RELOP< <)
      (define-relop-inline <= r6rs:<= RELOP<= <=)
      (define-relop-inline >= r6rs:>= RELOP>= >=)
      (define-relop-inline > r6rs:> RELOP> >))
    (define-inline 3 positive?  ; 3 so opt-level 2 errors come from positive?
      [(e) (handle-prim src sexpr 3 '> (list e `(quote 0)))])
    (define-inline 3 nonnegative? ; 3 so opt-level 2 errors come from nonnegative?
      [(e) (handle-prim src sexpr 3 '>= (list e `(quote 0)))])
    (define-inline 3 negative?  ; 3 so opt-level 2 errors come from negative?
      [(e) (handle-prim src sexpr 3 '< (list e `(quote 0)))])
    (define-inline 3 nonpositive?  ; 3 so opt-level 2 errors come from nonpositive?
      [(e) (handle-prim src sexpr 3 '<= (list e `(quote 0)))])
    (define-inline 2 zero?
      [(e)
       (or (relop-length RELOP= e)
           (nanopass-case (L7 Expr) e
             [(call ,info ,mdcl ,pr ,e)
              (guard
                (eq? (primref-name pr) 'ftype-pointer-address)
                (all-set? (prim-mask unsafe) (primref-flags pr)))
              (make-ftype-pointer-null? e)]
             [else
               (bind #t (e)
                 (build-simple-or
                   (%inline eq? ,e (immediate ,(fix 0)))
                   `(if ,(%type-check mask-fixnum type-fixnum ,e)
                        ,(%constant sfalse)
                        ,(build-libcall #t src sexpr zero? e))))]))])
    (define-inline 2 positive? [(e) (relop-length RELOP> e)])
    (define-inline 2 nonnegative? [(e) (relop-length RELOP>= e)])
    (define-inline 2 negative? [(e) (relop-length RELOP< e)])
    (define-inline 2 nonpositive? [(e) (relop-length RELOP<= e)])
    (let ()
      (define-syntax  define-logorop-inline
        (syntax-rules ()
          [(_ name ...)
           (let ()
             (define build-logop
               (lambda (src sexpr e1 e2 libcall)
                 (bind #t (e1 e2)
                   (bind #t ([t (%inline logor ,e1 ,e2)])
                     `(if ,(%type-check mask-fixnum type-fixnum ,t)
                          ,t
                          ,(libcall src sexpr e1 e2))))))
             (let ()
               (define libcall (lambda (src sexpr e1 e2) (build-libcall #t src sexpr name e1 e2)))
               (define-inline 2 name
                 [() `(immediate ,(fix 0))]
                 [(e) (build-logop src sexpr e `(immediate ,(fix 0)) libcall)]
                 [(e1 e2) (build-logop src sexpr e1 e2 libcall)]
                 [(e1 . e*) #f]))
             ...)]))
      (define-logorop-inline logor logior bitwise-ior))
    (let ()
      (define-syntax  define-logop-inline
        (syntax-rules ()
          [(_ op unit name ...)
           (let ()
             (define build-logop
               (lambda (src sexpr e1 e2 libcall)
                 (bind #t (e1 e2)
                   `(if ,(build-fixnums? (list e1 e2))
                        ,(%inline op ,e1 ,e2)
                        ,(libcall src sexpr e1 e2)))))
             (let ()
               (define libcall (lambda (src sexpr e1 e2) (build-libcall #t src sexpr name e1 e2)))
               (define-inline 2 name
                 [() `(immediate ,(fix unit))]
                 [(e) (build-logop src sexpr e `(immediate ,(fix unit)) libcall)]
                 [(e1 e2) (build-logop src sexpr e1 e2 libcall)]
                 [(e1 . e*) #f]))
             ...)]))
      (define-logop-inline logand -1 logand bitwise-and)
      (define-logop-inline logxor 0 logxor bitwise-xor))
    (let ()
      (define build-lognot
        (lambda (e libcall)
          (bind #t (e)
            `(if ,(%type-check mask-fixnum type-fixnum ,e)
                 ,(%inline logxor ,e (immediate ,(fxlognot (constant mask-fixnum))))
                 ,(libcall e)))))

      (define-inline 2 lognot
        [(e) (build-lognot e (lambda (e) (build-libcall #t src sexpr lognot e)))])
      (define-inline 2 bitwise-not
        [(e) (build-lognot e (lambda (e) (build-libcall #t src sexpr bitwise-not e)))]))

    (let ()
      (define build-logbit?
        (lambda (e1 e2 libcall)
          (or (nanopass-case (L7 Expr) e1
                [(quote ,d)
                 (or (and (and (fixnum? d) (fx<= 0 d (fx- (constant fixnum-bits) 2)))
                          (bind #t (e2)
                            `(if ,(%type-check mask-fixnum type-fixnum ,e2)
                                 ,(%inline logtest ,e2 (immediate ,(fix (ash 1 d))))
                                 ,(libcall e1 e2))))
                     (and (and (target-fixnum? d) (> d (fx- (constant fixnum-bits) 2)))
                          (bind #t (e2)
                            `(if ,(%type-check mask-fixnum type-fixnum ,e2)
                                 ,(%inline < ,e2 (immediate ,(fix 0)))
                                 ,(libcall e1 e2)))))]
                [else #f])
              (bind #t (e1 e2)
                `(if ,(build-and
                        (build-fixnums? (list e1 e2))
                        (%inline u< ,e1 (immediate ,(fix (constant fixnum-bits)))))
                     ,(%inline logtest
                        ,(%inline sra ,e2 ,(build-unfix e1))
                        (immediate ,(fix 1)))
                     ,(libcall e1 e2))))))

      (define-inline 2 logbit?
        [(e1 e2) (build-logbit? e1 e2 (lambda (e1 e2) (build-libcall #t src sexpr logbit? e1 e2)))])
      (define-inline 2 bitwise-bit-set?
        [(e1 e2) (build-logbit? e2 e1 (lambda (e2 e1) (build-libcall #t src sexpr bitwise-bit-set? e1 e2)))]))

    (define-inline 2 logbit1
      [(e1 e2) (or (nanopass-case (L7 Expr) e1
                     [(quote ,d)
                      (and (and (fixnum? d) (fx<= 0 d (fx- (constant fixnum-bits) 2)))
                           (bind #t (e2)
                             `(if ,(%type-check mask-fixnum type-fixnum ,e2)
                                  ,(%inline logor ,e2 (immediate ,(fix (ash 1 d))))
                                  ,(build-libcall #t src sexpr logbit1 e1 e2))))]
                     [else #f])
                   (bind #t (e1 e2)
                     `(if ,(build-and
                             (build-fixnums? (list e1 e2))
                             (%inline u< ,e1 (immediate ,(fix (fx- (constant fixnum-bits) 1)))))
                          ,(%inline logor ,e2
                             ,(%inline sll (immediate ,(fix 1)) ,(build-unfix e1)))
                          ,(build-libcall #t src sexpr logbit1 e1 e2))))])
    (define-inline 2 logbit0
      [(e1 e2) (or (nanopass-case (L7 Expr) e1
                     [(quote ,d)
                      (and (and (fixnum? d) (fx<= 0 d (fx- (constant fixnum-bits) 2)))
                           (bind #t (e2)
                             `(if ,(%type-check mask-fixnum type-fixnum ,e2)
                                  ,(%inline logand ,e2 (immediate ,(fix (lognot (ash 1 d)))))
                                  ,(build-libcall #t src sexpr logbit0 e1 e2))))]
                     [else #f])
                   (bind #t (e1 e2)
                     `(if ,(build-and
                             (build-fixnums? (list e1 e2))
                             (%inline u< ,e1 (immediate ,(fix (fx- (constant fixnum-bits) 1)))))
                          ,(%inline logand ,e2
                            ,(%inline lognot
                              ,(%inline sll (immediate ,(fix 1)) ,(build-unfix e1))))
                          ,(build-libcall #t src sexpr logbit0 e1 e2))))])
    (define-inline 2 logtest
      [(e1 e2) (bind #t (e1 e2)
                 `(if ,(build-fixnums? (list e1 e2))
                      ,(%inline logtest ,e1 ,e2)
                      ,(build-libcall #t src sexpr logtest e1 e2)))])
    (define-inline 3 $flhash
      [(e) (bind #t (e)
             `(if ,(build-fl= e e)
                 ,(%inline logand
                   ,(%inline srl
                     ,(constant-case ptr-bits
                        [(32) (%inline +
                                 ,(%mref ,e ,(constant flonum-data-disp))
                                 ,(%mref ,e ,(fx+ (constant flonum-data-disp) 4)))]
                        [(64) (%mref ,e ,(constant flonum-data-disp))])
                     (immediate 1))
                   (immediate ,(- (constant fixnum-factor))))
               ;; +nan.0
               (immediate ,(fix #xfa1e))))])
    (let ()
      (define build-flonum-extractor
        (lambda (pos size e1)
          (let ([cnt (- pos (constant fixnum-offset))]
                [mask (* (- (expt 2 size) 1) (expt 2 (constant fixnum-offset)))])
            (%inline logand
              ,(let ([body (constant-case native-endianness
                             [(unknown)
                              (constant-case ptr-bits
                                [(64)
                                 (%inline srl ,(%mref ,e1 ,(constant flonum-data-disp)) (immediate 32))]
                                [(32)
                                 (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/hi ,e)])]
                             [else
                              `(inline ,(make-info-load 'integer-32 #f) ,%load ,e1 ,%zero
                                       (immediate ,(constant-case native-endianness
                                                     [(little) (fx+ (constant flonum-data-disp) 4)]
                                                     [(big) (constant flonum-data-disp)])))])])
                 (let ([body (if (fx> cnt 0)
                                 (%inline srl ,body (immediate ,cnt))
                                 body)])
                   (if (fx< cnt 0)
                       (%inline sll ,body (immediate ,(fx- 0 cnt)))
                       body)))
              (immediate ,mask)))))

      (define-inline 3 fllp
        [(e) (build-flonum-extractor 19 12 e)])

      (define-inline 3 $flonum-sign
        [(e) (build-flonum-extractor 31 1 e)])

      (define-inline 3 $flonum-exponent
        [(e) (build-flonum-extractor 20 11 e)]))

    (let ()
      (define build-flonum-bit-field-to-fixnum
        (lambda (e start size)
          (safe-assert (< size (constant fixnum-bits)))
          (let ([mask (* (- (expt 2 size) 1) (expt 2 (constant fixnum-offset)))])
            (define (finish n start)
              (%inline logand
                       ,(cond
                          [(= start (constant fixnum-offset))
                           n]
                          [(< start (constant fixnum-offset))
                           (%inline sll ,n (immediate ,(fx- (constant fixnum-offset) start)))]
                          [else
                           (%inline srl ,n (immediate ,(fx- start (constant fixnum-offset))))])
                       (immediate ,mask)))
            (constant-case ptr-bits
              [(64)
               (finish `(inline ,(make-info-unboxed-args '(#t)) ,%fpcastto ,e) start)]
              [(32)
               (cond
                 [(<= (+ start size) 32)
                  (finish `(inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/lo ,e) start)]
                 [(>= start 32)
                  (finish `(inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/hi ,e) (fx- start 32))]
                 [else
                  (finish (%inline logor
                                   ,(%inline sll (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/hi ,e)
                                             (immediate ,(fx- (fx+ 32 (constant fixnum-offset)) start)))
                                   ,(%inline srl (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/lo ,e)
                                             (immediate ,(fx- start (constant fixnum-offset)))))
                          (constant fixnum-offset))])]))))
      (define (maybe-build-flonum-bit-field-to-fixnum e start end unsafe? src sexpr)
        (and (constant? fixnum? start) (constant? fixnum? end)
             (let ([start (constant-value start)] [end (constant-value end)])
               (and (fx<= 0 start end (constant flonum-bits))
                    (fx<= (fx- end start) (fx- (constant fixnum-bits) 1))
                    (let ()
                      (define (extract e)
                        (if (fx= end start)
                            (%seq ,e (immediate ,(fix 0)))
                            (build-flonum-bit-field-to-fixnum e start (fx- end start))))
                      (if (or unsafe?
                              (known-flonum-result? e))
                          (bind #t fp (e)
                                (extract e))
                          (bind #t (e)
                                `(if ,(%type-check mask-flonum type-flonum ,e)
                                     ,(extract e)
                                     ,(build-libcall #t src sexpr flbit-field e `(quote ,start) `(quote ,end))))))))))
      (define-inline 3 flbit-field
        [(e start end) (maybe-build-flonum-bit-field-to-fixnum e start end #t src sexpr)])
      (define-inline 2 flbit-field
        [(e start end) (maybe-build-flonum-bit-field-to-fixnum e start end #f src sexpr)]))

    (define-inline 3 $fleqv?
      [(e1 e2)
       (bind #t (e1 e2)
         `(if ,(build-fl= e1 e1) ; check e1 not +nan.0
              ,(constant-case ptr-bits
                [(32)
                 (bind #t (e1 e2) ;; could omit raw due to bind, but that would look amiss beside 64-bit case
                   (build-and
                          (%inline eq?
                           (raw ,(%mref ,e1 ,(constant flonum-data-disp)))
                           (raw ,(%mref ,e2 ,(constant flonum-data-disp))))
                          (%inline eq?
                            (raw ,(%mref ,e1 ,(fx+ (constant flonum-data-disp) 4)))
                            (raw ,(%mref ,e2 ,(fx+ (constant flonum-data-disp) 4))))))]
                [(64) (%inline eq?
                        (raw ,(%mref ,e1 ,(constant flonum-data-disp)))
                        (raw ,(%mref ,e2 ,(constant flonum-data-disp))))]
                [else ($oops 'compiler-internal
                             "$fleqv doesn't handle ptr-bits = ~s"
                             (constant ptr-bits))])
              ;; If e1 is +nan.0, see if e2 is +nan.0:
              ,(build-not (build-fl= e2 e2))))])

    (let ()
      (define build-fp-op-1
        (lambda (op e)
          (bind #f fp (e)
            (if (procedure? op) (op e) `(unboxed-fp (inline ,(make-info-unboxed-args '(#t)) ,op ,e))))))
      (define build-fp-op-2
        (lambda (op e1 e2)
          (bind #f fp (e1 e2)
            (if (procedure? op) (op e1 e2) `(unboxed-fp (inline ,(make-info-unboxed-args '(#t #t)) ,op ,e1 ,e2))))))
      (define build-fl-adjust-sign
        (lambda (e combine base)
          `(unboxed-fp
            ,(constant-case ptr-bits
               [(64)
                (let ([t (make-tmp 'flsgn)])
                  `(let ([,t (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto ,e)])
                     (inline ,null-info ,%fpcastfrom (inline ,null-info ,combine ,t ,base))))]
               [(32)
                (let ([thi (make-tmp 'flsgnh)]
                      [tlo (make-tmp 'flsgnl)])
                  (bind #t fp (e)
                    `(let ([,thi (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/hi ,e)]
                           [,tlo (inline ,(make-info-unboxed-args '(#t)) ,%fpcastto/lo ,e)])
                       (inline ,null-info ,%fpcastfrom (inline ,null-info ,combine ,thi ,base) ,tlo))))]))))
      (define build-flabs
        (lambda (e)
          (build-fl-adjust-sign e %logand (%inline srl (immediate -1) (immediate 1)))))
      (define build-flneg
        (lambda (e)
          (build-fl-adjust-sign e %logxor (%inline sll (immediate -1) (immediate ,(fx- (constant ptr-bits) 1))))))
      (define build-fl-call
        (lambda (entry . e*)
          `(foreign-call ,(with-output-language (Ltype Type)
                            (make-info-foreign '(atomic) (map (lambda (e) `(fp-double-float)) e*) `(fp-double-float) #t))
                         (literal ,(make-info-literal #f 'entry entry 0))
                         ,e* ...)))
      (define build-flminmax
        (lambda (min?)
          (lambda (e1 e2)
            (bind 'always fp (e1 e2)
                  `(if (inline ,(make-info-unboxed-args '(#t #t)) ,%fp< ,e1 ,e2)
                       (unboxed-fp ,(if min? e1 e2))
                       (if (inline ,(make-info-unboxed-args '(#t #t)) ,%fp<= ,e2 ,e1)
                           (unboxed-fp ,(if min? e2 e1))
                           ;; one of them must be +nan.0, so ensure +nan.0 result
                           (unboxed-fp (inline ,(make-info-unboxed-args '(#t #t)) ,%fp+ ,e1 ,e2))))))))

      (define-inline 3 fl+
        [() `(quote 0.0)]
        [(e) (ensure-single-valued e)]
        [(e1 e2) (build-fp-op-2 %fp+ e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'fl+ e1 e*)])
         
      (define-inline 3 fl*
        [() `(quote 1.0)]
        [(e) (ensure-single-valued e)]
        [(e1 e2) (build-fp-op-2 %fp* e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'fl* e1 e*)])

      (define-inline 3 fl-
        [(e) (build-flneg e)]
        [(e1 e2) (build-fp-op-2 %fp- e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'fl- e1 e*)])

      (define-inline 3 fl/
        [(e) (build-fp-op-2 %fp/ `(quote 1.0) e)]
        [(e1 e2) (build-fp-op-2 %fp/ e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'fl/ e1 e*)])

      (define-inline 3 flmin
        [(e) (ensure-single-valued e)]
        [(e1 e2) ((build-flminmax #t) e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'flmin e1 e*)])

      (define-inline 3 flmax
        [(e) (ensure-single-valued e)]
        [(e1 e2) ((build-flminmax #f) e1 e2)]
        [(e1 . e*) (reduce-fp src sexpr 3 'flmax e1 e*)])

      (define-inline 3 flsqrt
        [(e)
         (constant-case architecture
           [(x86 x86_64 arm32 arm64 riscv64 loongarch64 pb) (build-fp-op-1 %fpsqrt e)]
           [(ppc32) (build-fl-call (lookup-c-entry flsqrt) e)])])

      (define-inline 3 flsingle
        [(e) (build-fp-op-1 %fpsingle e)])

      (define-inline 3 flabs
        [(e) (build-flabs e)])

      (let ()
        (define-syntax define-fl-call
          (syntax-rules ()
            [(_ id extra ...)
             (define-inline 3 id
               [(e) (build-fl-call (lookup-c-entry id) e)]
               extra ...)]))
        (define-syntax define-fl2-call
          (syntax-rules ()
            [(_ id id2)
             (define-fl-call id
               [(e1 e2) (build-fl-call (lookup-c-entry id2) e1 e2)])]))
        (define-fl-call flround) ; no support in SSE2 for flround, though this was added in SSE4.1
        (define-fl-call flfloor)
        (define-fl-call flceiling)
        (define-fl-call fltruncate)
        (define-fl-call flsin)
        (define-fl-call flcos)
        (define-fl-call fltan)
        (define-fl-call flasin)
        (define-fl-call flacos)
        (define-fl2-call flatan flatan2)
        (define-fl-call flexp)
        (define-fl2-call fllog fllog2))

      (define-inline 3 flexpt
        [(e1 e2) (build-fl-call (lookup-c-entry flexpt) e1 e2)])
      
      (let ()
        (define build-fl-make-rectangular
          (lambda (e1 e2)
            (bind #f (e1 e2)
              (bind #t ([t (%constant-alloc type-typed-object (constant size-inexactnum))])
                (%seq
                   (set! ,(%mref ,t ,(constant inexactnum-type-disp))
                     ,(%constant type-inexactnum))
                   (set! ,(%mref ,t ,%zero ,(constant inexactnum-real-disp) fp)
                         ,(%mref ,e1 ,%zero ,(constant flonum-data-disp) fp))
                   (set! ,(%mref ,t ,%zero ,(constant inexactnum-imag-disp) fp)
                         ,(%mref ,e2 ,%zero ,(constant flonum-data-disp) fp))
                   ,t)))))

        (define-inline 3 fl-make-rectangular
          [(e1 e2) (build-fl-make-rectangular e1 e2)])

        (define-inline 3 cfl-
          [(e) (bind #t (e)
                 `(if ,(%type-check mask-flonum type-flonum ,e)
                      ,(build-flneg e)
                      ,(build-fl-make-rectangular
                         (build-flneg (build-$inexactnum-real-part e))
                         (build-flneg (build-$inexactnum-imag-part e)))))]
          [(e1 e2) (build-libcall #f src sexpr cfl- e1 e2)]
          ; TODO: add 3 argument version of cfl- library function
          #;[(e1 e2 e3) (build-libcall #f src sexpr cfl- e1 e2 e3)]
          [(e1 e2 . e*) #f])

        (define-inline 3 cfl+
          [() `(quote 0.0)]
          [(e) (ensure-single-valued e)]
          [(e1 e2) (build-libcall #f src sexpr cfl+ e1 e2)]
          ; TODO: add 3 argument version of cfl+ library function
          #;[(e1 e2 e3) (build-libcall #f src sexpr cfl+ e1 e2 e3)]
          [(e1 e2 . e*) #f])

        (define-inline 3 cfl*
          [() `(quote 1.0)]
          [(e) (ensure-single-valued e)]
          [(e1 e2) (build-libcall #f src sexpr cfl* e1 e2)]
          ; TODO: add 3 argument version of cfl* library function
          #;[(e1 e2 e3) (build-libcall #f src sexpr cfl* e1 e2 e3)]
          [(e1 e2 . e*) #f])

        (define-inline 3 cfl/
          [(e) (build-libcall #f src sexpr cfl/ `(quote 1.0) e)]
          [(e1 e2) (build-libcall #f src sexpr cfl/ e1 e2)]
          ; TODO: add 3 argument version of cfl/ library function
          #;[(e1 e2 e3) (build-libcall #f src sexpr cfl/ e1 e2 e3)]
          [(e1 e2 . e*) #f])

        (define-inline 3 cfl-conjugate
          [(e) (bind #t (e)
                 `(if ,(%type-check mask-flonum type-flonum ,e)
                      ,e
                      ,(build-fl-make-rectangular
                         (build-$inexactnum-real-part e)
                         (build-flneg (build-$inexactnum-imag-part e)))))]))

      (define-inline 3 $make-exactnum
        [(e1 e2) (bind #f (e1 e2)
                   (bind #t ([t (%constant-alloc type-typed-object (constant size-exactnum))])
                     (%seq
                       (set! ,(%mref ,t ,(constant exactnum-type-disp))
                         ,(%constant type-exactnum))
                       (set! ,(%mref ,t ,(constant exactnum-real-disp)) ,e1)
                       (set! ,(%mref ,t ,(constant exactnum-imag-disp)) ,e2)
                       ,t)))])

      (let ()
        (define (build-fl< e1 e2) `(inline ,(make-info-unboxed-args '(#t #t)) ,%fp< ,e1 ,e2))
        (define build-fl=
          (case-lambda
           [(e) (if (constant nan-single-comparison-true?)
                    (%seq ,e (quote #t))
                    (bind #t fp (e) (build-fl= e e)))]
           [(e1 e2) (bind #f fp (e1 e2)
                      `(inline ,(make-info-unboxed-args '(#t #t)) ,%fp= ,e1 ,e2))]))
        (define (build-fl<= e1 e2) `(inline ,(make-info-unboxed-args '(#t #t)) ,%fp<= ,e1 ,e2))

        (let ()
          (define-syntax define-fl-cmp-inline
            (lambda (x)
              (syntax-case x ()
                [(_ op r6rs:op builder inequality? swapped?)
                 (with-syntax ([(args ...) (if (datum swapped?) #'(e2 e1) #'(e1 e2))]
                               [reducer (if (datum inequality?)
                                            #'(reduce-fp-compare reduce-inequality)
                                            #'(reduce-fp-compare reduce-equality))])
                   #'(begin
                       (define-inline 3 op
                         [(e) (build-fl= e)]
                         [(e1 e2) (builder args ...)]
                         [(e1 e2 . e*) (reducer src sexpr moi e1 e2 e*)])
                       (define-inline 3 r6rs:op
                         [(e1 e2) (builder args ...)]
                         [(e1 e2 . e*) (reducer src sexpr moi e1 e2 e*)])))])))

          (define-fl-cmp-inline fl= fl=? build-fl= #f #f)
          (define-fl-cmp-inline fl< fl<? build-fl< #t #f)
          (define-fl-cmp-inline fl> fl>? build-fl< #t #t)
          (define-fl-cmp-inline fl<= fl<=? build-fl<= #t #f)
          (define-fl-cmp-inline fl>= fl>=? build-fl<= #t #t))
        (let ()
          (define-syntax build-bind-and-check
            (syntax-rules ()
              [(_ src sexpr op e1 e2 body)
               (if (known-flonum-result? e1)
                   (if (known-flonum-result? e2)
                       body
                       (bind #t (e2)
                         `(if ,(%type-check mask-flonum type-flonum ,e2)
                              ,body
                              ,(build-libcall #t src sexpr op e2 e2))))
                   (if (known-flonum-result? e2)
                       (bind #t (e1)
                         `(if ,(%type-check mask-flonum type-flonum ,e1)
                              ,body
                              ,(build-libcall #t src sexpr op e1 e1)))
                       (bind #t (e1 e2)
                         `(if ,(build-and
                                (%type-check mask-flonum type-flonum ,e1)
                                (%type-check mask-flonum type-flonum ,e2))
                              ,body
                              ,(build-libcall #t src sexpr op e1 e2)))))]))
          (define build-check-fp-arguments
            (lambda (e* build-libcall k)
              (let loop ([e* e*] [check-e* '()] [all-e* '()])
                (cond
                  [(null? e*)
                   (let loop ([check-e* (reverse check-e*)])
                     (cond
                       [(null? check-e*) (apply k (reverse all-e*))]
                       [(null? (cdr check-e*))
                        (let ([e1 (car check-e*)])
                          `(if ,(%type-check mask-flonum type-flonum ,e1)
                               ,(loop '())
                               ,(build-libcall e1 e1)))]
                       [else
                        (let ([e1 (car check-e*)]
                              [e2 (cadr check-e*)])
                          `(if ,(build-and
                                 (%type-check mask-flonum type-flonum ,e1)
                                 (%type-check mask-flonum type-flonum ,e2))
                               ,(loop (cddr check-e*))
                               ,(build-libcall e1 e2)))]))]
                  [else
                   (let ([e1 (car e*)])
                     (if (known-flonum-result? e1)
                         (loop (cdr e*) check-e* (cons e1 all-e*))
                         (bind #t (e1)
                           (loop (cdr e*) (cons e1 check-e*) (cons e1 all-e*)))))]))))
          (define-syntax define-fl-cmp-inline
            (lambda (x)
              (syntax-case x ()
                [(_ op r6rs:op builder inequality? swapped?)
                 (with-syntax ([(args ...) (if (datum swapped?) #'(e2 e1) #'(e1 e2))]
                               [reducer (if (datum inequality?)
                                            #'(reduce-fp-compare reduce-inequality)
                                            #'(reduce-fp-compare reduce-equality))])
                   #'(begin
                       (define-inline 2 op
                         [(e1) (if (known-flonum-result? e1)
                                   (build-fl= e1)
                                   (bind #t (e1)
                                     `(if ,(%type-check mask-flonum type-flonum ,e1)
                                          ,(build-fl= e1)
                                          ,(build-libcall #t src sexpr op e1 e1))))]
                         [(e1 e2) (build-bind-and-check src sexpr op e1 e2 (builder args ...))]
                         [(e1 e2 . e*) (and
                                        (fx<= (length e*) (fx- inline-args-limit 2))
                                        (build-check-fp-arguments (cons* e1 e2 e*)
                                          (lambda (e1 e2) (build-libcall #t src sexpr op e1 e2))
                                          (lambda (e1 e2 . e*) (reducer src sexpr moi e1 e2 e*))))])
                       (define-inline 2 r6rs:op
                         [(e1 e2) (build-bind-and-check src sexpr r6rs:op e1 e2 (builder args ...))]
                         [(e1 e2 . e*) (and
                                        (fx<= (length e*) (fx- inline-args-limit 2))
                                        (build-check-fp-arguments (cons* e1 e2 e*)
                                          (lambda (e1 e2) (build-libcall #t src sexpr r6rs:op e1 e2))
                                          (lambda (e1 e2 . e*) (reducer src sexpr moi e1 e2 e*))))])))])))

          (define-fl-cmp-inline fl= fl=? build-fl= #f #f)
          (define-fl-cmp-inline fl< fl<? build-fl< #t #f)
          (define-fl-cmp-inline fl> fl>? build-fl< #t #t)
          (define-fl-cmp-inline fl<= fl<=? build-fl<= #t #f)
          (define-fl-cmp-inline fl>= fl>=? build-fl<= #t #t))
        (let ()
          (define build-cfl=
            ; NB: e1 and e2 must be bound
            (lambda (e1 e2)
              `(if ,(%type-check mask-flonum type-flonum ,e1)
                   (if ,(%type-check mask-flonum type-flonum ,e2)
                       ,(build-fl= e1 e2)
                       ,(build-and
                          (build-fl= `(quote 0.0) (build-$inexactnum-imag-part e2))
                          (build-fl= e1 (build-$inexactnum-real-part e2))))
                   (if ,(%type-check mask-flonum type-flonum ,e2)
                       ,(build-and
                          (build-fl= `(quote 0.0) (build-$inexactnum-imag-part e1))
                          (build-fl= e2 (build-$inexactnum-real-part e1)))
                       ,(build-and
                          (build-fl=
                            (build-$inexactnum-imag-part e1)
                            (build-$inexactnum-imag-part e2))
                          (build-fl=
                            (build-$inexactnum-real-part e1)
                            (build-$inexactnum-real-part e2)))))))
          (define-inline 3 cfl=
            [(e) (if (constant nan-single-comparison-true?)
                     (%seq ,e (quote #t))
                     (bind #f (e) (build-cfl= e e)))]
            [(e1 e2) (bind #f (e1 e2) (build-cfl= e1 e2))]
            ; TODO: should we avoid building for more then the 3 item case?
            [(e1 e2 . e*) (reduce-equality src sexpr moi e1 e2 e*)])))

      (let ()
        (define build-checked-fp-op
          (case-lambda
            [(e k)
             (if (known-flonum-result? e)
                 e
                 (bind #t (e)
                   `(if ,(build-flonums? (list e))
                        ,e
                        ,(k e))))]
            [(e1 op k) ; `op` can be a procedure that produces an unboxed value
             (if (known-flonum-result? e1)
                 (build-fp-op-1 op e1)
                 (bind #t (e1)
                   (let ([e (build-fp-op-1 op e1)]
                         [k (lambda (e)
                              `(if ,(build-flonums? (list e1))
                                   ,e
                                   ,(k e1)))])
                     ((lift-fp-unboxed k) e))))]
            [(e1 e2 op k) ; `op` can be a procedure that produces an unboxed value
             ;; uses result of `e1` or `e2` twice for error if other is always a flonum
             (let ([build (lambda (e1 e2)
                            (build-fp-op-2 op e1 e2))])
               (if (known-flonum-result? e1)
                   (if (known-flonum-result? e2)
                       (build e1 e2)
                       (bind #t (e2)
                         (build e1 `(if ,(build-flonums? (list e2))
                                        ,e2
                                        ,(k e2 e2)))))
                   (if (known-flonum-result? e2)
                       (bind #t (e1)
                         (build `(if ,(build-flonums? (list e1))
                                     ,e1
                                     ,(k e1 e1))
                                e2))
                       (bind #t (e1 e2)
                         (let ([e (build e1 e2)]
                               [k (lambda (e)
                                    `(if ,(build-flonums? (list e1 e2))
                                         ,e
                                         ,(k e1 e2)))])
                           ((lift-fp-unboxed k) e))))))]))

        (define-inline 2 fl+
          [() `(quote 0.0)]
          [(e) (build-checked-fp-op e
                 (lambda (e)
                   (build-libcall #t src sexpr fl+ e `(quote 0.0))))]
          [(e1 e2) (build-checked-fp-op e1 e2 %fp+
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr fl+ e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'fl+ e1 e*)])

        (define-inline 2 fl*
          [() `(quote 1.0)]
          [(e) (build-checked-fp-op e
                 (lambda (e)
                   (build-libcall #t src sexpr fl* e `(quote 1.0))))]
          [(e1 e2) (build-checked-fp-op e1 e2 %fp*
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr fl* e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'fl* e1 e*)])

        (define-inline 2 fl-
          [(e) (build-checked-fp-op e build-flneg
                 (lambda (e)
                   (build-libcall #t src sexpr flnegate e)))]
          [(e1 e2) (build-checked-fp-op e1 e2 %fp-
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr fl- e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'fl- e1 e*)])

        (define-inline 2 fl/
          [(e) (build-checked-fp-op `(quote 1.0) e %fp/
                 (lambda (e1 e2)
                   (build-libcall #t src sexpr fl/ e1 e2)))]
          [(e1 e2) (build-checked-fp-op e1 e2 %fp/
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr fl/ e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'fl/ e1 e*)])

        (define-inline 2 flmin
          [(e) (build-checked-fp-op e
                 (lambda (e)
                   (build-libcall #t src sexpr flmin e `(quote 0.0))))]
          [(e1 e2) (build-checked-fp-op e1 e2 (build-flminmax #t)
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr flmin e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'flmin e1 e*)])

        (define-inline 2 flmax
          [(e) (build-checked-fp-op e
                 (lambda (e)
                   (build-libcall #t src sexpr flmax e `(quote 0.0))))]
          [(e1 e2) (build-checked-fp-op e1 e2 (build-flminmax #f)
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr flmax e1 e2)))]
          [(e1 . e*) (reduce-fp src sexpr 2 'flmax e1 e*)])

      (define-inline 2 flabs
        [(e) (build-checked-fp-op e build-flabs
               (lambda (e)
                 (build-libcall #t src sexpr flabs e)))])

      (define-inline 2 flsqrt
        [(e)
         (build-checked-fp-op e
           (lambda (e)
             (constant-case architecture
               [(x86 x86_64 arm32 arm64 riscv64 loongarch64 pb) (build-fp-op-1 %fpsqrt e)]
               [(ppc32) (build-fl-call (lookup-c-entry flsqrt) e)]))
           (lambda (e)
             (build-libcall #t src sexpr flsqrt e)))])

      (define-inline 2 flsingle
        [(e)
         (build-checked-fp-op e
           (lambda (e) (build-fp-op-1 %fpsingle e))
           (lambda (e)
             (build-libcall #t src sexpr flsingle e)))])

      (let ()
        (define-syntax define-fl-call
          (syntax-rules ()
            [(_ id)
             (define-inline 2 id
               [(e) (build-checked-fp-op e (lambda (e) (build-fl-call (lookup-c-entry id) e))
                      (lambda (e)
                        (build-libcall #t src sexpr id e)))])]))
        (define-syntax define-fl2-call
          (syntax-rules ()
            [(_ id id2)
             (define-inline 2 id
               [(e) (build-checked-fp-op e (lambda (e) (build-fl-call (lookup-c-entry id) e))
                      (lambda (e)
                        (build-libcall #t src sexpr id e)))]
               [(e1 e2) (build-checked-fp-op e1 e2 (lambda (e1 e2) (build-fl-call (lookup-c-entry id2) e1 e2))
                          (lambda (e1 e2)
                            (build-libcall #t src sexpr id2 e1 e2)))])]))
        (define-fl-call flround)
        (define-fl-call flfloor)
        (define-fl-call flceiling)
        (define-fl-call fltruncate)
        (define-fl-call flsin)
        (define-fl-call flcos)
        (define-fl-call fltan)
        (define-fl-call flasin)
        (define-fl-call flacos)
        (define-fl2-call flatan flatan2)
        (define-fl-call flexp)
        (define-fl2-call fllog fllog2))
      
      (define-inline 2 flexpt
        [(e1 e2) (build-checked-fp-op e1 e2
                   (lambda (e1 e2) (build-fl-call (lookup-c-entry flexpt) e1 e2))
                   (lambda (e1 e2)
                     (build-libcall #t src sexpr flexpt e1 e2)))])

      ;; NB: assuming that we have a trunc instruction for now, will need to change to support Sparc
      (define-inline 3 flonum->fixnum
        [(e-x) (bind #f fp (e-x)
                 (build-fix
                  `(inline ,(make-info-unboxed-args '(#t)) ,%fptrunc ,e-x)))])
      (define-inline 2 flonum->fixnum
        [(e-x) (build-checked-fp-op e-x
                 (lambda (e-x)
                   (define (build-fl< e1 e2) `(inline ,(make-info-unboxed-args '(#t #t)) ,%fp< ,e1 ,e2))
                   (bind #t (e-x)
                     `(if ,(build-and
                            (build-fl< e-x `(quote ,(constant too-positive-flonum-for-fixnum)))
                            (build-fl< `(quote ,(constant too-negative-flonum-for-fixnum)) e-x))
                          ,(build-fix
                            `(inline ,(make-info-unboxed-args '(#t)) ,%fptrunc ,e-x))
                          ;; We have to box the flonum to report an error:
                          ,(let ([t (make-tmp 't)])
                             `(let ([,t ,(%constant-alloc type-flonum (constant size-flonum))])
                                (seq
                                 (set! ,(%mref ,t ,%zero ,(constant flonum-data-disp) fp) ,e-x)
                                 ,(build-libcall #t src sexpr flonum->fixnum t)))))))
                 (lambda (e-x)
                   (build-libcall #t src sexpr flonum->fixnum e-x)))])))

    (let ()
      (define build-fixnum->flonum
       ; NB: x must already be bound in order to ensure it is done before the flonum is allocated
        (lambda (e-x k)
          (k `(unboxed-fp ,(%inline fpt ,(build-unfix e-x))))))
      (define-inline 3 fixnum->flonum
        [(e-x) (bind #f (e-x) (build-fixnum->flonum e-x values))])
      (define-inline 2 fixnum->flonum
        [(e-x) (bind #t (e-x)
                 (build-fixnum->flonum e-x
                   (lift-fp-unboxed
                     (lambda (e)
                       `(if ,(%type-check mask-fixnum type-fixnum ,e-x)
                            ,e
                            ,(build-libcall #t src sexpr fixnum->flonum e-x))))))])
      (define-inline 2 real->flonum
        [(e-x)
         (if (known-flonum-result? e-x)
             e-x
             (bind #t (e-x)
               `(if ,(%type-check mask-fixnum type-fixnum ,e-x)
                    ,(build-fixnum->flonum e-x values)
                    (if ,(%type-check mask-flonum type-flonum ,e-x)
                        ,e-x
                        ,(build-libcall #t src sexpr real->flonum e-x `(quote real->flonum))))))]))
    (define-inline 3 $real->flonum
      [(x who) (build-$real->flonum src sexpr x who)])
    (define-inline 2 $record
      [(tag . args) (build-$record tag args)])
    (define-inline 3 $object-address
      [(e-ptr e-offset)
       (unsigned->ptr
         (%inline + ,e-ptr ,(build-unfix e-offset))
         (type->width ptr-type))])
    (define-inline 3 $address->object
      [(e-addr e-roffset)
       (bind #f (e-roffset)
         (%inline -
            ,(ptr->integer e-addr (type->width ptr-type))
            ,(build-unfix e-roffset)))])
    (define-inline 3 object->reference-address
      [(e-ptr) (bind #t (e-ptr)
                 `(if ,(%inline eq? ,e-ptr (immediate ,(constant sfalse)))
                      (immediate 0)
                      ,(unsigned->ptr (%inline + ,e-ptr ,(%constant reference-disp)) (type->width ptr-type))))])
    (define-inline 3 reference-address->object
      [(e-ptr) (bind #t (e-ptr)
                 `(if ,(%inline eq? ,e-ptr (immediate 0))
                      (immediate ,(constant sfalse))
                      ,(%inline - ,(ptr->integer e-ptr (type->width ptr-type)) ,(%constant reference-disp))))])
    (define-inline 2 $object-ref
      [(type base offset)
       (nanopass-case (L7 Expr) type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (build-object-ref #f type base offset)))]
         [else #f])])
    (define-inline 2 $swap-object-ref
      [(type base offset)
       (nanopass-case (L7 Expr) type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (build-object-ref #t type base offset)))]
         [else #f])])
    (define-inline 3 foreign-ref
      [(e-type e-addr e-offset)
       (nanopass-case (L7 Expr) e-type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (bind #f (e-offset)
                   (build-object-ref #f type
                     (ptr->integer e-addr (constant ptr-bits))
                     e-offset))))]
         [else #f])])
    (define-inline 3 $foreign-swap-ref
      [(e-type e-addr e-offset)
       (nanopass-case (L7 Expr) e-type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (bind #f (e-offset)
                   (build-object-ref #t type
                     (ptr->integer e-addr (constant ptr-bits))
                     e-offset))))]
         [else #f])])
    (define-inline 2 $object-set!
      [(type base offset value)
       (nanopass-case (L7 Expr) type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (or (>= (constant ptr-bits) (type->width type)) (eq? type 'double-float))
                 (build-object-set! type base offset value)))]
         [else #f])])
    (define-inline 3 foreign-set!
      [(e-type e-addr e-offset e-value)
       (nanopass-case (L7 Expr) e-type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool)))
                 (or (>= (constant ptr-bits) (type->width type)) (eq? type 'double-float))
                 (bind #f (e-offset e-value)
                   (build-object-set! type
                     (ptr->integer e-addr (constant ptr-bits))
                     e-offset
                     e-value))))]
         [else #f])])
    (define-inline 3 $foreign-swap-set!
      [(e-type e-addr e-offset e-value)
       (nanopass-case (L7 Expr) e-type
         [(quote ,d)
          (let ([type (filter-foreign-type d)])
            (and (memq type (record-datatype list))
                 (not (memq type '(char wchar boolean stdbool single-float)))
                 (>= (constant ptr-bits) (type->width type))
                 (bind #f (e-offset e-value)
                   (build-swap-object-set! type
                     (ptr->integer e-addr (constant ptr-bits))
                     e-offset
                     e-value))))]
         [else #f])])
    (define-inline 2 $make-fptr
      [(e-ftype e-addr)
       (nanopass-case (L7 Expr) e-addr
         [(call ,info ,mdcl ,pr ,e1)
          (guard
            (eq? (primref-name pr) 'ftype-pointer-address)
            (all-set? (prim-mask unsafe) (primref-flags pr)))
          (bind #f (e-ftype e1)
            (bind #t ([t (%constant-alloc type-typed-object (fx* 2 (constant ptr-bytes)))])
              (%seq
                (set! ,(%mref ,t ,(constant record-type-disp)) ,e-ftype)
                (set! ,(%mref ,t ,(constant record-data-disp))
                  ,(%mref ,e1 ,(constant record-data-disp)))
                ,t)))]
         [else
          (bind #f (e-ftype e-addr)
            (bind #t ([t (%constant-alloc type-typed-object (fx* 2 (constant ptr-bytes)))])
              (%seq
                (set! ,(%mref ,t ,(constant record-type-disp)) ,e-ftype)
                (set! ,(%mref ,t ,(constant record-data-disp))
                  ,(ptr->integer e-addr (constant ptr-bits)))
                ,t)))])])
    (define-inline 3 ftype-pointer-address
      [(e-fptr)
       (build-object-ref #f
         (constant-case ptr-bits
           [(64) 'unsigned-64]
           [(32) 'unsigned-32])
         e-fptr %zero (constant record-data-disp))])
    (define-inline 3 ftype-pointer-null?
      [(e-fptr) (make-ftype-pointer-null? e-fptr)])
    (define-inline 3 ftype-pointer=?
      [(e1 e2) (make-ftype-pointer-equal? e1 e2)])
    (let ()
      (define build-fx+raw
        (lambda (fx-arg raw-arg)
          (if (constant? (lambda (x) (eqv? x 0)) fx-arg)
              raw-arg ;; already marked raw
              `(raw ,(%inline + ,raw-arg ,(build-unfix fx-arg))))))
      (define $extract-fptr-address
        (lambda (e-fptr)
          (define suppress-unsafe-cast
            (lambda (e-fptr)
              (nanopass-case (L7 Expr) e-fptr
                [(call ,info1 ,mdcl1 ,pr1 (quote ,d) (call ,info2 ,mdcl2 ,pr2 ,e))
                 (guard
                   (eq? (primref-name pr1) '$make-fptr)
                   (all-set? (prim-mask unsafe) (primref-flags pr2))
                   (eq? (primref-name pr2) 'ftype-pointer-address)
                   (all-set? (prim-mask unsafe) (primref-flags pr2)))
                  e]
                [else e-fptr])))
          (nanopass-case (L7 Expr) e-fptr
            ; skip allocation and dereference of ftype-pointer for $fptr-fptr-ref
            [(call ,info ,mdcl ,pr ,e1 ,e2 ,e3) ; e1, e2, e3 = fptr, offset, ftd
             (guard
               (eq? (primref-name pr) '$fptr-fptr-ref)
               (all-set? (prim-mask unsafe) (primref-flags pr)))
             (let-values ([(e-index imm-offset) (offset-expr->index+offset e2)])
               (bind #f (e-index e3) ;; evaluate e3 for effect
                 `(inline ,(make-info-load ptr-type #f) ,%load
                    ,($extract-fptr-address e1)
                    ,e-index (immediate ,imm-offset))))]
            ; skip allocation and dereference of ftype-pointer for $fptr-&ref
            [(call ,info ,mdcl ,pr ,e1 ,e2 ,e3) ; e1, e2, e3 = fptr, offset, ftd
             (guard
               (eq? (primref-name pr) '$fptr-&ref)
               (all-set? (prim-mask unsafe) (primref-flags pr)))
             (bind #f (e3) ;; evaluate e3 for effect
               (build-fx+raw e2 ($extract-fptr-address e1)))]
            ; skip allocation and dereference of ftype-pointer for $make-fptr
            [(call ,info ,mdcl ,pr ,e1 ,e2) ; e1, e2 = ftd, (ptr) addr
             (guard
               (eq? (primref-name pr) '$make-fptr)
               (all-set? (prim-mask unsafe) (primref-flags pr)))
             (nanopass-case (L7 Expr) e2
               [(call ,info ,mdcl ,pr ,e3)
                (guard
                  (eq? (primref-name pr) 'ftype-pointer-address)
                  (all-set? (prim-mask unsafe) (primref-flags pr)))
                (bind #f (e1)
                  `(raw ,(%mref ,e3 ,(constant record-data-disp))))]
               [else
                (bind #f (e1)
                  (ptr->integer e2 (constant ptr-bits)))])]
            [else
             `(raw (inline ,(make-info-load ptr-type #f) ,%load ,(suppress-unsafe-cast e-fptr) ,%zero
                     ,(%constant record-data-disp)))])))
      (let ()
        (define-inline 3 $fptr-offset-addr
          [(e-fptr e-offset)
           ; bind offset before doing the load (a) to maintain applicative order---the
           ; load can cause an invalid memory reference---and (b) so that the raw value
           ; isn't live across any calls
           (bind #f (e-offset)
             (build-fx+raw e-offset
               ($extract-fptr-address e-fptr)))])
        (define-inline 3 $fptr-&ref
          [(e-fptr e-offset e-ftd)
           ; see comment in $fptr-offset-addr
           (bind #f (e-offset e-ftd)
             (build-$record e-ftd
               (list (build-fx+raw e-offset ($extract-fptr-address e-fptr)))))]))
      (define-inline 3 $fptr-fptr-ref
        [(e-fptr e-offset e-ftd)
         (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
           (bind #f (e-index)
             (build-$record e-ftd
               (list `(inline ,(make-info-load ptr-type #f) ,%load
                        ,($extract-fptr-address e-fptr)
                        ,e-index (immediate ,imm-offset))))))])
      (define-inline 3 $fptr-fptr-set!
        [(e-fptr e-offset e-val)
         (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
           (bind #f ([e-addr ($extract-fptr-address e-fptr)] e-index e-val)
             `(inline ,(make-info-load ptr-type #f) ,%store ,e-addr ,e-index (immediate ,imm-offset)
                (inline ,(make-info-load ptr-type #f) ,%load ,e-val ,%zero
                  ,(%constant record-data-disp)))))])
      (let ()
        (define $do-fptr-ref-inline
          (lambda (swapped? type e-fptr e-offset)
            (bind #f (e-offset)
              (build-object-ref swapped? type ($extract-fptr-address e-fptr) e-offset))))
        (define-syntax define-fptr-ref-inline
          (lambda (x)
            (define build-inline
              (lambda (name type ref maybe-k)
                #`(define-inline 3 #,name
                    [(e-fptr e-offset)
                     #,((lambda (body) (if maybe-k #`(#,maybe-k #,body) body))
                         #`($do-fptr-ref-inline #,ref #,type e-fptr e-offset))])))
            (syntax-case x ()
              [(_ name ?type ref) (build-inline #'name #'?type #'ref #f)]
              [(_ name ?type ref ?k) (build-inline #'name #'?type #'ref #'?k)])))

        (define-fptr-ref-inline $fptr-ref-integer-8 'integer-8 #f)
        (define-fptr-ref-inline $fptr-ref-unsigned-8 'unsigned-8 #f)

        (define-fptr-ref-inline $fptr-ref-integer-16 'integer-16 #f)
        (define-fptr-ref-inline $fptr-ref-unsigned-16 'unsigned-16 #f)
        (define-fptr-ref-inline $fptr-ref-swap-integer-16 'integer-16 #t)
        (define-fptr-ref-inline $fptr-ref-swap-unsigned-16 'unsigned-16 #t)

        (when-known-endianness
         (define-fptr-ref-inline $fptr-ref-integer-24 'integer-24 #f)
         (define-fptr-ref-inline $fptr-ref-unsigned-24 'unsigned-24 #f)
         (define-fptr-ref-inline $fptr-ref-swap-integer-24 'integer-24 #t)
         (define-fptr-ref-inline $fptr-ref-swap-unsigned-24 'unsigned-24 #t))

        (define-fptr-ref-inline $fptr-ref-integer-32 'integer-32 #f)
        (define-fptr-ref-inline $fptr-ref-unsigned-32 'unsigned-32 #f)
        (define-fptr-ref-inline $fptr-ref-swap-integer-32 'integer-32 #t)
        (define-fptr-ref-inline $fptr-ref-swap-unsigned-32 'unsigned-32 #t)

        (when-known-endianness
         (define-fptr-ref-inline $fptr-ref-integer-40 'integer-40 #f)
         (define-fptr-ref-inline $fptr-ref-unsigned-40 'unsigned-40 #f)
         (define-fptr-ref-inline $fptr-ref-swap-integer-40 'integer-40 #t)
         (define-fptr-ref-inline $fptr-ref-swap-unsigned-40 'unsigned-40 #t)

         (define-fptr-ref-inline $fptr-ref-integer-48 'integer-48 #f)
         (define-fptr-ref-inline $fptr-ref-unsigned-48 'unsigned-48 #f)
         (define-fptr-ref-inline $fptr-ref-swap-integer-48 'integer-48 #t)
         (define-fptr-ref-inline $fptr-ref-swap-unsigned-48 'unsigned-48 #t)
         
         (define-fptr-ref-inline $fptr-ref-integer-56 'integer-56 #f)
         (define-fptr-ref-inline $fptr-ref-unsigned-56 'unsigned-56 #f)
         (define-fptr-ref-inline $fptr-ref-swap-integer-56 'integer-56 #t)
         (define-fptr-ref-inline $fptr-ref-swap-unsigned-56 'unsigned-56 #t))

        (define-fptr-ref-inline $fptr-ref-integer-64 'integer-64 #f)
        (define-fptr-ref-inline $fptr-ref-unsigned-64 'unsigned-64 #f)
        (define-fptr-ref-inline $fptr-ref-swap-integer-64 'integer-64 #t)
        (define-fptr-ref-inline $fptr-ref-swap-unsigned-64 'unsigned-64 #t)

        (define-fptr-ref-inline $fptr-ref-double-float 'double-float #f)
        (define-fptr-ref-inline $fptr-ref-swap-double-float 'double-float #t)

        (define-fptr-ref-inline $fptr-ref-single-float 'single-float #f)
        (define-fptr-ref-inline $fptr-ref-swap-single-float 'single-float #t)

        (define-fptr-ref-inline $fptr-ref-char 'unsigned-8 #f
          (lambda (x) (build-integer->char x)))

        (define-fptr-ref-inline $fptr-ref-wchar
          (constant-case wchar-bits [(16) 'unsigned-16] [(32) 'unsigned-32])
          #f
          (lambda (x) (build-integer->char x)))
        (define-fptr-ref-inline $fptr-ref-swap-wchar
          (constant-case wchar-bits [(16) 'unsigned-16] [(32) 'unsigned-32])
          #t
          (lambda (x) (build-integer->char x)))

        (define-fptr-ref-inline $fptr-ref-boolean
          (constant-case int-bits [(32) 'unsigned-32] [(64) 'unsigned-64])
          #f
          (lambda (x)
            `(if ,(%inline eq? ,x (immediate 0))
                 ,(%constant sfalse)
                 ,(%constant strue))))
        (define-fptr-ref-inline $fptr-ref-swap-boolean
          (constant-case int-bits [(32) 'unsigned-32] [(64) 'unsigned-64])
          #t
          (lambda (x)
            `(if ,(%inline eq? ,x (immediate 0))
                 ,(%constant sfalse)
                 ,(%constant strue))))

        (define-fptr-ref-inline $fptr-ref-stdbool
          (constant-case stdbool-bits [(8) 'unsigned-8])
          #f
          (lambda (x)
            `(if ,(%inline eq? ,x (immediate 0))
                 ,(%constant sfalse)
                 ,(%constant strue))))
        (define-fptr-ref-inline $fptr-ref-swap-stdbool
          (constant-case stdbool-bits [(8) 'unsigned-8])
          #t
          (lambda (x)
            `(if ,(%inline eq? ,x (immediate 0))
                 ,(%constant sfalse)
                 ,(%constant strue))))

        (define-fptr-ref-inline $fptr-ref-fixnum 'fixnum #f)
        (define-fptr-ref-inline $fptr-ref-swap-fixnum 'fixnum #t))
      (let ()
        (define $do-fptr-set!-inline
          (lambda (set type e-fptr e-offset e-val)
            (bind #f (e-offset)
              (set type ($extract-fptr-address e-fptr) e-offset e-val))))
        (define-syntax define-fptr-set!-inline
          (lambda (x)
            (define build-body
              (lambda (type set maybe-massage-val)
                #``(seq ,e-info
                     #,(let ([body #`($do-fptr-set!-inline #,set #,type e-fptr e-offset e-val)])
                         (if maybe-massage-val
                             #`,(bind #f (e-offset [e-val (#,maybe-massage-val e-val)]) #,body)
                             #`,(bind #f (e-offset e-val) #,body))))))
            (define build-inline
              (lambda (name check-64? body)
                #`(define-inline 3 #,name
                    [(e-info e-fptr e-offset e-val)
                     #,(if check-64?
                           #`(and (fx>= (constant ptr-bits) 64) #,body)
                           body)])))
            (syntax-case x ()
              [(_ check-64? name ?type set)
               (build-inline #'name (datum check-64?) (build-body #'?type #'set #f))]
              [(_ check-64? name ?type set ?massage-value)
               (build-inline #'name (datum check-64?) (build-body #'?type #'set #'?massage-value))])))

        (define-fptr-set!-inline #f $fptr-set-integer-8! 'integer-8 build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-unsigned-8! 'unsigned-8 build-object-set!)

        (define-fptr-set!-inline #f $fptr-set-integer-16! 'integer-16 build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-unsigned-16! 'unsigned-16 build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-swap-integer-16! 'integer-16 build-swap-object-set!)
        (define-fptr-set!-inline #f $fptr-set-swap-unsigned-16! 'unsigned-16 build-swap-object-set!)

        (when-known-endianness
         (define-fptr-set!-inline #f $fptr-set-integer-24! 'integer-24 build-object-set!)
         (define-fptr-set!-inline #f $fptr-set-unsigned-24! 'unsigned-24 build-object-set!)
         (define-fptr-set!-inline #f $fptr-set-swap-integer-24! 'integer-24 build-swap-object-set!)
         (define-fptr-set!-inline #f $fptr-set-swap-unsigned-24! 'unsigned-24 build-swap-object-set!))

        (define-fptr-set!-inline #f $fptr-set-integer-32! 'integer-32 build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-unsigned-32! 'unsigned-32 build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-swap-integer-32! 'integer-32 build-swap-object-set!)
        (define-fptr-set!-inline #f $fptr-set-swap-unsigned-32! 'unsigned-32 build-swap-object-set!)

        (when-known-endianness
         (define-fptr-set!-inline #t $fptr-set-integer-40! 'integer-40 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-unsigned-40! 'unsigned-40 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-integer-40! 'integer-40 build-swap-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-unsigned-40! 'unsigned-40 build-swap-object-set!)

         (define-fptr-set!-inline #t $fptr-set-integer-48! 'integer-48 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-unsigned-48! 'unsigned-48 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-integer-48! 'integer-48 build-swap-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-unsigned-48! 'unsigned-48 build-swap-object-set!)
         
         (define-fptr-set!-inline #t $fptr-set-integer-56! 'integer-56 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-unsigned-56! 'unsigned-56 build-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-integer-56! 'integer-56 build-swap-object-set!)
         (define-fptr-set!-inline #t $fptr-set-swap-unsigned-56! 'unsigned-56 build-swap-object-set!))

        (define-fptr-set!-inline #t $fptr-set-integer-64! 'integer-64 build-object-set!)
        (define-fptr-set!-inline #t $fptr-set-unsigned-64! 'unsigned-64 build-object-set!)
        (define-fptr-set!-inline #t $fptr-set-swap-integer-64! 'integer-64 build-swap-object-set!)
        (define-fptr-set!-inline #t $fptr-set-swap-unsigned-64! 'unsigned-64 build-swap-object-set!)

        (define-fptr-set!-inline #f $fptr-set-double-float! 'double-float build-object-set!)
        (define-fptr-set!-inline #t $fptr-set-swap-double-float! 'double-float build-swap-object-set!)

        (define-fptr-set!-inline #f $fptr-set-single-float! 'single-float build-object-set!)

        (define-fptr-set!-inline #f $fptr-set-char! 'unsigned-8 build-object-set!
          (lambda (z) (build-char->integer z)))

        (define-fptr-set!-inline #f $fptr-set-wchar!
          (constant-case wchar-bits
            [(16) 'unsigned-16]
            [(32) 'unsigned-32])
          build-object-set!
          (lambda (z) (build-char->integer z)))
        (define-fptr-set!-inline #f $fptr-set-swap-wchar!
          (constant-case wchar-bits
            [(16) 'unsigned-16]
            [(32) 'unsigned-32])
          build-swap-object-set!
          (lambda (z) (build-char->integer z)))

        (define-fptr-set!-inline #f $fptr-set-boolean!
          (constant-case int-bits
            [(32) 'unsigned-32]
            [(64) 'unsigned-64])
          build-object-set!
          (lambda (z) `(if ,z (immediate ,(fix 1)) (immediate ,(fix 0)))))
        (define-fptr-set!-inline #f $fptr-set-swap-boolean!
          (constant-case int-bits
            [(32) 'unsigned-32]
            [(64) 'unsigned-64])
          build-swap-object-set!
          (lambda (z) `(if ,z (immediate ,(fix 1)) (immediate ,(fix 0)))))

        (define-fptr-set!-inline #f $fptr-set-stdbool!
          (constant-case stdbool-bits
            [(8) 'unsigned-8])
          build-object-set!
          (lambda (z) `(if ,z (immediate ,(fix 1)) (immediate ,(fix 0)))))
        (define-fptr-set!-inline #f $fptr-set-swap-stdbool!
          (constant-case stdbool-bits
            [(8) 'unsigned-8])
          build-swap-object-set!
          (lambda (z) `(if ,z (immediate ,(fix 1)) (immediate ,(fix 0)))))

        (define-fptr-set!-inline #f $fptr-set-fixnum! 'fixnum build-object-set!)
        (define-fptr-set!-inline #f $fptr-set-swap-fixnum! 'fixnum build-swap-object-set!))
      (let ()
        (define-syntax define-fptr-bits-ref-inline
          (lambda (x)
            (syntax-case x ()
              [(_ name signed? type swapped?)
               #'(define-inline 3 name
                   [(e-fptr e-offset e-start e-end)
                    (and (fixnum-constant? e-start) (fixnum-constant? e-end)
                         (let ([imm-start (constant-value e-start)] [imm-end (constant-value e-end)])
                           (and (<= (type->width 'type) (constant ptr-bits))
                                (and (fx>= imm-start 0) (fx> imm-end imm-start) (fx<= imm-end (constant ptr-bits)))
                                ((if signed? fx<= fx<) (fx- imm-end imm-start) (constant fixnum-bits))
                                (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
                                  (bind #f (e-index)
                                    (build-int-load swapped? 'type ($extract-fptr-address e-fptr) e-index imm-offset
                                      (lambda (x)
                                        ((if signed? extract-signed-bitfield extract-unsigned-bitfield) #t imm-start imm-end x))))))))])])))

        (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-8 #t unsigned-8 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-8 #f unsigned-8 #f)

        (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-16 #t unsigned-16 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-16 #f unsigned-16 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-16 #t unsigned-16 #t)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-16 #f unsigned-16 #t)

        (when-known-endianness
         (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-24 #t unsigned-24 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-24 #f unsigned-24 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-24 #t unsigned-24 #t)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-24 #f unsigned-24 #t))

        (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-32 #t unsigned-32 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-32 #f unsigned-32 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-32 #t unsigned-32 #t)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-32 #f unsigned-32 #t)

        (when-known-endianness
         (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-40 #t unsigned-40 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-40 #f unsigned-40 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-40 #t unsigned-40 #t)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-40 #f unsigned-40 #t)

         (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-48 #t unsigned-48 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-48 #f unsigned-48 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-48 #t unsigned-48 #t)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-48 #f unsigned-48 #t)

         (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-56 #t unsigned-56 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-56 #f unsigned-56 #f)
         (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-56 #t unsigned-56 #t)
         (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-56 #f unsigned-56 #t))

        (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-64 #t unsigned-64 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-64 #f unsigned-64 #f)
        (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-64 #t unsigned-64 #t)
        (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-64 #f unsigned-64 #t))
      (let ()
        (define-syntax define-fptr-bits-set-inline
          (lambda (x)
            (syntax-case x ()
              [(_ check-64? name type swapped?)
               (with-syntax ([(checks ...) #'((fixnum-constant? e-start) (fixnum-constant? e-end))])
                 (with-syntax ([(checks ...) (if (datum check-64?)
                                                 #'((fx>= (constant ptr-bits) 64) checks ...)
                                                 #'(checks ...))])
                   #`(define-inline 3 name
                       [(e-fptr e-offset e-start e-end e-val)
                        (and
                          checks ...
                          (let ([imm-start (constant-value e-start)] [imm-end (constant-value e-end)])
                            (and (<= (type->width 'type) (constant ptr-bits))
                                 (and (fx>= imm-start 0) (fx> imm-end imm-start) (fx<= imm-end (constant ptr-bits)))
                                 (fx< (fx- imm-end imm-start) (constant fixnum-bits))
                                 (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
                                   (bind #t (e-index)
                                     (bind #f (e-val)
                                       (bind #t ([e-addr ($extract-fptr-address e-fptr)])
                                         (build-int-load swapped? 'type e-addr e-index imm-offset
                                           (lambda (x)
                                             (build-int-store swapped? 'type e-addr e-index imm-offset
                                               (insert-bitfield #t imm-start imm-end (type->width 'type) x
                                                 e-val)))))))))))])))])))

        (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-8! unsigned-8 #f)

        (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-16! unsigned-16 #f)
        (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-16! unsigned-16 #t)

        (when-known-endianness
         (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-24! unsigned-24 #f)
         (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-24! unsigned-24 #t))

        (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-32! unsigned-32 #f)
        (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-32! unsigned-32 #t)

        (when-known-endianness
         (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-40! unsigned-40 #f)
         (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-40! unsigned-40 #t)

         (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-48! unsigned-48 #f)
         (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-48! unsigned-48 #t)

         (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-56! unsigned-56 #f)
         (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-56! unsigned-56 #t))

        (define-fptr-bits-set-inline #t $fptr-set-bits-unsigned-64! unsigned-64 #f)
        (define-fptr-bits-set-inline #t $fptr-set-bits-swap-unsigned-64! unsigned-64 #t))
      (define-inline 3 $fptr-locked-decr!
        [(e-fptr e-offset)
         `(seq
            ,(let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
               (%inline locked-decr!
                 ,($extract-fptr-address e-fptr)
                 ,e-index (immediate ,imm-offset)))
            (inline ,(make-info-condition-code 'eq? #f #t) ,%condition-code))])
      (define-inline 3 $fptr-locked-incr!
        [(e-fptr e-offset)
         `(seq
            ,(let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
               (%inline locked-incr!
                 ,($extract-fptr-address e-fptr)
                 ,e-index (immediate ,imm-offset)))
            (inline ,(make-info-condition-code 'eq? #f #t) ,%condition-code))])
      (let ()
        (define clear-lock
          (lambda (e-fptr e-offset)
            (let ([lock-type (constant-case ptr-bits [(32) 'integer-32] [(64) 'integer-64])])
              (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
                `(inline ,(make-info-load lock-type #f) ,%store
                   ,($extract-fptr-address e-fptr)
                   ,e-index (immediate ,imm-offset) (immediate 0))))))
        (define-inline 3 $fptr-init-lock!
          [(e-fptr e-offset) (clear-lock e-fptr e-offset)])
        (define-inline 3 $fptr-unlock!
          [(e-fptr e-offset) (clear-lock e-fptr e-offset)]))
      (define-inline 3 $fptr-lock!
        [(e-fptr e-offset)
         (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
           (bind #t ([e-base ($extract-fptr-address e-fptr)])
             (%inline lock! ,e-base ,e-index (immediate ,imm-offset))))])
      (define-inline 3 $fptr-spin-lock!
        [(e-fptr e-offset)
         (let-values ([(e-index imm-offset) (offset-expr->index+offset e-offset)])
           (bind #t ([e-base ($extract-fptr-address e-fptr)])
             (bind #t (e-index)
               (let ([L1 (make-local-label 'L1)] [L2 (make-local-label 'L2)])
                 `(label ,L1
                    (if ,(%inline lock! ,e-base ,e-index (immediate ,imm-offset))
                        ,(%constant svoid)
                        (seq
                          (pariah)
                          (label ,L2
                            (seq
                              ,(%inline pause)
                              (if ,(%inline eq? (mref ,e-base ,e-index ,imm-offset uptr) (immediate 0))
                                  (goto ,L1)
                                  (goto ,L2)))))))))))]))
    (let ()
      (define build-port-flags-set?
        (lambda (e-p e-flags)
          (%inline logtest
             ,(%mref ,e-p ,(constant port-type-disp))
             ,(nanopass-case (L7 Expr) e-flags
                [(quote ,d) `(immediate ,(ash d (constant port-flags-offset)))]
                [else (%inline sll ,e-flags
                         (immediate ,(fx- (constant port-flags-offset) (constant fixnum-offset))))]))))
      (define build-port-input-empty?
        (lambda (e-p)
          (%inline eq?
             ,(%mref ,e-p ,(constant port-icount-disp))
             (immediate 0))))
      (define-inline 3 binary-port?
        [(e-p) (build-port-flags-set? e-p `(quote ,(constant port-flag-binary)))])
      (define-inline 3 textual-port?
        [(e-p) (build-not (build-port-flags-set? e-p `(quote ,(constant port-flag-binary))))])
      (define-inline 3 port-closed?
        [(e-p) (build-port-flags-set? e-p `(quote ,(constant port-flag-closed)))])
      (define-inline 3 $port-flags-set?
        [(e-p e-flags) (build-port-flags-set? e-p e-flags)])
      (define-inline 3 port-eof?
        [(e-p)
         (bind #t (e-p)
           `(if ,(build-port-input-empty? e-p)
                (if ,(build-port-flags-set? e-p `(quote ,(constant port-flag-eof)))
                    (immediate ,(constant strue))
                    ,(build-libcall #t src sexpr unsafe-port-eof? e-p))
                (immediate ,(constant sfalse))))])
      (define-inline 2 port-eof?
        [(e-p)
         (let ([Llib (make-local-label 'Llib)])
           (bind #t (e-p)
             `(if ,(%type-check mask-typed-object type-typed-object ,e-p)
                  ,(bind #t ([t0 (%mref ,e-p ,(constant typed-object-type-disp))])
                     `(if ,(%type-check mask-input-port type-input-port ,t0)
                          (if ,(build-port-input-empty? e-p)
                              (if ,(%inline logtest ,t0
                                     (immediate ,(ash (constant port-flag-eof) (constant port-flags-offset))))
                                  (immediate ,(constant strue))
                                  (label ,Llib ,(build-libcall #t src sexpr safe-port-eof? e-p)))
                              (immediate ,(constant sfalse)))
                          (goto ,Llib)))
                  (goto ,Llib))))])
      (define-inline 3 port-input-empty?
        [(e-p) (build-port-input-empty? e-p)])
      (define-inline 3 port-output-full?
        [(e-p)
         (%inline eq?
            ,(%mref ,e-p ,(constant port-ocount-disp))
            (immediate 0))]))
    (let ()
      (define build-set-port-flags!
        (lambda (e-p e-flags)
          (bind #t (e-p)
            `(set! ,(%mref ,e-p ,(constant port-type-disp))
               ,(%inline logor
                 ,(%mref ,e-p ,(constant port-type-disp))
                 ,(nanopass-case (L7 Expr) e-flags
                    [(quote ,d) `(immediate ,(ash d (constant port-flags-offset)))]
                    [else
                      (translate e-flags
                        (constant fixnum-offset)
                        (constant port-flags-offset))]))))))
      (define build-reset-port-flags!
        (lambda (e-p e-flags)
          (bind #t (e-p)
            `(set! ,(%mref ,e-p ,(constant port-type-disp))
               ,(%inline logand
                 ,(%mref ,e-p ,(constant port-type-disp))
                 ,(nanopass-case (L7 Expr) e-flags
                    [(quote ,d) `(immediate ,(lognot (ash d (constant port-flags-offset))))]
                    [else
                      (%inline lognot
                         ,(translate e-flags
                            (constant fixnum-offset)
                            (constant port-flags-offset)))]))))))
      (define-inline 3 $set-port-flags!
        [(e-p e-flags) (build-set-port-flags! e-p e-flags)])
      (define-inline 3 $reset-port-flags!
        [(e-p e-flags) (build-reset-port-flags! e-p e-flags)])
      (define-inline 3 mark-port-closed!
        [(e-p) (build-set-port-flags! e-p `(quote ,(constant port-flag-closed)))])
      (let ()
        (define (go e-p e-bool flag)
          (let ([e-flags `(quote ,flag)])
            (nanopass-case (L7 Expr) e-bool
              [(quote ,d)
               ((if d build-set-port-flags! build-reset-port-flags!) e-p e-flags)]
              [else
                (bind #t (e-p)
                  `(if ,e-bool
                       ,(build-set-port-flags! e-p e-flags)
                       ,(build-reset-port-flags! e-p e-flags)))])))
        (define-inline 3 set-port-bol!
          [(e-p e-bool) (go e-p e-bool (constant port-flag-bol))])
        (define-inline 3 set-port-eof!
          [(e-p e-bool) (go e-p e-bool (constant port-flag-eof))])))
    (let ()
      (define (build-port-input-size port-type e-p)
        (bind #t (e-p)
          (translate
            (%inline -
               ,(%inline -
                 ,(%mref ,e-p ,(constant port-ilast-disp))
                 ,(%mref ,e-p ,(constant port-ibuffer-disp)))
               (immediate
                 ,(if (eq? port-type 'textual)
                      (constant string-data-disp)
                      (constant bytevector-data-disp))))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-input-size
        [(e-p) (build-port-input-size 'textual e-p)])
      (define-inline 3 binary-port-input-size
        [(e-p) (build-port-input-size 'binary e-p)]))
    (let ()
      (define (build-port-output-size port-type e-p)
        (bind #t (e-p)
          (translate
            (%inline -
               ,(%inline -
                 ,(%mref ,e-p ,(constant port-olast-disp))
                 ,(%mref ,e-p ,(constant port-obuffer-disp)))
               (immediate
                 ,(if (eq? port-type 'textual)
                      (constant string-data-disp)
                      (constant bytevector-data-disp))))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-output-size
        [(e-p) (build-port-output-size 'textual e-p)])
      (define-inline 3 binary-port-output-size
        [(e-p) (build-port-output-size 'binary e-p)]))
    (let ()
      (define (build-port-input-index port-type e-p)
        (bind #t (e-p)
          (translate
            ; TODO: use lea2?
            (%inline +
               ,(%inline -
                 ,(%inline -
                   ,(%mref ,e-p ,(constant port-ilast-disp))
                   ,(%mref ,e-p ,(constant port-ibuffer-disp)))
                 (immediate
                   ,(if (eq? port-type 'textual)
                        (constant string-data-disp)
                        (constant bytevector-data-disp))))
               ,(%mref ,e-p ,(constant port-icount-disp)))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-input-index
        [(e-p) (build-port-input-index 'textual e-p)])
      (define-inline 3 binary-port-input-index
        [(e-p) (build-port-input-index 'binary e-p)]))
    (let ()
      (define (build-port-output-index port-type e-p)
        (bind #t (e-p)
          (translate
            (%inline +
               ,(%inline -
                 ,(%inline -
                   ,(%mref ,e-p ,(constant port-olast-disp))
                   ,(%mref ,e-p ,(constant port-obuffer-disp)))
                 (immediate
                   ,(if (eq? port-type 'textual)
                        (constant string-data-disp)
                        (constant bytevector-data-disp))))
               ,(%mref ,e-p ,(constant port-ocount-disp)))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-output-index
        [(e-p) (build-port-output-index 'textual e-p)])
      (define-inline 3 binary-port-output-index
        [(e-p) (build-port-output-index 'binary e-p)]))
    (let ()
      (define (build-port-input-count port-type e-p)
        (bind #t (e-p)
          (translate
            (%inline -
               (immediate 0)
               ,(%mref ,e-p ,(constant port-icount-disp)))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-input-count
        [(e-p) (build-port-input-count 'textual e-p)])
      (define-inline 3 binary-port-input-count
        [(e-p) (build-port-input-count 'binary e-p)]))
    (let ()
      (define (build-port-output-count port-type e-p)
        (bind #t (e-p)
          (translate
            (%inline -
               (immediate 0)
               ,(%mref ,e-p ,(constant port-ocount-disp)))
            (if (eq? port-type 'textual) (constant string-char-offset) 0)
            (constant fixnum-offset))))
      (define-inline 3 textual-port-output-count
        [(e-p) (build-port-output-count 'textual e-p)])
      (define-inline 3 binary-port-output-count
        [(e-p) (build-port-output-count 'binary e-p)]))
    (let ()
      (define (build-set-port-input-size! port-type e-p e-x)
        ; actually, set last to buffer[0] + size; count to size
        (bind #t (e-p)
          (bind #t ([e-x (translate e-x
                           (constant fixnum-offset)
                           (if (eq? port-type 'textual) (constant string-char-offset) 0))])
            `(seq
               (set! ,(%mref ,e-p ,(constant port-icount-disp))
                 ,(%inline - (immediate 0) ,e-x))
               (set! ,(%mref ,e-p ,(constant port-ilast-disp))
                 ,(%inline +
                   ,(%inline +
                     ,(%mref ,e-p ,(constant port-ibuffer-disp))
                     (immediate
                       ,(if (eq? port-type 'textual)
                            (constant string-data-disp)
                            (constant bytevector-data-disp))))
                   ,e-x))))))
      (define-inline 3 set-textual-port-input-size!
        [(e-p e-x) (build-set-port-input-size! 'textual e-p e-x)])
      (define-inline 3 set-binary-port-input-size!
        [(e-p e-x) (build-set-port-input-size! 'binary e-p e-x)]))
    (let ()
      (define (build-set-port-output-size! port-type e-p e-x)
        ; actually, set last to buffer[0] + size; count to size
        (bind #t (e-p)
          (bind #t ([e-x (translate e-x
                           (constant fixnum-offset)
                           (if (eq? port-type 'textual) (constant string-char-offset) 0))])
            `(seq
               (set! ,(%mref ,e-p ,(constant port-ocount-disp))
                 ,(%inline - (immediate 0) ,e-x))
               (set! ,(%mref ,e-p ,(constant port-olast-disp))
                 ,(%inline +
                   ,(%inline +
                     ,(%mref ,e-p ,(constant port-obuffer-disp))
                     (immediate
                       ,(if (eq? port-type 'textual)
                            (constant string-data-disp)
                            (constant bytevector-data-disp))))
                   ,e-x))))))
      (define-inline 3 set-textual-port-output-size!
        [(e-p e-x) (build-set-port-output-size! 'textual e-p e-x)])
      (define-inline 3 set-binary-port-output-size!
        [(e-p e-x) (build-set-port-output-size! 'binary e-p e-x)]))
    (let ()
      (define (build-set-port-input-index! port-type e-p e-x)
        ; actually, set count to index - size, where size = last - buffer[0]
        (bind #t (e-p)
          `(set! ,(%mref ,e-p ,(constant port-icount-disp))
             ,(%inline -
               ,(translate e-x
                  (constant fixnum-offset)
                  (if (eq? port-type 'textual) (constant string-char-offset) 0))
               ,(%inline -
                 ,(%mref ,e-p ,(constant port-ilast-disp))
                 ,(%inline +
                   ,(%mref ,e-p ,(constant port-ibuffer-disp))
                   (immediate
                     ,(if (eq? port-type 'textual)
                          (constant string-data-disp)
                          (constant bytevector-data-disp)))))))))
      (define-inline 3 set-textual-port-input-index!
        [(e-p e-x) (build-set-port-input-index! 'textual e-p e-x)])
      (define-inline 3 set-binary-port-input-index!
        [(e-p e-x) (build-set-port-input-index! 'binary e-p e-x)]))
    (let ()
      (define (build-set-port-output-index! port-type e-p e-x)
        ; actually, set count to index - size, where size = last - buffer[0]
        (bind #t (e-p)
          `(set! ,(%mref ,e-p ,(constant port-ocount-disp))
             ,(%inline -
               ,(translate e-x
                  (constant fixnum-offset)
                  (if (eq? port-type 'textual) (constant string-char-offset) 0))
               ,(%inline -
                 ,(%mref ,e-p ,(constant port-olast-disp))
                 ,(%inline +
                   ,(%mref ,e-p ,(constant port-obuffer-disp))
                   (immediate
                     ,(if (eq? port-type 'textual)
                          (constant string-data-disp)
                          (constant bytevector-data-disp)))))))))
      (define-inline 3 set-textual-port-output-index!
        [(e-p e-x) (build-set-port-output-index! 'textual e-p e-x)])
      (define-inline 3 set-binary-port-output-index!
        [(e-p e-x) (build-set-port-output-index! 'binary e-p e-x)]))
    (let ()
      (define (make-build-set-port-buffer! port-type ibuffer-disp icount-disp ilast-disp)
        (lambda (e-p e-b new?)
          (bind #t (e-p e-b)
            `(seq
               ,(if new?
                    `(set! ,(%mref ,e-p ,ibuffer-disp) ,e-b)
                    (build-dirty-store e-p ibuffer-disp e-b))
               ,(bind #t ([e-length (if (eq? port-type 'textual)
                                        (translate
                                          (%inline logand
                                             ,(%mref ,e-b ,(constant string-type-disp))
                                             (immediate ,(fx- (expt 2 (constant string-length-offset)))))
                                          (constant string-length-offset)
                                          (constant string-char-offset))
                                        (%inline srl
                                           ,(%mref ,e-b ,(constant bytevector-type-disp))
                                           ,(%constant bytevector-length-offset)))])
                  `(seq
                     (set! ,(%mref ,e-p ,icount-disp)
                       ,(%inline - (immediate 0) ,e-length))
                     (set! ,(%mref ,e-p ,ilast-disp)
                       ,(%lea ,e-b ,e-length
                          (if (eq? port-type 'textual)
                              (constant string-data-disp)
                              (constant bytevector-data-disp))))))))))
      (define (make-port e-name e-handler e-ib e-ob e-info flags set-ibuf! set-obuf!)
        (bind #f (e-name e-handler e-info e-ib e-ob)
          (bind #t ([e-p (%constant-alloc type-typed-object (constant size-port))])
            (%seq
              (set! ,(%mref ,e-p ,(constant port-type-disp)) (immediate ,flags))
              (set! ,(%mref ,e-p ,(constant port-handler-disp)) ,e-handler)
              (set! ,(%mref ,e-p ,(constant port-name-disp)) ,e-name)
              (set! ,(%mref ,e-p ,(constant port-info-disp)) ,e-info)
              ,(set-ibuf! e-p e-ib #t)
              ,(set-obuf! e-p e-ob #t)
              ,e-p))))
      (define (make-build-clear-count count-disp)
        (lambda (e-p e-b new?)
          `(set! ,(%mref ,e-p ,count-disp) (immediate 0))))
      (let ()
        (define build-set-textual-port-input-buffer!
          (make-build-set-port-buffer! 'textual
            (constant port-ibuffer-disp)
            (constant port-icount-disp)
            (constant port-ilast-disp)))
        (define build-set-textual-port-output-buffer!
          (make-build-set-port-buffer! 'textual
            (constant port-obuffer-disp)
            (constant port-ocount-disp)
            (constant port-olast-disp)))
        (define-inline 3 set-textual-port-input-buffer!
          [(e-p e-b) (build-set-textual-port-input-buffer! e-p e-b #f)])
        (define-inline 3 set-textual-port-output-buffer!
          [(e-p e-b) (build-set-textual-port-output-buffer! e-p e-b #f)])
        (let ()
          (define (go e-name e-handler e-ib e-info)
            (make-port e-name e-handler e-ib `(quote "") e-info
              (fxlogor (constant type-input-port) (constant PORT-FLAG-INPUT-MODE))
              build-set-textual-port-input-buffer!
              (make-build-clear-count (constant port-ocount-disp))))
          (define-inline 3 $make-textual-input-port
            [(e-name e-handler e-ib) (go e-name e-handler e-ib `(quote #f))]
            [(e-name e-handler e-ib e-info) (go e-name e-handler e-ib e-info)]))
        (let ()
          (define (go e-name e-handler e-ob e-info)
            (make-port e-name e-handler `(quote "") e-ob e-info
              (constant type-output-port)
              (make-build-clear-count (constant port-icount-disp))
              build-set-textual-port-output-buffer!))
          (define-inline 3 $make-textual-output-port
            [(e-name e-handler e-ob) (go e-name e-handler e-ob `(quote #f))]
            [(e-name e-handler e-ob e-info) (go e-name e-handler e-ob e-info)]))
        (let ()
          (define (go e-name e-handler e-ib e-ob e-info)
            (make-port e-name e-handler e-ib e-ob e-info
              (constant type-io-port)
              build-set-textual-port-input-buffer!
              build-set-textual-port-output-buffer!))
          (define-inline 3 $make-textual-input/output-port
            [(e-name e-handler e-ib e-ob) (go e-name e-handler e-ib e-ob `(quote #f))]
            [(e-name e-handler e-ib e-ob e-info) (go e-name e-handler e-ib e-ob e-info)])))
      (let ()
        (define build-set-binary-port-input-buffer!
          (make-build-set-port-buffer! 'binary
            (constant port-ibuffer-disp)
            (constant port-icount-disp)
            (constant port-ilast-disp)))
        (define build-set-binary-port-output-buffer!
          (make-build-set-port-buffer! 'binary
            (constant port-obuffer-disp)
            (constant port-ocount-disp)
            (constant port-olast-disp)))
        (define-inline 3 set-binary-port-input-buffer!
          [(e-p e-b) (build-set-binary-port-input-buffer! e-p e-b #f)])
        (define-inline 3 set-binary-port-output-buffer!
          [(e-p e-b) (build-set-binary-port-output-buffer! e-p e-b #f)])
        (let ()
          (define (go e-name e-handler e-ib e-info)
            (make-port e-name e-handler e-ib `(quote #vu8()) e-info
              (fxlogor (constant type-input-port) (constant PORT-FLAG-INPUT-MODE) (constant PORT-FLAG-BINARY))
              build-set-binary-port-input-buffer!
              (make-build-clear-count (constant port-ocount-disp))))
          (define-inline 3 $make-binary-input-port
            [(e-name e-handler e-ib) (go e-name e-handler e-ib `(quote #f))]
            [(e-name e-handler e-ib e-info) (go e-name e-handler e-ib e-info)]))
        (let ()
          (define (go e-name e-handler e-ob e-info)
            (make-port e-name e-handler `(quote #vu8()) e-ob e-info
              (fxlogor (constant type-output-port) (constant PORT-FLAG-BINARY))
              (make-build-clear-count (constant port-icount-disp))
              build-set-binary-port-output-buffer!))
          (define-inline 3 $make-binary-output-port
            [(e-name e-handler e-ob) (go e-name e-handler e-ob `(quote #f))]
            [(e-name e-handler e-ob e-info) (go e-name e-handler e-ob e-info)]))
        (let ()
          (define (go e-name e-handler e-ib e-ob e-info)
            (make-port e-name e-handler e-ib e-ob e-info
              (fxlogor (constant type-io-port) (constant PORT-FLAG-BINARY))
              build-set-binary-port-input-buffer!
              build-set-binary-port-output-buffer!))
          (define-inline 3 $make-binary-input/output-port
            [(e-name e-handler e-ib e-ob) (go e-name e-handler e-ib e-ob `(quote #f))]
            [(e-name e-handler e-ib e-ob e-info) (go e-name e-handler e-ib e-ob e-info)]))))
    (let ()
      (define build-fxvector-ref-check (build-ref-check fxvector-type-disp maximum-fxvector-length fxvector-length-offset type-fxvector mask-fxvector never-immutable-flag))
      (define build-fxvector-set!-check (build-ref-check fxvector-type-disp maximum-fxvector-length fxvector-length-offset type-fxvector mask-fxvector never-immutable-flag))
      (define-inline 2 $fxvector-ref-check?
        [(e-fv e-i) (bind #t (e-fv e-i) (build-fxvector-ref-check e-fv e-i #f))])
      (define-inline 2 $fxvector-set!-check?
        [(e-fv e-i) (bind #t (e-fv e-i) (build-fxvector-set!-check e-fv e-i #f))])
      (let ()
        (define (go e-fv e-i)
          (cond
            [(expr->index e-i 1 (constant maximum-fxvector-length)) =>
             (lambda (index)
               (%mref ,e-fv
                  ,(+ (fix index) (constant fxvector-data-disp))))]
            [else (%mref ,e-fv ,e-i ,(constant fxvector-data-disp))]))
        (define-inline 3 fxvector-ref
          [(e-fv e-i) (go e-fv e-i)])
        (define-inline 2 fxvector-ref
          [(e-fv e-i)
           (bind #t (e-fv e-i)
             `(if ,(build-fxvector-ref-check e-fv e-i #f)
                  ,(go e-fv e-i)
                  ,(build-libcall #t src sexpr fxvector-ref e-fv e-i)))]))
      (let ()
        (define (go e-fv e-i e-new)
          `(set!
             ,(cond
                [(expr->index e-i 1 (constant maximum-fxvector-length)) =>
                 (lambda (index)
                   (%mref ,e-fv
                      ,(+ (fix index) (constant fxvector-data-disp))))]
                [else (%mref ,e-fv ,e-i ,(constant fxvector-data-disp))])
             ,e-new))
       (define-inline 3 fxvector-set!
         [(e-fv e-i e-new)
          (go e-fv e-i e-new)])
       (define-inline 2 fxvector-set!
         [(e-fv e-i e-new)
          (bind #t (e-fv e-i e-new)
            `(if ,(build-fxvector-set!-check e-fv e-i e-new)
                 ,(go e-fv e-i e-new)
                 ,(build-libcall #t src sexpr fxvector-set! e-fv e-i e-new)))])))
    (let ()
      (define build-flvector-ref-check (build-ref-check flvector-type-disp maximum-flvector-length flvector-length-offset type-flvector mask-flvector never-immutable-flag))
      (define build-flvector-set!-check (build-ref-check flvector-type-disp maximum-flvector-length flvector-length-offset type-flvector mask-flvector never-immutable-flag))
      (define-inline 2 $flvector-ref-check?
        [(e-fv e-i) (bind #t (e-fv e-i) (build-flvector-ref-check e-fv e-i #f))])
      (define-inline 2 $flvector-set!-check?
        [(e-fv e-i) (bind #t (e-fv e-i) (build-flvector-set!-check e-fv e-i #f))])
      (let ()
        (define (go e-fv e-i)
          (cond
            [(expr->index e-i 1 (constant maximum-flvector-length)) =>
             (lambda (index)
               `(unboxed-fp ,(%mref ,e-fv ,%zero ,(+ (fx* index (constant flonum-bytes)) (constant flvector-data-disp)) fp)))]
            [else `(unboxed-fp ,(%mref ,e-fv ,(build-double-scale e-i) ,(constant flvector-data-disp) fp))]))
        (define-inline 3 flvector-ref
          [(e-fv e-i) (go e-fv e-i)])
        (define-inline 2 flvector-ref
          [(e-fv e-i)
           (bind #t (e-fv e-i)
             `(if ,(build-flvector-ref-check e-fv e-i #f)
                  ,(go e-fv e-i)
                  ,(build-libcall #t src sexpr flvector-ref e-fv e-i)))]))
      (let ()
        (define (go e-fv e-i e-new)
          `(set!
             ,(cond
                [(expr->index e-i 1 (constant maximum-flvector-length)) =>
                 (lambda (index)
                   (%mref ,e-fv ,%zero ,(+ (fx* index (constant flonum-bytes)) (constant flvector-data-disp)) fp))]
                [else (%mref ,e-fv ,(build-double-scale e-i) ,(constant flvector-data-disp) fp)])
             ,e-new))
        (define (checked-go src sexpr e-fv e-i e-new add-check)
          `(if ,(add-check (build-flvector-set!-check e-fv e-i #f))
               ,(go e-fv e-i e-new)
               ,(build-libcall #t src sexpr flvector-set! e-fv e-i e-new)))
       (define-inline 3 flvector-set!
         [(e-fv e-i e-new)
          (go e-fv e-i e-new)])
       (define-inline 2 flvector-set!
         [(e-fv e-i e-new)
          (bind #t (e-fv e-i)
            (if (known-flonum-result? e-new)
                (bind #t fp (e-new)
                  (checked-go src sexpr e-fv e-i e-new values))
                (bind #t (e-new)
                  (checked-go src sexpr e-fv e-i e-new
                              (lambda (e)
                                (build-and e (build-flonums? (list e-new))))))))])))
    (let ()
      (define build-string-ref-check
        (lambda (e-s e-i)
          ((build-ref-check string-type-disp maximum-string-length string-length-offset type-string mask-string string-immutable-flag) e-s e-i #f)))
      (define build-string-set!-check
        (lambda (e-s e-i)
          ((build-ref-check string-type-disp maximum-string-length string-length-offset type-mutable-string mask-mutable-string string-immutable-flag) e-s e-i #f)))
      (define-inline 2 $string-ref-check?
        [(e-s e-i) (bind #t (e-s e-i) (build-string-ref-check e-s e-i))])
      (define-inline 2 $string-set!-check?
        [(e-s e-i) (bind #t (e-s e-i) (build-string-set!-check e-s e-i))])
      (let ()
        (define (go e-s e-i)
          (cond
            [(expr->index e-i 1 (constant maximum-string-length)) =>
             (lambda (index)
               `(inline ,(make-info-load (string-char-type) #f) ,%load ,e-s ,%zero
                  (immediate ,(+ (* (constant string-char-bytes) index) (constant string-data-disp)))))]
            [else
             `(inline ,(make-info-load (string-char-type) #f) ,%load ,e-s
                ,(translate e-i
                   (constant fixnum-offset)
                   (constant string-char-offset))
                ,(%constant string-data-disp))]))
        (define-inline 3 string-ref
          [(e-s e-i) (go e-s e-i)])
        (define-inline 2 string-ref
          [(e-s e-i)
           (bind #t (e-s e-i)
             `(if ,(build-string-ref-check e-s e-i)
                  ,(go e-s e-i)
                  ,(build-libcall #t src sexpr string-ref e-s e-i)))]))
      (let ()
        (define (go e-s e-i e-new)
          (cond
            [(expr->index e-i 1 (constant maximum-string-length)) =>
             (lambda (index)
               `(inline ,(make-info-load (string-char-type) #f) ,%store ,e-s ,%zero
                  (immediate ,(+ (* (constant string-char-bytes) index) (constant string-data-disp)))
                  ,e-new))]
            [else
             `(inline ,(make-info-load (string-char-type) #f) ,%store ,e-s
                ,(translate e-i
                     (constant fixnum-offset)
                     (constant string-char-offset))
                ,(%constant string-data-disp)
                ,e-new)]))
        (define-inline 3 string-set!
          [(e-s e-i e-new) (go e-s e-i e-new)])
        (define-inline 2 string-set!
          [(e-s e-i e-new)
           (bind #t (e-s e-i e-new)
             `(if ,(let ([e-ref-check (build-string-set!-check e-s e-i)])
                     (if (constant? char? e-new)
                         e-ref-check
                         (build-and e-ref-check (%type-check mask-char type-char ,e-new))))
                  ,(go e-s e-i e-new)
                  ,(build-libcall #t src sexpr string-set! e-s e-i e-new)))])
        (define-inline 3 $string-set-immutable!
          [(e-s) ((build-set-immutable! string-type-disp string-immutable-flag) e-s)])))
    (let ()
      (define build-vector-ref-check (build-ref-check vector-type-disp maximum-vector-length vector-length-offset type-vector mask-vector vector-immutable-flag))
      (define build-vector-set!-check (build-ref-check vector-type-disp maximum-vector-length vector-length-offset type-mutable-vector mask-mutable-vector vector-immutable-flag))
      (define-inline 2 $vector-ref-check?
        [(e-v e-i) (bind #t (e-v e-i) (build-vector-ref-check e-v e-i #f))])
      (define-inline 2 $vector-set!-check?
        [(e-v e-i) (bind #t (e-v e-i) (build-vector-set!-check e-v e-i #f))])
      (let ()
        (define (go e-v e-i)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (%mref ,e-v ,(+ (fix d) (constant vector-data-disp)))]
            [else (%mref ,e-v ,e-i ,(constant vector-data-disp))]))
        (define-inline 3 vector-ref
          [(e-v e-i) (go e-v e-i)])
        (define-inline 2 vector-ref
          [(e-v e-i)
           (bind #t (e-v e-i)
             `(if ,(build-vector-ref-check e-v e-i #f)
                  ,(go e-v e-i)
                  ,(build-libcall #t src sexpr vector-ref e-v e-i)))]))
      (let ()
        (define (go e-v e-i e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store e-v (+ (fix d) (constant vector-data-disp)) e-new)]
            [else (build-dirty-store e-v e-i (constant vector-data-disp) e-new)]))
        (define-inline 3 vector-set!
          [(e-v e-i e-new) (go e-v e-i e-new)])
        (define-inline 2 vector-set!
          [(e-v e-i e-new)
           (bind #t (e-v e-i)
             (dirty-store-bind #t (e-new)
               `(if ,(build-vector-set!-check e-v e-i #f)
                    ,(go e-v e-i e-new)
                    ,(build-libcall #t src sexpr vector-set! e-v e-i e-new))))])
        (define-inline 3 $vector-set-immutable!
          [(e-fv) ((build-set-immutable! vector-type-disp vector-immutable-flag) e-fv)]))
      (let ()
        (define (go e-v e-i e-old e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store e-v %zero (+ (fix d) (constant vector-data-disp)) e-new (make-build-cas e-old) build-cas-seq)]
            [else (build-dirty-store e-v e-i (constant vector-data-disp) e-new (make-build-cas e-old) build-cas-seq)]))
        (define-inline 3 vector-cas!
          [(e-v e-i e-old e-new) (go e-v e-i e-old e-new)])
        (define-inline 2 vector-cas!
          [(e-v e-i e-old e-new)
           (bind #t (e-v e-i e-old)
             (dirty-store-bind #t (e-new)
               `(if ,(build-vector-set!-check e-v e-i #f)
                    ,(go e-v e-i e-old e-new)
                    ,(build-libcall #t src sexpr vector-cas! e-v e-i e-old e-new))))]))
      (let ()
        (define (go e-v e-i e-new)
          `(set!
             ,(nanopass-case (L7 Expr) e-i
                [(quote ,d)
                 (guard (target-fixnum? d))
                 (%mref ,e-v ,(+ (fix d) (constant vector-data-disp)))]
                [else (%mref ,e-v ,e-i ,(constant vector-data-disp))])
             ,e-new))
        (define-inline 3 vector-set-fixnum!
          [(e-v e-i e-new) (go e-v e-i e-new)])
        (define-inline 2 vector-set-fixnum!
          [(e-v e-i e-new)
           (bind #t (e-v e-i e-new)
             `(if ,(build-vector-set!-check e-v e-i e-new)
                  ,(go e-v e-i e-new)
                  ,(build-libcall #t src sexpr vector-set-fixnum! e-v e-i e-new)))])))
      (let ()
        (define (go e-v e-i)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (%mref ,e-v ,(+ (fix d) (constant stencil-vector-data-disp)))]
            [else (%mref ,e-v ,e-i ,(constant stencil-vector-data-disp))]))
        (define-inline 3 $stencil-vector-ref
          [(e-v e-i) (go e-v e-i)])
        (define-inline 3 stencil-vector-ref
          [(e-v e-i) (go e-v e-i)]))
      (let ()
        (define (go e-v e-i e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store e-v (+ (fix d) (constant stencil-vector-data-disp)) e-new)]
            [else (build-dirty-store e-v e-i (constant stencil-vector-data-disp) e-new)]))
        (define-inline 3 $stencil-vector-set!
          [(e-v e-i e-new) (go e-v e-i e-new)])
        (define-inline 3 stencil-vector-set!
          [(e-v e-i e-new) (go e-v e-i e-new)]))
      (let ()
        (define (build-dirty-store-reference base index offset e)
          (let ([a (if (eq? index %zero)
                       (%lea ,base offset)
                       (%lea ,base ,index offset))])
            (bind #t ([e e])
              ;; eval a second so the address is not live across any calls
              (bind #t ([a a])
                `(if ,(%inline eq? ,e (immediate ,(constant sfalse)))
                     (set! ,(%mref ,a ,0) (immediate 0))
                     ,(add-store-fence
                       (%seq
                        (set! ,(%mref ,a ,0) ,(%inline + ,e ,(%constant reference-disp)))
                        ,(%inline remember ,a))))))))
        (define (go e-v e-i e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store-reference e-v %zero (+ d (constant bytevector-data-disp)) e-new)]
            [else (build-dirty-store-reference e-v (build-unfix e-i) (constant bytevector-data-disp) e-new)]))
        (define-inline 3 bytevector-reference-set!
          [(e-v e-i e-new) (go e-v e-i e-new)])
        (define-inline 3 bytevector-reference-ref
          [(bv i) (let ([t (make-tmp 't 'uptr)])
                    `(let ([,t (inline ,(make-info-load ptr-type #f) ,%load
                                       ,bv ,(build-unfix i) (immediate ,(constant bytevector-data-disp)))])
                       (if ,(%inline eq? ,t (immediate 0))
                           (immediate ,(constant sfalse))
                           ,(%inline - ,t ,(%constant reference-disp)))))]))
      (let ()
        (define (go e-v e-i e-new)
          `(set!
             ,(nanopass-case (L7 Expr) e-i
                [(quote ,d)
                 (guard (target-fixnum? d))
                 (%mref ,e-v ,(+ (fix d) (constant stencil-vector-data-disp)))]
                [else (%mref ,e-v ,e-i ,(constant stencil-vector-data-disp))])
             ,e-new))
        (define-inline 3 $stencil-vector-fill-set!
          [(e-v e-i e-new) (go e-v e-i e-new)]))
      (let ()
        (define (go e-v e-i)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (%mref ,e-v ,(+ (fix d) (constant record-data-disp)))]
            [else (%mref ,e-v ,e-i ,(constant record-data-disp))]))
        (define-inline 3 $record-ref
          [(e-v e-i) (go e-v e-i)]))
      (let ()
        (define (go e-v e-i e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store e-v (+ (fix d) (constant record-data-disp)) e-new)]
            [else (build-dirty-store e-v e-i (constant record-data-disp) e-new)]))
        (define-inline 3 $record-set!
          [(e-v e-i e-new) (go e-v e-i e-new)]))
      (let ()
        (define (go e-v e-i e-old e-new)
          (nanopass-case (L7 Expr) e-i
            [(quote ,d)
             (guard (target-fixnum? d))
             (build-dirty-store e-v %zero (+ (fix d) (constant record-data-disp)) e-new (make-build-cas e-old) build-cas-seq)]
            [else (build-dirty-store e-v e-i (constant record-data-disp) e-new (make-build-cas e-old) build-cas-seq)]))
        (define-inline 3 $record-cas!
          [(e-v e-i e-old e-new) (go e-v e-i e-old e-new)]))
      (let ()
        (define build-bytevector-ref-check
          (lambda (e-bits e-bv e-i check-mutable?)
            (nanopass-case (L7 Expr) e-bits
              [(quote ,d)
               (guard (and (fixnum? d) (fx> d 0) (fx= (* (fxquotient d 8) 8) d)))
               (let ([bits d] [bytes (fxquotient d 8)])
                 (bind #t (e-bv e-i)
                   (build-and
                     (%type-check mask-typed-object type-typed-object ,e-bv)
                     (bind #t ([t (%mref ,e-bv ,(constant bytevector-type-disp))])
                       (build-and
                         (if check-mutable?
                             (%type-check mask-mutable-bytevector type-mutable-bytevector ,t)
                             (%type-check mask-bytevector type-bytevector ,t))
                         (cond
                           [(expr->index e-i bytes (constant maximum-bytevector-length)) =>
                            (lambda (index)
                              (%inline u<
                                (immediate ,(logor (ash (+ index (fx- bytes 1)) (constant bytevector-length-offset))
                                              (constant type-bytevector) (constant bytevector-immutable-flag)))
                                ,t))]
                           [else
                             (build-and
                               ($type-check (fxlogor (fix (fx- bytes 1)) (constant mask-fixnum)) (constant type-fixnum) e-i)
                               (%inline u<
                                 ; NB. add cannot overflow or change negative to positive when
                                 ; low-order (log2 bytes) bits of fixnum value are zero, as
                                 ; guaranteed by type-check above
                                 ,(if (fx= bytes 1)
                                      e-i
                                      (%inline + ,e-i (immediate ,(fix (fx- bytes 1)))))
                                 ,(%inline logand
                                    ,(translate t
                                       (constant bytevector-length-offset)
                                       (constant fixnum-offset))
                                    (immediate ,(- (constant fixnum-factor))))))]))))))]
              [(seq (profile ,src) ,[e]) (and e `(seq (profile ,src) ,e))]
              [else #f])))
        (define-inline 2 $bytevector-ref-check?
          [(e-bits e-bv e-i) (build-bytevector-ref-check e-bits e-bv e-i #f)])
        (define-inline 2 $bytevector-set!-check?
          [(e-bits e-bv e-i) (build-bytevector-ref-check e-bits e-bv e-i #t)]))
    (let ()
      (define build-bytevector-fill
        (let ([filler (make-build-fill 1 (constant bytevector-data-disp))])
          (lambda (e-bv e-bytes e-fill)
            (bind #t uptr ([e-fill (build-unfix e-fill)])
              (filler e-bv e-bytes e-fill)))))
      (let ()
        (define do-make-bytevector
          (lambda (e-length maybe-e-fill)
            ; NB: caller must bind maybe-e-fill
            (safe-assert (or (not maybe-e-fill) (no-need-to-bind? #f maybe-e-fill)))
            (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                (let ([n (constant-value e-length)])
                  (if (fx= n 0)
                      `(quote ,(bytevector))
                      (bind #t ([t (%constant-alloc type-typed-object
                                     (fx+ (constant header-size-bytevector) n))])
                        `(seq
                           (set! ,(%mref ,t ,(constant bytevector-type-disp))
                             (immediate ,(fx+ (fx* n (constant bytevector-length-factor))
                                              (constant type-bytevector))))
                           ,(if maybe-e-fill
                                (build-bytevector-fill t `(immediate ,n) maybe-e-fill)
                                t)))))
                (bind #t (e-length)
                  (let ([t-bytes (make-tmp 'tbytes 'uptr)] [t-vec (make-tmp 'tvec)])
                    `(if ,(%inline eq? ,e-length (immediate 0))
                         (quote ,(bytevector))
                         (let ([,t-bytes ,(build-unfix e-length)])
                           (let ([,t-vec (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                              ,(%inline logand
                                                ,(%inline + ,t-bytes
                                                  (immediate ,(fx+ (constant header-size-bytevector)
                                                                      (fx- (constant byte-alignment) 1))))
                                                (immediate ,(- (constant byte-alignment)))))])
                             (seq
                               (set! ,(%mref ,t-vec ,(constant bytevector-type-disp))
                                 ,(build-type/length t-bytes
                                    (constant type-bytevector)
                                    0
                                    (constant bytevector-length-offset)))
                               ,(if maybe-e-fill
                                    (build-bytevector-fill t-vec t-bytes maybe-e-fill)
                                    t-vec))))))))))
        (let ()
          (define valid-length?
            (lambda (e-length)
              (constant?
                (lambda (x)
                  (and (or (fixnum? x) (bignum? x))
                       (<= 0 x (constant maximum-bytevector-length))))
                e-length)))
          (define-inline 2 make-bytevector
            [(e-length) (and (valid-length? e-length) (do-make-bytevector e-length #f))]
            [(e-length e-fill)
             (and (valid-length? e-length)
                  (constant? (lambda (x) (and (fixnum? x) (fx<= -128 x 255))) e-fill)
                  (do-make-bytevector e-length e-fill))]))
        (define-inline 3 make-bytevector
          [(e-length) (do-make-bytevector e-length #f)]
          [(e-length e-fill) (bind #f (e-fill) (do-make-bytevector e-length e-fill))]))
      (define-inline 3 bytevector-fill!
        [(e-bv e-fill)
         (bind #t (e-bv e-fill)
           `(seq
              ,(build-bytevector-fill e-bv
                 (%inline srl
                    ,(%mref ,e-bv ,(constant bytevector-type-disp))
                    ,(%constant bytevector-length-offset))
                 e-fill)
              ,(%constant svoid)))])
      (define-inline 2 bytevector->immutable-bytevector
        [(e-bv)
         (nanopass-case (L7 Expr) e-bv
           [(quote ,d)
            (guard (bytevector? d) (= 0 (bytevector-length d)))
            `(literal ,(make-info-literal #f 'entry (lookup-c-entry null-immutable-bytevector) 0))]
           [else #f])]))

    (let ()
      (define build-bytevector
        (lambda (e*)
          (define (find-k n)
            (constant-case native-endianness
              [(unknown)
               (values 1 'unsigned-8)]
              [else
               (let loop ([bytes (constant-case ptr-bits [(32) 4] [(64) 8])]
                          [type* (constant-case ptr-bits
                                   [(32) '(unsigned-32 unsigned-16 unsigned-8)]
                                   [(64) '(unsigned-64 unsigned-32 unsigned-16 unsigned-8)])])
                 (let ([bytes/2 (fxsrl bytes 1)])
                   (if (fx<= n bytes/2)
                       (loop bytes/2 (cdr type*))
                       (values bytes (car type*)))))]))
          (define (build-chunk k n e*)
            (define (build-shift e shift)
              (if (fx= shift 0) e (%inline sll ,e (immediate ,shift))))
            (let loop ([k (constant-case native-endianness
                            [(little) (fxmin k n)]
                            [(big) k]
                            [(unknown) (safe-assert (= k 1)) 1])]
                       [e* (constant-case native-endianness
                             [(little) (reverse (if (fx<= n k) e* (list-head e* k)))]
                             [(big) e*]
                             [(unknown) e*])]
                       [constant-part 0]
                       [expression-part #f]
                       [expression-shift 0]
                       [mask? #f]) ; no need to mask the high-order byte
              (if (fx= k 0)
                  (if expression-part
                      (let ([expression-part (build-shift expression-part expression-shift)])
                        (if (= constant-part 0)
                            expression-part
                            (%inline logor ,expression-part (immediate ,constant-part))))
                      `(immediate ,constant-part))
                  (let ([k (fx- k 1)]
                        [constant-part (ash constant-part 8)]
                        [expression-shift (fx+ expression-shift 8)])
                    (if (null? e*)
                        (loop k e* constant-part expression-part expression-shift #t)
                        (let ([e (car e*)] [e* (cdr e*)])
                          (if (fixnum-constant? e)
                              (loop k e* (logor constant-part (logand (constant-value e) #xff)) expression-part expression-shift #t)
                              (loop k e* constant-part
                                (let* ([e (build-unfix e)]
                                       [e (if mask? (%inline logand ,e (immediate #xff)) e)])
                                  (if expression-part
                                      (%inline logor ,(build-shift expression-part expression-shift) ,e)
                                      e))
                                0 #t))))))))
          (let ([len (length e*)])
            (if (fx= len 0)
                `(quote ,(bytevector))
                (list-bind #f (e*)
                  (bind #t ([t (%constant-alloc type-typed-object
                                 (fx+ (constant header-size-bytevector) len))])
                    `(seq
                       (set! ,(%mref ,t ,(constant bytevector-type-disp))
                         (immediate ,(+ (* len (constant bytevector-length-factor))
                                        (constant type-bytevector))))
                       ;  build and store k-octet (k = 4 on 32-bit machines, k = 8 on 64-bit
                       ;  machines) chunks, taking endianness into account.  for the last
                       ;  chunk, set k = 1, 2, 4, or 8 depending on the number of octets
                       ;  remaining, padding with zeros as necessary.
                       ,(let f ([e* e*] [n (length e*)] [offset (constant bytevector-data-disp)])
                          (let-values ([(k type) (find-k n)])
                            `(seq
                               (inline ,(make-info-load type #f) ,%store ,t ,%zero (immediate ,offset)
                                 ,(build-chunk k n e*))
                               ,(if (fx<= n k)
                                    t
                                    (f (list-tail e* k) (fx- n k) (fx+ offset k)))))))))))))

      (define-inline 2 bytevector
        [e* (and (andmap
                   (lambda (x)
                     (constant?
                       (lambda (x) (and (fixnum? x) (fx<= -128 x 255)))
                       x))
                   e*)
                 (build-bytevector e*))])

      (define-inline 3 bytevector
        [e* (build-bytevector e*)]))

    (let ()
      (define byte-offset
        (lambda (off)
          (cond
            [(nanopass-case (L7 Expr) off
               [(quote ,d)
                (and (and (integer? d) (exact? d))
                     (let ([n (+ d (constant bytevector-data-disp))])
                       (and (target-fixnum? n)
                            `(quote ,n))))]
               [else #f])]
            [else (%inline + ,off
                     (quote ,(constant bytevector-data-disp)))])))

      (define-inline 3 bytevector-copy!
        [(bv1 off1 bv2 off2 n)
         (%primcall src sexpr $byte-copy! ,bv1 ,(byte-offset off1) ,bv2 ,(byte-offset off2) ,n)]))

    (define-inline 3 bytevector-truncate!
      [(bv len)
       (if (fixnum-constant? len)
           (let ([len (constant-value len)])
             (if (fx= len 0)
                 `(quote ,(bytevector))
                 (bind #t (bv)
                   `(seq
                      (set! ,(%mref ,bv ,(constant bytevector-type-disp))
                        (immediate ,(fx+ (fx* len (constant bytevector-length-factor))
                                         (constant type-bytevector))))
                      ,bv))))
           (bind #t (bv len)
             `(if ,(%inline eq? ,len (immediate 0))
                  (quote ,(bytevector))
                  (seq
                    (set! ,(%mref ,bv ,(constant bytevector-type-disp))
                      ,(build-type/length len
                         (constant type-bytevector)
                         (constant fixnum-offset)
                         (constant bytevector-length-offset)))
                    ,bv))))])

    (define-inline 3 $bytevector-set-immutable!
      [(bv) ((build-set-immutable! bytevector-type-disp bytevector-immutable-flag) bv)])

    (let ()
      (define bv-index-offset
        (lambda (offset-expr)
          (if (fixnum-constant? offset-expr)
              (values %zero (+ (constant bytevector-data-disp) (constant-value offset-expr)))
              (values (build-unfix offset-expr) (constant bytevector-data-disp)))))

      (define bv-offset-okay?
        (lambda (x mask)
          (constant? (lambda (x) (and (target-fixnum? x) (>= x 0) (eq? (logand x mask) 0))) x)))

      (let ()
        (define-syntax define-bv-8-inline
          (syntax-rules ()
            [(_ name type)
             (define-inline 2 name
               [(e-bv e-offset)
                (bind #t (e-bv e-offset)
                  `(if ,(handle-prim #f #f 3 '$bytevector-ref-check? (list `(quote 8) e-bv e-offset))
                       ,(let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                          (build-object-ref #f 'type e-bv e-index imm-offset))
                       ,(build-libcall #t src sexpr name e-bv e-offset)))])]))

        (define-bv-8-inline bytevector-s8-ref integer-8)
        (define-bv-8-inline bytevector-u8-ref unsigned-8))

      (let ()
        (define-syntax define-bv-native-ref-inline
          (lambda (x)
            (syntax-case x ()
              [(_ name type)
               #'(define-inline 3 name
                   [(e-bv e-offset)
                    (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                      (build-object-ref #f 'type e-bv e-index imm-offset))])])))

        (define-bv-native-ref-inline bytevector-s8-ref integer-8)
        (define-bv-native-ref-inline bytevector-u8-ref unsigned-8)

        (define-bv-native-ref-inline bytevector-s16-native-ref integer-16)
        (define-bv-native-ref-inline bytevector-u16-native-ref unsigned-16)

        (define-bv-native-ref-inline bytevector-s32-native-ref integer-32)
        (define-bv-native-ref-inline bytevector-u32-native-ref unsigned-32)

        (define-bv-native-ref-inline bytevector-s64-native-ref integer-64)
        (define-bv-native-ref-inline bytevector-u64-native-ref unsigned-64)

        (define-bv-native-ref-inline bytevector-ieee-single-native-ref single-float)
        (define-bv-native-ref-inline bytevector-ieee-double-native-ref double-float)

        ;; Inline to enable unboxing:
        (define-inline 2 bytevector-ieee-double-native-ref
          [(e-bv e-offset)
           (bind #t (e-bv e-offset)
             (let ([info (make-info-call #f #f #f #f #f)])
               `(if (call ,info ,#f ,(lookup-primref 3 '$bytevector-ref-check?) (quote 64) ,e-bv ,e-offset)
                    (call ,info ,#f ,(lookup-primref 3 'bytevector-ieee-double-native-ref) ,e-bv ,e-offset)
                    ,(build-libcall #t src sexpr bytevector-ieee-double-native-ref e-bv e-offset))))]))

      (let ()
        (define-syntax define-bv-native-int-set!-inline
          (lambda (x)
            (syntax-case x ()
              [(_ check-64? name type)
               (with-syntax ([body #'(let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                                       (build-object-set! 'type e-bv e-index imm-offset e-val))])
                 (with-syntax ([body (if (datum check-64?)
                                         #'(and (>= (constant ptr-bits) 64) body)
                                         #'body)])
                   #'(define-inline 3 name
                       [(e-bv e-offset e-val) body])))])))

        (define-bv-native-int-set!-inline #f bytevector-s8-set! integer-8)
        (define-bv-native-int-set!-inline #f bytevector-u8-set! unsigned-8)
        (define-bv-native-int-set!-inline #f $bytevector-set! unsigned-8)

        (define-bv-native-int-set!-inline #f bytevector-s16-native-set! integer-16)
        (define-bv-native-int-set!-inline #f bytevector-u16-native-set! unsigned-16)

        (define-bv-native-int-set!-inline #f bytevector-s32-native-set! integer-32)
        (define-bv-native-int-set!-inline #f bytevector-u32-native-set! unsigned-32)

        (define-bv-native-int-set!-inline #t bytevector-s64-native-set! integer-64)
        (define-bv-native-int-set!-inline #t bytevector-u64-native-set! unsigned-64))

      (let ()
        (define-syntax define-bv-native-ieee-set!-inline
          (lambda (x)
            (syntax-case x ()
              [(_ name type)
               #'(define-inline 3 name
                   [(e-bv e-offset e-val)
                    (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                      (bind #f (e-bv e-index)
                        (build-object-set! 'type e-bv e-index imm-offset
                          (build-$real->flonum src sexpr e-val `(quote name)))))])])))

        (define-bv-native-ieee-set!-inline bytevector-ieee-single-native-set! single-float)
        (define-bv-native-ieee-set!-inline bytevector-ieee-double-native-set! double-float)

        ;; Inline to enable unboxing:
        (define-inline 2 bytevector-ieee-double-native-set!
          [(e-bv e-offset e-val)
           (bind #t (e-bv e-offset)
             (let ([info (make-info-call #f #f #f #f #f)])
               `(if (call ,info ,#f ,(lookup-primref 3 '$bytevector-set!-check?) (quote 64) ,e-bv ,e-offset)
                    ;; checks to make sure e-val produces a real number:
                    (call ,info ,#f ,(lookup-primref 3 'bytevector-ieee-double-native-set!) ,e-bv ,e-offset ,e-val)
                    ,(build-libcall #t src sexpr bytevector-ieee-double-native-set! e-bv e-offset))))]))

      (let ()
        (define-syntax define-bv-int-ref-inline
          (lambda (x)
            (define p2?
              (lambda (n)
                (let f ([i 1])
                  (or (fx= i n)
                      (and (not (fx> i n)) (f (fxsll i 1)))))))
            (syntax-case x ()
              [(_ name type mask)
               #`(define-inline 3 name
                   [(e-bv e-offset e-eness)
                    (and (or (constant unaligned-integers)
                             (and #,(p2? (fx+ (datum mask) 1)) (bv-offset-okay? e-offset mask)))
                         (constant? (lambda (x) (memq x '(big little))) e-eness)
                         (not (eq? (constant native-endianness) 'unknown))
                         (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                           (build-object-ref (not (eq? (constant-value e-eness) (constant native-endianness)))
                             'type e-bv e-index imm-offset)))])])))

        (define-bv-int-ref-inline bytevector-s16-ref integer-16 1)
        (define-bv-int-ref-inline bytevector-u16-ref unsigned-16 1)

        (when-known-endianness
         (define-bv-int-ref-inline bytevector-s24-ref integer-24 1)
         (define-bv-int-ref-inline bytevector-u24-ref unsigned-24 1))

        (define-bv-int-ref-inline bytevector-s32-ref integer-32 3)
        (define-bv-int-ref-inline bytevector-u32-ref unsigned-32 3)

        (when-known-endianness
         (define-bv-int-ref-inline bytevector-s40-ref integer-40 3)
         (define-bv-int-ref-inline bytevector-u40-ref unsigned-40 3)

         (define-bv-int-ref-inline bytevector-s48-ref integer-48 3)
         (define-bv-int-ref-inline bytevector-u48-ref unsigned-48 3)

         (define-bv-int-ref-inline bytevector-s56-ref integer-56 7)
         (define-bv-int-ref-inline bytevector-u56-ref unsigned-56 7))

        (define-bv-int-ref-inline bytevector-s64-ref integer-64 7)
        (define-bv-int-ref-inline bytevector-u64-ref unsigned-64 7))

      (let ()
        (define-syntax define-bv-ieee-ref-inline
          (lambda (x)
            (syntax-case x ()
              [(_ name type mask)
               #'(define-inline 3 name
                   [(e-bv e-offset e-eness)
                    (and (or (constant unaligned-floats)
                             (bv-offset-okay? e-offset mask))
                         (not (eq? (constant native-endianness) 'unknown))
                         (constant? (lambda (x) (eq? x (constant native-endianness))) e-eness)
                         (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                           (build-object-ref #f 'type e-bv e-index imm-offset)))])])))

        (define-bv-ieee-ref-inline bytevector-ieee-single-ref single-float 3)
        (define-bv-ieee-ref-inline bytevector-ieee-double-ref double-float 7))

      (let ()
        (define-syntax define-bv-int-set!-inline
          (lambda (x)
            (syntax-case x ()
              [(_ check-64? name type mask)
               (with-syntax ([body #'(and (or (constant unaligned-integers)
                                              (and mask (bv-offset-okay? e-offset mask)))
                                          (not (eq? (constant native-endianness) 'unknown))
                                          (constant? (lambda (x) (memq x '(big little))) e-eness)
                                          (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                                            (if (eq? (constant-value e-eness) (constant native-endianness))
                                                (build-object-set! 'type e-bv e-index imm-offset e-value)
                                                (build-swap-object-set! 'type e-bv e-index imm-offset e-value))))])
                 (with-syntax ([body (if (datum check-64?)
                                         #'(and (>= (constant ptr-bits) 64) body)
                                         #'body)])
                   #'(define-inline 3 name
                       [(e-bv e-offset e-value e-eness) body])))])))

        (define-bv-int-set!-inline #f bytevector-s16-set! integer-16 1)
        (define-bv-int-set!-inline #f bytevector-u16-set! unsigned-16 1)

        (define-bv-int-set!-inline #f bytevector-s24-set! integer-24 #f)
        (define-bv-int-set!-inline #f bytevector-u24-set! unsigned-24 #f)

        (define-bv-int-set!-inline #f bytevector-s32-set! integer-32 3)
        (define-bv-int-set!-inline #f bytevector-u32-set! unsigned-32 3)

        (define-bv-int-set!-inline #t bytevector-s40-set! integer-40 #f)
        (define-bv-int-set!-inline #t bytevector-u40-set! unsigned-40 #f)

        (define-bv-int-set!-inline #t bytevector-s48-set! integer-48 #f)
        (define-bv-int-set!-inline #t bytevector-u48-set! unsigned-48 #f)

        (define-bv-int-set!-inline #t bytevector-s56-set! integer-56 #f)
        (define-bv-int-set!-inline #t bytevector-u56-set! unsigned-56 #f)

        (define-bv-int-set!-inline #t bytevector-s64-set! integer-64 7)
        (define-bv-int-set!-inline #t bytevector-u64-set! unsigned-64 7))

      (let ()
        (define-syntax define-bv-ieee-set!-inline
          (lambda (x)
            (syntax-case x ()
              [(_ name type mask)
               #'(define-inline 3 name
                   [(e-bv e-offset e-value e-eness)
                    (and (or (constant unaligned-floats) (bv-offset-okay? e-offset mask))
                         (not (eq? (constant native-endianness) 'unknown))
                         (constant? (lambda (x) (eq? x (constant native-endianness))) e-eness)
                         (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                           (bind #f (e-bv e-index)
                             (build-object-set! 'type e-bv e-index imm-offset
                               (build-$real->flonum src sexpr e-value
                                 `(quote name))))))])])))

        (define-bv-ieee-set!-inline bytevector-ieee-single-set! single-float 3)
        (define-bv-ieee-set!-inline bytevector-ieee-double-set! double-float 7))

      (let ()
        (define anyint-ref-helper
          (lambda (type mask e-bv e-offset e-eness)
            (and (or (constant unaligned-integers) (bv-offset-okay? e-offset mask))
                 (constant? (lambda (x) (memq x '(big little))) e-eness)
                 (not (eq? (constant native-endianness) 'unknown))
                 (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                   (build-object-ref (not (eq? (constant-value e-eness) (constant native-endianness)))
                     type e-bv e-index imm-offset)))))
        (define-syntax define-bv-anyint-ref-inline
          (syntax-rules ()
            [(_ name type8 type16 type32 type64)
             (define-inline 3 name
               [(e-bv e-offset e-eness e-size)
                (and (fixnum-constant? e-size)
                     (case (constant-value e-size)
                       [(1) (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                              `(seq
                                 ,e-eness
                                 ,(build-object-ref #f 'type8 e-bv e-index imm-offset)))]
                       [(2) (anyint-ref-helper 'type16 #b1 e-bv e-offset e-eness)]
                       [(4) (anyint-ref-helper 'type32 #b11 e-bv e-offset e-eness)]
                       [(8) (anyint-ref-helper 'type64 #b111 e-bv e-offset e-eness)]
                       [else #f]))])]))

        (define-bv-anyint-ref-inline bytevector-sint-ref
          integer-8 integer-16 integer-32 integer-64)
        (define-bv-anyint-ref-inline bytevector-uint-ref
          unsigned-8 unsigned-16 unsigned-32 unsigned-64))

      (let ()
        (define anyint-set!-helper
          (lambda (type mask e-bv e-offset e-value e-eness)
            (and (or (constant unaligned-integers) (bv-offset-okay? e-offset mask))
                 (not (eq? (constant native-endianness) 'unknown))
                 (constant? (lambda (x) (memq x '(big little))) e-eness)
                 (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                   (if (eq? (constant-value e-eness) (constant native-endianness))
                       (build-object-set! type e-bv e-index imm-offset e-value)
                       (build-swap-object-set! type e-bv e-index imm-offset e-value))))))
        (define-syntax define-bv-anyint-set!-inline
          (syntax-rules ()
            [(_ name type8 type16 type32 type64)
             (define-inline 3 name
               [(e-bv e-offset e-value e-eness e-size)
                (and (fixnum-constant? e-size)
                     (case (constant-value e-size)
                       [(1) (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                              `(seq
                                 ,e-eness
                                 ,(build-object-set! 'type8 e-bv e-index imm-offset e-value)))]
                       [(2) (anyint-set!-helper 'type16 1 e-bv e-offset e-value e-eness)]
                       [(4) (anyint-set!-helper 'type32 3 e-bv e-offset e-value e-eness)]
                       [(8) (and (>= (constant ptr-bits) 64)
                                 (anyint-set!-helper 'type64 7 e-bv e-offset e-value e-eness))]
                       [else #f]))])]))

        (define-bv-anyint-set!-inline bytevector-sint-set!
          integer-8 integer-16 integer-32 integer-64)
        (define-bv-anyint-set!-inline bytevector-uint-set!
          unsigned-8 unsigned-16 unsigned-32 unsigned-64)))

    (let ()
      (define (byte-count e-n)
        (or (nanopass-case (L7 Expr) e-n
              [(quote ,d)
               (and (and (integer? d) (exact? d))
                    (let ([n (* d (constant string-char-bytes))])
                      (and (target-fixnum? n)
                           `(immediate ,(fix n)))))]
              [else #f])
            (%inline sll ,e-n ,(%constant string-char-offset))))
      (define byte-offset
        (lambda (e-off)
          (or (nanopass-case (L7 Expr) e-off
                [(quote ,d)
                 (and (and (integer? d) (exact? d))
                      (let ([n (+ (* d (constant string-char-bytes))
                                  (constant string-data-disp))])
                        (and (target-fixnum? n)
                             `(immediate ,(fix n)))))]
                [else #f])
              (%inline +
                ,(%inline sll ,e-off ,(%constant string-char-offset))
                (immediate ,(fix (constant string-data-disp)))))))
      (define-inline 3 string-copy!
        [(e-bv1 e-off1 e-bv2 e-off2 e-n)
         (%primcall src sexpr $byte-copy! ,e-bv1 ,(byte-offset e-off1) ,e-bv2 ,(byte-offset e-off2) ,(byte-count e-n))]))

    (define-inline 3 string-truncate!
      [(e-str e-len)
       (if (fixnum-constant? e-len)
           (let ([len (constant-value e-len)])
             (if (fx= len 0)
                 `(quote ,(string))
                 (bind #t (e-str)
                   `(seq
                      (set! ,(%mref ,e-str ,(constant string-type-disp))
                        (immediate ,(fx+ (fx* len (constant string-length-factor))
                                            (constant type-string))))
                      ,e-str))))
           (bind #t (e-str e-len)
             `(if ,(%inline eq? ,e-len (immediate 0))
                  (quote ,(string))
                  (seq
                    (set! ,(%mref ,e-str ,(constant string-type-disp))
                      ,(build-type/length e-len
                         (constant type-string)
                         (constant fixnum-offset)
                         (constant string-length-offset)))
                    ,e-str))))])

    (let ()
      (define build-string-fill
        (make-build-fill (constant string-char-bytes) (constant string-data-disp)))
      (let ()
        (define do-make-string
          (lambda (e-length maybe-e-fill)
            ; NB: caller must bind e-fill
            (safe-assert (or (not maybe-e-fill) (no-need-to-bind? #f maybe-e-fill)))
            (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                (let ([n (constant-value e-length)])
                  (if (fx= n 0)
                      `(quote ,(string))
                      (let ([bytes (fx* n (constant string-char-bytes))])
                        (bind #t ([t (%constant-alloc type-typed-object
                                       (fx+ (constant header-size-string) bytes))])
                          `(seq
                             (set! ,(%mref ,t ,(constant string-type-disp))
                               (immediate ,(fx+ (fx* n (constant string-length-factor))
                                                   (constant type-string))))
                             ,(if maybe-e-fill
                                  (build-string-fill t `(immediate ,bytes) maybe-e-fill)
                                  t))))))
                (bind #t (e-length)
                  (let ([t-bytes (make-tmp 'tsize 'uptr)] [t-str (make-tmp 'tstr)])
                    `(if ,(%inline eq? ,e-length (immediate 0))
                         (quote ,(string))
                         (let ([,t-bytes ,(translate e-length
                                               (constant fixnum-offset)
                                               (constant string-char-offset))])
                           (let ([,t-str (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                           ,(%inline logand
                                              ,(%inline + ,t-bytes
                                                 (immediate ,(fx+ (constant header-size-string)
                                                                  (fx- (constant byte-alignment) 1))))
                                              (immediate ,(- (constant byte-alignment)))))])
                             (seq
                               (set! ,(%mref ,t-str ,(constant string-type-disp))
                                 ,(build-type/length t-bytes
                                    (constant type-string)
                                    (constant string-char-offset)
                                    (constant string-length-offset)))
                               ,(if maybe-e-fill
                                    (build-string-fill t-str t-bytes maybe-e-fill)
                                    t-str))))))))))
        (define default-fill `(immediate ,(ptr->imm #\nul)))
        (define-inline 3 $make-uninitialized-string
          [(e-length) (do-make-string e-length #f)])
        (define-inline 3 make-string
          [(e-length) (do-make-string e-length default-fill)]
          [(e-length e-fill) (bind #t (e-fill) (do-make-string e-length e-fill))])
        (let ()
          (define (valid-length? e-length)
            (constant?
              (lambda (x)
                (and (or (fixnum? x) (bignum? x))
                     (<= 0 x (constant maximum-string-length))))
              e-length))
          (define-inline 2 make-string
            [(e-length)
             (and (valid-length? e-length)
                  (do-make-string e-length default-fill))]
            [(e-length e-fill)
             (and (valid-length? e-length)
                  (constant? char? e-fill)
                  (do-make-string e-length e-fill))])))
      (define-inline 3 string-fill!
        [(e-str e-fill)
         `(seq
            ,(bind #t (e-str e-fill)
               (build-string-fill e-str
                 (translate
                   (%inline logxor
                      ,(%mref ,e-str ,(constant string-type-disp))
                      ,(%constant type-string))
                   (constant string-length-offset)
                   (constant string-char-offset))
                 e-fill))
            ,(%constant svoid))])
      (define-inline 2 string->immutable-string
        [(e-str)
         (nanopass-case (L7 Expr) e-str
           [(quote ,d)
            (guard (string? d) (= 0 (string-length d)))
            `(literal ,(make-info-literal #f 'entry (lookup-c-entry null-immutable-string) 0))]
           [else #f])]))

    (let ()
      (define build-fxvector-fill
        (make-build-fill (constant ptr-bytes) (constant fxvector-data-disp)))
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (let ()
        (define do-make-fxvector
          (lambda (e-length e-fill)
            ; NB: caller must bind e-fill
            (safe-assert (no-need-to-bind? #f e-fill))
            (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                (let ([n (constant-value e-length)])
                  (if (fx= n 0)
                      `(quote ,(fxvector))
                      (let ([bytes (fx* n (constant ptr-bytes))])
                        (bind #t ([t (%constant-alloc type-typed-object
                                       (fx+ (constant header-size-fxvector) bytes))])
                          `(seq
                             (set! ,(%mref ,t ,(constant fxvector-type-disp))
                               (immediate ,(fx+ (fx* n (constant fxvector-length-factor))
                                                   (constant type-fxvector))))
                             ,(build-fxvector-fill t `(immediate ,bytes) e-fill))))))
                (bind #t (e-length) ; fixnum length doubles as byte count
                  (let ([t-fxv (make-tmp 'tfxv)])
                    `(if ,(%inline eq? ,e-length (immediate 0))
                         (quote ,(fxvector))
                         (let ([,t-fxv (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                         ,(%inline logand
                                            ,(%inline + ,e-length
                                               (immediate ,(fx+ (constant header-size-fxvector)
                                                                (fx- (constant byte-alignment) 1))))
                                            (immediate ,(- (constant byte-alignment)))))])
                           (seq
                             (set! ,(%mref ,t-fxv ,(constant fxvector-type-disp))
                               ,(build-type/length e-length
                                  (constant type-fxvector)
                                  (constant fixnum-offset)
                                  (constant fxvector-length-offset)))
                             ,(build-fxvector-fill t-fxv e-length e-fill)))))))))
        (define default-fill `(immediate ,(fix 0)))
        (define-inline 3 make-fxvector
          [(e-length) (do-make-fxvector e-length default-fill)]
          [(e-length e-fill) (bind #t (e-fill) (do-make-fxvector e-length e-fill))])
        (let ()
          (define (valid-length? e-length)
            (constant?
              (lambda (x)
                (and (or (fixnum? x) (bignum? x))
                     (<= 0 x (constant maximum-fxvector-length))))
              e-length))
          (define-inline 2 make-fxvector
            [(e-length)
             (and (valid-length? e-length)
                  (do-make-fxvector e-length default-fill))]
            [(e-length e-fill)
             (and (valid-length? e-length)
                  (constant? fixnum? e-fill)
                  (do-make-fxvector e-length e-fill))])))
      (define-inline 3 fxvector-fill!
        [(e-fxv e-fill)
         `(seq
            ,(bind #t (e-fxv e-fill)
               (build-fxvector-fill e-fxv
                 (translate
                   (%inline logxor
                      ,(%mref ,e-fxv ,(constant fxvector-type-disp))
                      ,(%constant type-fxvector))
                   (constant fxvector-length-offset)
                   (constant fixnum-offset))
                 e-fill))
            ,(%constant svoid))]))

    (let ()
      ;; Used only to fill with 0s:
      (define build-flvector-fill
        (make-build-fill (constant ptr-bytes) (constant flvector-data-disp)))
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (let ()
        (define do-make-flvector
          (lambda (e-length)
            (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                (let ([n (constant-value e-length)])
                  (if (fx= n 0)
                      `(quote ,(flvector))
                      (let ([bytes (fx* n (constant flonum-bytes))])
                        (bind #t ([t (%constant-alloc type-typed-object
                                       (fx+ (constant header-size-flvector) bytes))])
                          `(seq
                             (set! ,(%mref ,t ,(constant flvector-type-disp))
                               (immediate ,(fx+ (fx* n (constant flvector-length-factor))
                                                   (constant type-flvector))))
                             ,(build-flvector-fill t `(immediate ,bytes) `(immediate 0)))))))
                (bind #t (e-length) ; fixnum length doubles as byte count
                  (let ([t-fxv (make-tmp 'tfxv)])
                    `(if ,(%inline eq? ,e-length (immediate 0))
                         (quote ,(flvector))
                         (let ([,t-fxv (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                         ,(%inline logand
                                            ,(%inline + ,(build-double-scale e-length)
                                               (immediate ,(fx+ (constant header-size-flvector)
                                                                (fx- (constant byte-alignment) 1))))
                                            (immediate ,(- (constant byte-alignment)))))])
                           (seq
                             (set! ,(%mref ,t-fxv ,(constant flvector-type-disp))
                               ,(build-type/length e-length
                                  (constant type-flvector)
                                  (constant fixnum-offset)
                                  (constant flvector-length-offset)))
                             ,(build-flvector-fill t-fxv (build-double-scale e-length) `(immediate 0))))))))))
        (define-inline 3 make-flvector
          [(e-length) (do-make-flvector e-length)]
          [(e-length e-init) #f])
        (let ()
          (define (valid-length? e-length)
            (constant?
              (lambda (x)
                (and (or (fixnum? x) (bignum? x))
                     (<= 0 x (constant maximum-flvector-length))))
              e-length))
          (define-inline 2 make-flvector
            [(e-length)
             (and (valid-length? e-length)
                  (do-make-flvector e-length))]
            [(e-length e-init) #f]))))

    (let ()
      (define build-vector-fill
        (make-build-fill (constant ptr-bytes) (constant vector-data-disp)))
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (let ()
        (define do-make-vector
          (lambda (type e-length e-fill)
            ; NB: caller must bind e-fill, if not #f
            (safe-assert (or (not e-fill) (no-need-to-bind? #f e-fill)))
            (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                (let ([n (constant-value e-length)])
                  (if (fx= n 0)
                      `(quote ,(if (fx= type (constant type-vector)) (vector) (immutable-vector)))
                      (let ([bytes (fx* n (constant ptr-bytes))])
                        (bind #t ([t (%constant-alloc type-typed-object
                                       (fx+ (constant header-size-vector) bytes))])
                          `(seq
                             (set! ,(%mref ,t ,(constant vector-type-disp))
                               (immediate ,(+ (fx* n (constant vector-length-factor))
                                              type)))
                             ,(if e-fill
                                  (build-vector-fill t `(immediate ,bytes) e-fill)
                                  t))))))
                (bind #t (e-length) ; fixnum length doubles as byte count
                  (let ([t-vec (make-tmp 'tvec)])
                    `(if ,(%inline eq? ,e-length (immediate 0))
                         (quote ,(if (fx= type (constant type-vector)) (vector) (immutable-vector)))
                         (let ([,t-vec (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                         ,(%inline logand
                                            ,(%inline + ,e-length
                                               (immediate ,(fx+ (constant header-size-vector)
                                                                (fx- (constant byte-alignment) 1))))
                                            (immediate ,(- (constant byte-alignment)))))])
                           (seq
                             (set! ,(%mref ,t-vec ,(constant vector-type-disp))
                               ,(build-type/length e-length
                                  type
                                  (constant fixnum-offset)
                                  (constant vector-length-offset)))
                             ,(if e-fill
                                  (build-vector-fill t-vec e-length e-fill)
                                  t-vec)))))))))
        (define default-fill `(immediate ,(fix 0)))
        (define-inline 3 make-vector
          [(e-length) (do-make-vector (constant type-vector) e-length default-fill)]
          [(e-length e-fill) (bind #t (e-fill) (do-make-vector (constant type-vector) e-length e-fill))])
        (let ()
          (define (extract-vector-length vec)
            (extract-length (%mref ,vec ,(constant vector-type-disp)) (constant vector-length-offset)))
          (define build-vector-copy
            (lambda (type e-vec e-start e-len e-elem prefix-elem? n-elem)
              (let ([Ltop (make-local-label 'Ltop)]
                    [vec (make-tmp 'vec 'ptr)]
                    [t (make-assigned-tmp 't 'uptr)])
                (bind #t (e-vec e-start e-len)
                  `(let ([,t (immediate 0)]
                         [,vec ,(if (not e-elem)
                                    (do-make-vector type e-len #f)
                                    (let ([total-len (make-tmp 'total-len 'uptr)])
                                      `(let ([,total-len ,(%inline + ,e-len ,n-elem)])
                                         ,(do-make-vector type total-len #f))))])
                     (label ,Ltop
                       (if ,(%inline eq? ,t ,e-len)
                           ,(cond
                             [(not e-elem) vec]
                             [(nanopass-case (L7 Expr) n-elem
                                [(immediate ,imm) (guard (eqv? imm (fix 1))) #t]
                                [(quote ,d) (guard (eqv? d 1)) #t]
                                [else #f])
                              (let ([idx (if prefix-elem? `(immediate 0) e-len)])
                                (%seq
                                 (set! ,(%mref ,vec ,idx ,(constant vector-data-disp)) ,e-elem)
                                 ,vec))]
                             [else
                              (let ([Lfill (make-local-label 'Lfill)]
                                    [idx (if prefix-elem?
                                             t
                                             (with-output-language (L7 Expr)
                                               (%inline + ,t ,e-len)))])
                                (%seq
                                 (set! ,t (immediate 0))
                                 (label ,Lfill
                                   (if ,(%inline eq? ,t ,n-elem)
                                       ,vec
                                       ,(%seq
                                         (set! ,(%mref ,vec ,idx ,(constant vector-data-disp)) ,e-elem)
                                         (set! ,t ,(%inline + ,t (immediate ,(constant ptr-bytes))))
                                         (goto ,Lfill))))))])
                           ,(%seq
                             (set! ,(let ([idx (if prefix-elem?
                                                   (with-output-language (L7 Expr)
                                                     (%inline + ,t ,n-elem))
                                                   t)])
                                      (%mref ,vec ,idx ,(constant vector-data-disp)))
                                   ,(%mref ,e-vec ,(%inline + ,t ,e-start) ,(constant vector-data-disp)))
                             (set! ,t ,(%inline + ,t (immediate ,(constant ptr-bytes))))
                             (goto ,Ltop)))))))))
          (define build-immutable-vector-copy
            (lambda (vec)
              (bind #t (vec)
                `(if ,(%typed-object-check mask-mutable-vector type-immutable-vector ,vec)
                     ,vec
                     ,(build-vector-copy (constant type-immutable-vector) vec
                                         `(immediate ,(fix 0)) (extract-vector-length vec) #f #f #f)))))
          (define build-vector-set/copy
            (lambda (type e-vec e-idx e-val)
              (let ([Ltop (make-local-label 'Ltop)]
                    [vec (make-tmp 'vec 'ptr)]
                    [t (make-assigned-tmp 't 'uptr)])
                (bind #t (e-vec e-idx)
                  (bind #f (e-val)
                    `(let ([,t ,(extract-vector-length e-vec)])
                       (let ([,vec ,(do-make-vector type t #f)])
                         (label ,Ltop
                           (if ,(%inline eq? ,t (immediate 0))
                               ,(%seq
                                 (set! ,(%mref ,vec ,e-idx ,(constant vector-data-disp)) ,e-val)
                                 ,vec)
                               ,(%seq
                                 (set! ,t ,(%inline - ,t (immediate ,(constant ptr-bytes))))
                                 (set! ,(%mref ,vec ,t ,(constant vector-data-disp))
                                       ,(%mref ,e-vec ,t ,(constant vector-data-disp)))
                                 (goto ,Ltop)))))))))))
          (define build-vector-append
            (lambda (type e-vecs)
              (let loop ([e-vecs e-vecs] [len `(immediate 0)])
                (cond
                  [(null? e-vecs)
                   (do-make-vector type len #f)]
                  [else
                   (let ([Ltop (make-local-label 'Ltop)]
                         [d-vec (make-tmp 'd-vec 'ptr)]
                         [e-vec (car e-vecs)]
                         [t (make-tmp 't 'uptr)]
                         [e-len (make-assigned-tmp 'e-len 'uptr)])
                     (bind #t (e-vec)
                       `(let ([,t ,len]
                              [,e-len ,(extract-vector-length e-vec)])
                          (let ([,d-vec ,(loop (cdr e-vecs) (%inline + ,t ,e-len))])
                            (label ,Ltop
                                   (if ,(%inline eq? ,e-len (immediate 0))
                                       ,d-vec
                                       ,(%seq
                                         (set! ,e-len ,(%inline - ,e-len (immediate ,(constant ptr-bytes))))
                                         (set! ,(%mref ,d-vec ,(%inline + ,t ,e-len) ,(constant vector-data-disp))
                                               ,(%mref ,e-vec ,e-len ,(constant vector-data-disp)))
                                         (goto ,Ltop))))))))]))))
          (define (okay-make-vector? pr e1)
            (and (eq? (primref-name pr) 'make-vector)
                 (or (>= (primref-level pr) 3)
                     (constant? (lambda (x) (and (target-fixnum? x) (>= x 0))) e1))))
          (define build-vector-append-two
            (lambda (type vec1 vec2)
              (nanopass-case (L7 Expr) vec1
                [(call ,info1 ,mdcl1 ,pr ,e1)
                 (guard (memq (primref-name pr) '(vector immutable-vector)))
                 (bind #f (e1)
                   (bind #t (vec2)
                     (build-vector-copy type vec2 `(immediate ,(fix 0)) (extract-vector-length vec2) e1 #t `(immediate ,(fix 1)))))]
                [(call ,info1 ,mdcl1 ,pr ,e1 ,e2)
                 (guard (okay-make-vector? pr e1))
                 (bind #t (e1 e2 vec2)
                   (build-vector-copy type vec2 `(immediate ,(fix 0)) (extract-vector-length vec2) e2 #t e1))]
                [else
                 (nanopass-case (L7 Expr) vec2
                   [(call ,info2 ,mdcl2 ,pr ,e2)
                    (guard (memq (primref-name pr) '(vector immutable-vector)))
                    (bind #t (vec1)
                      (bind #f (e2)
                        (build-vector-copy type vec1 `(immediate ,(fix 0)) (extract-vector-length vec1) e2 #f `(immediate ,(fix 1)))))]
                   [(call ,info2 ,mdcl2 ,pr ,e1 ,e2)
                    (guard (okay-make-vector? pr e1))
                    (bind #t (e1 e2 vec1)
                      (build-vector-copy type vec1 `(immediate ,(fix 0)) (extract-vector-length vec1) e2 #f e1))]
                   [else (build-vector-append type (list vec1 vec2))])])))
          (define-inline 3 vector-copy
            [(vec) (bind #t (vec)
                     (build-vector-copy (constant type-vector) vec `(immediate ,(fix 0)) (extract-vector-length vec) #f #f #f))]
            [(vec start len) (build-vector-copy (constant type-vector) vec start len #f #f #f)])
          (define-inline 3 immutable-vector-copy
            [(vec) (build-immutable-vector-copy vec)]
            [(vec start len) (build-vector-copy (constant type-immutable-vector) vec start len #f #f #f)])
          (define-inline 3 vector->immutable-vector
            [(vec) (build-immutable-vector-copy vec)])
          (define-inline 3 vector-set/copy
            [(vec idx val) (build-vector-set/copy (constant type-vector) vec idx val)])
          (define-inline 3 immutable-vector-set/copy
            [(vec idx val) (build-vector-set/copy (constant type-immutable-vector) vec idx val)])
          (define-inline 3 vector-append
            [() `(quote ,(vector))]
            [(vec1 vec2) (build-vector-append-two (constant type-vector) vec1 vec2)]
            [(vec . vecs) (build-vector-append (constant type-vector) (cons vec vecs))])
          (define-inline 3 immutable-vector-append
            [() `(quote ,(immutable-vector))]
            [(vec) (build-immutable-vector-copy vec)]
            [(vec1 vec2) (build-vector-append-two (constant type-immutable-vector) vec1 vec2)]
            [(vec . vecs) (build-vector-append (constant type-immutable-vector) (cons vec vecs))]))
        (let ()
          (define (valid-length? e-length)
            (constant?
              (lambda (x) (and (target-fixnum? x) (>= x 0)))
              e-length))
          (define-inline 2 make-vector
            [(e-length)
             (and (valid-length? e-length)
                  (do-make-vector (constant type-vector) e-length default-fill))]
            [(e-length e-fill)
             (and (valid-length? e-length)
                  (constant? fixnum? e-fill)
                  (do-make-vector (constant type-vector) e-length e-fill))]))
        (define-inline 2 vector->immutable-vector
          [(e-vec)
           (nanopass-case (L7 Expr) e-vec
             [(quote ,d)
              (guard (vector? d) (fx= 0 (vector-length d)))
              `(literal ,(make-info-literal #f 'entry (lookup-c-entry null-immutable-vector) 0))]
             [else #f])])))

    (let ()
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (let ()
        (define build-stencil-vector-type
          (lambda (e-mask type) ; e-mask is used only once
            (%inline logor
                     (immediate ,type)
                     ,(%inline sll ,e-mask (immediate ,(fx- (constant stencil-vector-mask-offset)
                                                            (constant fixnum-offset)))))))
        (define do-stencil-vector
          (lambda (e-mask e-val* type)
            (list-bind #f (e-val*)
              (bind #f (e-mask)
                  (let ([t-vec (make-tmp 'tvec)])
                    `(let ([,t-vec ,(%constant-alloc type-typed-object
                                                     (fx+ (constant header-size-stencil-vector)
                                                          (fx* (length e-val*) (constant ptr-bytes))))])
                       ,(let loop ([e-val* e-val*] [i 0])
                          (if (null? e-val*)
                              `(seq
                                 (set! ,(%mref ,t-vec ,(constant stencil-vector-type-disp))
                                       ,(build-stencil-vector-type e-mask type))
                                 ,t-vec)
                              `(seq
                                (set! ,(%mref ,t-vec ,(fx+ i (constant stencil-vector-data-disp))) ,(car e-val*))
                                ,(loop (cdr e-val*) (fx+ i (constant ptr-bytes))))))))))))
        (define do-make-stencil-vector
          (lambda (e-length e-mask type)
            (bind #t (e-length)
                  (bind #f (e-mask)
                        (let ([t-vec (make-tmp 'tvec)])
                          `(let ([,t-vec (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                                ,(%inline logand
                                                    ,(%inline + ,e-length
                                                         (immediate ,(fx+ (constant header-size-stencil-vector)
                                                                          (fx- (constant byte-alignment) 1))))
                                                     (immediate ,(- (constant byte-alignment)))))])
                           ,(%seq
                             (set! ,(%mref ,t-vec ,(constant stencil-vector-type-disp))
                                   ,(build-stencil-vector-type e-mask type))
                             ;; Content not filled! This function is meant to be called by
                             ;; `[$]$stencil-vector-do-update`, which has GC disabled between
                             ;; allocation and filling in the data
                             ,t-vec)))))))
        (define-inline 3 stencil-vector
          [(e-mask . e-val*)
           (do-stencil-vector e-mask e-val* (constant type-stencil-vector))])
        (define-inline 3 $system-stencil-vector
          [(e-mask . e-val*)
           (do-stencil-vector e-mask e-val* (constant type-sys-stencil-vector))])
        (define-inline 2 $make-stencil-vector
          [(e-length e-mask) (do-make-stencil-vector e-length e-mask (constant type-stencil-vector))])
        (define-inline 2 $make-system-stencil-vector
          [(e-length e-mask) (do-make-stencil-vector e-length e-mask (constant type-sys-stencil-vector))])
        (define-inline 3 $make-stencil-vector
          [(e-length e-mask) (do-make-stencil-vector e-length e-mask (constant type-stencil-vector))])
        (define-inline 3 $make-system-stencil-vector
          [(e-length e-mask) (do-make-stencil-vector e-length e-mask (constant type-sys-stencil-vector))])
        (define-inline 3 stencil-vector-update
          [(e-vec e-sub-mask e-add-mask . e-val*)
           `(call ,(make-info-call src sexpr #f #f #f) #f
                  ,(lookup-primref 3 '$stencil-vector-do-update)
                  ,e-vec ,e-sub-mask ,e-add-mask ,e-val* ...)])
        (define-inline 3 $system-stencil-vector-update
          [(e-vec e-sub-mask e-add-mask . e-val*)
           `(call ,(make-info-call src sexpr #f #f #f) #f
                  ,(lookup-primref 3 '$system-stencil-vector-do-update)
                  ,e-vec ,e-sub-mask ,e-add-mask ,e-val* ...)])
        (define-inline 3 stencil-vector-truncate!
          [(e-vec e-mask)
           (bind #f (e-vec e-mask)
             `(seq
               (set! ,(%mref ,e-vec ,(constant stencil-vector-type-disp))
                     ,(build-stencil-vector-type e-mask (constant type-stencil-vector)))
               ,(%constant svoid)))])
        (define-inline 3 $system-stencil-vector-truncate!
          [(e-vec e-mask)
           (bind #f (e-vec e-mask)
             `(seq
               (set! ,(%mref ,e-vec ,(constant stencil-vector-type-disp))
                     ,(build-stencil-vector-type e-mask (constant type-sys-stencil-vector)))
               ,(%constant svoid)))])))
    (let ()
      (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
      (define-inline 3 $make-eqhash-vector
        [(e-length)
         (let ([t-vec (make-tmp 'tvec)]
               [t-idx (make-assigned-tmp 't-idx)]
               [Ltop (make-local-label 'Ltop)])
           `(let ([,t-idx ,e-length])
              (if ,(%inline eq? ,t-idx (immediate 0))
                  (quote ,(vector))
                  (let ([,t-vec (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                  ,(%inline logand
                                     ,(%inline + ,t-idx
                                        (immediate ,(fx+ (constant header-size-vector)
                                                      (fx- (constant byte-alignment) 1))))
                                     (immediate ,(- (constant byte-alignment)))))])
                    (seq
                      (set! ,(%mref ,t-vec ,(constant vector-type-disp))
                        ,(build-type/length t-idx
                           (constant type-vector)
                           (constant fixnum-offset)
                           (constant vector-length-offset)))
                      (label ,Ltop
                        ,(%seq
                           (set! ,t-idx ,(%inline - ,t-idx (immediate ,(fix 1))))
                           (set! ,(%mref ,t-vec ,t-idx ,(constant vector-data-disp)) ,t-idx)
                           (if ,(%inline eq? ,t-idx (immediate 0))
                               ,t-vec
                               (goto ,Ltop)))))))))]))

    (let ()
      (define build-continuation?-test
        (lambda (e) ; e must be bound
          (build-and
           (%type-check mask-closure type-closure ,e)
           (%type-check mask-continuation-code type-continuation-code
             ,(%mref
               ,(%inline -
                  ,(%mref ,e ,(constant closure-code-disp))
                  ,(%constant code-data-disp))
               ,(constant code-type-disp))))))
      (define-inline 2 $continuation?
        [(e) (bind #t (e)
               (build-continuation?-test e))])
      (define-inline 2 $assert-continuation
        [(e) (bind #t (e)
               `(if ,(build-and
                      (build-continuation?-test e)
                      (%inline eq? ,(%mref ,e ,(constant continuation-winders-disp)) ,(%tc-ref winders)))
                    ,(%constant svoid)
                    ,(build-libcall #t src sexpr $check-continuation e (%constant sfalse) (%constant sfalse))))]
        [(e1 e2) (bind #t (e1 e2)
                   `(if ,(build-and
                          (build-continuation?-test e1)
                          (build-and
                           (%inline eq? ,(%mref ,e1 ,(constant continuation-winders-disp)) ,(%tc-ref winders))
                           (build-simple-or
                            (%inline eq? ,e2 ,(%mref ,e1 ,(constant continuation-attachments-disp)))
                            (build-and
                             (%type-check mask-pair type-pair ,e2)
                             (%inline eq? ,(%mref ,e2 ,(constant pair-cdr-disp)) ,(%mref ,e1 ,(constant continuation-attachments-disp)))))))
                        ,(%constant svoid)
                        ,(build-libcall #t src sexpr $check-continuation e1 (%constant strue) e2)))])
      (define-inline 3 $assert-continuation
        [(e) (bind #t (e)
               `(if ,(%inline eq? ,(%mref ,e ,(constant continuation-winders-disp)) ,(%tc-ref winders))
                    ,(%constant svoid)
                    ,(build-libcall #t src sexpr $check-continuation e (%constant sfalse) (%constant sfalse))))]
        [(e1 e2) #f]))

    (define-inline 3 $continuation-stack-length
      [(e)
       (translate (%mref ,e ,(constant continuation-stack-length-disp))
         (constant fixnum-offset)
         (constant log2-ptr-bytes))])
    (define-inline 3 $continuation-stack-clength
      [(e)
       (translate (%mref ,e ,(constant continuation-stack-clength-disp))
         (constant fixnum-offset)
         (constant log2-ptr-bytes))])
    (let ()
      (define (build-ra e)
        (%mref ,e ,(constant continuation-return-address-disp)))
      (define (build-stack-ra e-k e-i)
        (%mref ,(%mref ,e-k ,(constant continuation-stack-disp))
               ,(translate e-i (constant fixnum-offset) (constant log2-ptr-bytes))
               0))
      
      (define build-return-code
        (lambda (e-ra)
          (safe-assert (= (constant compact-return-address-toplink-disp)
                          (constant return-address-toplink-disp)))
          (bind #t ([ra e-ra])
            (bind #t ([t (%inline + ,ra ,(%constant compact-return-address-toplink-disp))])
              (%inline - ,t ,(%mref ,t 0))))))
      (define build-return-offset
        (lambda (e-ra)
          (bind #t ([ra e-ra])
            (build-fix
             `(if ,(%inline logtest ,(%mref ,ra ,(constant compact-return-address-mask+size+mode-disp))
                            ,(%constant compact-header-mask))
                  ,(%inline - ,(%mref ,ra ,(constant compact-return-address-toplink-disp))
                            ,(%constant compact-return-address-toplink-disp))
                  ,(%inline - ,(%mref ,ra ,(constant return-address-toplink-disp))
                            ,(%constant return-address-toplink-disp)))))))
      (define build-return-livemask
        (lambda (e-ra)
          (bind #t ([ra e-ra])
            (bind #t ([mask+size+mode (%mref ,ra ,(constant compact-return-address-mask+size+mode-disp))])
              `(if ,(%inline logtest ,mask+size+mode ,(%constant compact-header-mask))
                   ,(%inline sll ,(%inline srl ,mask+size+mode ,(%constant compact-frame-mask-offset))
                             ,(%constant fixnum-offset))
                   ,(%mref ,ra ,(constant return-address-livemask-disp)))))))
      (define build-return-frame-words
        (lambda (e-ra)
          (bind #t ([ra e-ra])
            (bind #t ([mask+size+mode (%mref ,ra ,(constant compact-return-address-mask+size+mode-disp))])
              `(if ,(%inline logtest ,mask+size+mode ,(%constant compact-header-mask))
                   ,(%inline sll ,(%inline logand ,(%inline srl ,mask+size+mode ,(%constant compact-frame-words-offset))
                                           ,(%constant compact-frame-words-mask))
                             ,(%constant fixnum-offset))
                   ,(%mref ,ra ,(constant return-address-frame-size-disp)))))))
      
      (define-inline 3 $continuation-return-code
        [(e) (build-return-code (build-ra e))])
      (define-inline 3 $continuation-return-offset
        [(e) (build-return-offset (build-ra e))])
      (define-inline 3 $continuation-return-livemask
        [(e) (build-return-livemask (build-ra e))])
      (define-inline 3 $continuation-return-frame-words
        [(e) (build-return-frame-words (build-ra e))])
      (define-inline 3 $continuation-stack-ref
        [(e-k e-i)
         (%mref
          ,(%mref ,e-k ,(constant continuation-stack-disp))
          ,(translate e-i (constant fixnum-offset) (constant log2-ptr-bytes))
          0)])
      (define-inline 3 $continuation-stack-return-code
        [(e-k e-i) (build-return-code (build-stack-ra e-k e-i))])
      (define-inline 3 $continuation-stack-return-offset
        [(e-k e-i) (build-return-offset (build-stack-ra e-k e-i))])
      (define-inline 3 $continuation-stack-return-frame-words
        [(e-k e-i) (build-return-frame-words (build-stack-ra e-k e-i))]))

    (define-inline 2 $foreign-char?
      [(e)
       (bind #t (e)
         (build-and
           (%type-check mask-char type-char ,e)
           (%inline < ,e (immediate ,(ptr->imm (integer->char #x100))))))])
    (define-inline 2 $foreign-wchar?
      [(e)
       (constant-case wchar-bits
         [(16)
          (bind #t (e)
            (build-and
              (%type-check mask-char type-char ,e)
              (%inline < ,e (immediate ,(ptr->imm (integer->char #x10000))))))]
         [(32) (%type-check mask-char type-char ,e)])])
    (define-inline 2 $integer-8?
      [(e)
       (unless (fx>= (constant fixnum-bits) 8) ($oops '$integer-8? "unexpected fixnum-bits"))
       (bind #t (e)
         (build-and
           (%type-check mask-fixnum type-fixnum ,e)
           (%inline u<
              ,(%inline + ,e (immediate ,(fix #x80)))
              (immediate ,(fix #x180)))))])
    (define-inline 2 $integer-16?
      [(e)
       (unless (fx>= (constant fixnum-bits) 16) ($oops '$integer-16? "unexpected fixnum-bits"))
       (bind #t (e)
         (build-and
           (%type-check mask-fixnum type-fixnum ,e)
           (%inline u<
              ,(%inline + ,e (immediate ,(fix #x8000)))
              (immediate ,(fix #x18000)))))])
    (define-inline 2 $integer-24?
      [(e)
       (unless (fx>= (constant fixnum-bits) 24) ($oops '$integer-24? "unexpected fixnum-bits"))
       (bind #t (e)
         (build-and
           (%type-check mask-fixnum type-fixnum ,e)
           (%inline u<
              ,(%inline + ,e (immediate ,(fix #x800000)))
              (immediate ,(fix #x1800000)))))])
    (define-inline 2 $integer-32?
      [(e)
       (bind #t (e)
         (if (fx>= (constant fixnum-bits) 32)
             (build-and
               (%type-check mask-fixnum type-fixnum ,e)
               (%inline u<
                  ,(%inline + ,e (immediate ,(fix #x80000000)))
                  (immediate ,(fix #x180000000))))
             (build-simple-or
               (%type-check mask-fixnum type-fixnum ,e)
               (build-and
                 (%type-check mask-typed-object type-typed-object ,e)
                 (bind #t ([t (%mref ,e ,(constant bignum-type-disp))])
                   `(if ,(%type-check mask-signed-bignum type-positive-bignum ,t)
                        ,(build-libcall #f #f sexpr <= e `(quote #xffffffff))
                        ,(build-and
                           (%type-check mask-signed-bignum type-negative-bignum ,t)
                           (build-libcall #f #f sexpr >= e `(quote #x-80000000)))))))))])
    (define-inline 2 $integer-40?
      [(e)
       (bind #t (e)
         (if (fx>= (constant fixnum-bits) 32)
             (build-and
               (%type-check mask-fixnum type-fixnum ,e)
               (%inline u<
                  ,(%inline + ,e (immediate ,(fix #x8000000000)))
                  (immediate ,(fix #x18000000000))))
             (build-simple-or
               (%type-check mask-fixnum type-fixnum ,e)
               (build-and
                 (%type-check mask-typed-object type-typed-object ,e)
                 (bind #t ([t (%mref ,e ,(constant bignum-type-disp))])
                   `(if ,(%type-check mask-signed-bignum type-positive-bignum ,t)
                        ,(build-libcall #f #f sexpr <= e `(quote #xffffffffff))
                        ,(build-and
                           (%type-check mask-signed-bignum type-negative-bignum ,t)
                           (build-libcall #f #f sexpr >= e `(quote #x-8000000000)))))))))])
    (define-inline 2 $integer-48?
      [(e)
       (bind #t (e)
         (if (fx>= (constant fixnum-bits) 32)
             (build-and
               (%type-check mask-fixnum type-fixnum ,e)
               (%inline u<
                  ,(%inline + ,e (immediate ,(fix #x800000000000)))
                  (immediate ,(fix #x1800000000000))))
             (build-simple-or
               (%type-check mask-fixnum type-fixnum ,e)
               (build-and
                 (%type-check mask-typed-object type-typed-object ,e)
                 (bind #t ([t (%mref ,e ,(constant bignum-type-disp))])
                   `(if ,(%type-check mask-signed-bignum type-positive-bignum ,t)
                        ,(build-libcall #f #f sexpr <= e `(quote #xffffffffffff))
                        ,(build-and
                           (%type-check mask-signed-bignum type-negative-bignum ,t)
                           (build-libcall #f #f sexpr >= e `(quote #x-800000000000)))))))))])
    (define-inline 2 $integer-56?
      [(e)
       (bind #t (e)
         (if (fx>= (constant fixnum-bits) 32)
             (build-and
               (%type-check mask-fixnum type-fixnum ,e)
               (%inline u<
                  ,(%inline + ,e (immediate ,(fix #x80000000000000)))
                  (immediate ,(fix #x180000000000000))))
             (build-simple-or
               (%type-check mask-fixnum type-fixnum ,e)
               (build-and
                 (%type-check mask-typed-object type-typed-object ,e)
                 (bind #t ([t (%mref ,e ,(constant bignum-type-disp))])
                   `(if ,(%type-check mask-signed-bignum type-positive-bignum ,t)
                        ,(build-libcall #f #f sexpr <= e `(quote #xffffffffffffff))
                        ,(build-and
                           (%type-check mask-signed-bignum type-negative-bignum ,t)
                           (build-libcall #f #f sexpr >= e `(quote #x-80000000000000)))))))))])
    (define-inline 2 $integer-64?
      [(e)
       (when (fx>= (constant fixnum-bits) 64) ($oops '$integer-64? "unexpected fixnum-bits"))
       (bind #t (e)
         (build-simple-or
           (%type-check mask-fixnum type-fixnum ,e)
           (build-and
             (%type-check mask-typed-object type-typed-object ,e)
             (bind #t ([t (%mref ,e ,(constant bignum-type-disp))])
               `(if ,(%type-check mask-signed-bignum type-positive-bignum ,t)
                    ,(build-libcall #f #f sexpr <= e `(quote #xffffffffffffffff))
                    ,(build-and
                       (%type-check mask-signed-bignum type-negative-bignum ,t)
                       (build-libcall #f #f sexpr >= e `(quote #x-8000000000000000))))))))])
    (define-inline 3 char->integer
      ; assumes types are set up so that fixnum tag will be right after the shift
      [(e-char) (build-char->integer e-char)])
    (define-inline 2 char->integer
      ; assumes types are set up so that fixnum tag will be right after the shift
      [(e-char)
       (bind #t (e-char)
         `(if ,(%type-check mask-char type-char ,e-char)
              ,(%inline srl ,e-char
                 (immediate ,(fx- (constant char-data-offset) (constant fixnum-offset))))
              ,(build-libcall #t src sexpr char->integer e-char)))])
    (define-inline 3 char-
      ; assumes fixnum is zero
      [(e1 e2)
       (%inline sra
          ,(%inline - ,e1 ,e2)
          (immediate ,(fx- (constant char-data-offset) (constant fixnum-offset))))])
    (define-inline 3 integer->char
      [(e-int) (build-integer->char e-int)])
    (define-inline 3 boolean=?
      [(e1 e2) (%inline eq? ,e1 ,e2)]
      [(e1 e2 . e*) (reduce-equality src sexpr moi e1 e2 e*)])
    (define-inline 3 symbol=?
      [(e1 e2) (%inline eq? ,e1 ,e2)]
      [(e1 e2 . e*) (reduce-equality src sexpr moi e1 e2 e*)])
    (let ()
      (define (go e flag)
        (%inline logtest
           ,(%mref ,e ,(constant record-type-flags-disp))
           (immediate ,(fix flag))))
      (define-inline 3 record-type-opaque?
        [(e) (go e (constant rtd-opaque))])
      (define-inline 3 record-type-sealed?
        [(e) (go e (constant rtd-sealed))])
      (define-inline 3 $record-type-act-sealed?
        [(e) (go e (fxior (constant rtd-sealed) (constant rtd-act-sealed)))])
      (define-inline 3 record-type-generative?
        [(e) (go e (constant rtd-generative))]))
    (let ()
      (define build-record?
        (lambda (e)
          (bind #t (e)
            (build-and
              (%type-check mask-typed-object type-typed-object ,e)
              (bind #t ([t (%mref ,e ,(constant typed-object-type-disp))])
                (build-and
                  (%type-check mask-record type-record ,t)
                  (build-not
                    (%inline logtest
                      ,(%mref ,t ,(constant record-type-flags-disp))
                      (immediate ,(fix (constant rtd-opaque)))))))))))
      (define build-sealed-isa?
        (lambda (e e-rtd assume-record?)
          (bind #t (e)
            (bind #f (e-rtd)
              (maybe-build-and
                (and (not assume-record?)
                     (%type-check mask-typed-object type-typed-object ,e))
                (%inline eq?
                  ,(%mref ,e ,(constant typed-object-type-disp))
                  ,e-rtd))))))
      (define build-unsealed-isa?
        (lambda (e e-rtd assume-record?)
          (let ([known-depth (nanopass-case (L7 Expr) e-rtd
                               [(quote ,d) (and (record-type-descriptor? d)
                                                (vector-length (rtd-ancestry d)))]
                               [else #f])])
            ;; `t` is rtd of `e`, and it's used once
            (define (compare-at-depth e-rtd t known-depth)
              (cond
                [(eqv? known-depth (constant minimum-ancestry-vector-length))
                 ;; no need to check ancestry array length
                 (%inline eq? ,e-rtd ,(%mref ,(%mref ,t ,(constant record-type-ancestry-disp))
                                             ,(fx+ (constant vector-data-disp)
                                                   (fx* (fx- known-depth 1) (constant ptr-bytes)))))]
                [known-depth
                 ;; need to check ancestry array length
                 (let ([a (make-tmp 'a)])
                   `(let ([,a ,(%mref ,t ,(constant record-type-ancestry-disp))])
                      (if ,(%inline <=
                                    (immediate ,(fxsll known-depth (constant vector-length-offset)))
                                    ,(%mref ,a ,(constant vector-type-disp)))
                          ,(%inline eq? ,e-rtd ,(%mref ,a ,(fx+ (constant vector-data-disp)
                                                                (fx* (fx- known-depth 1) (constant ptr-bytes)))))
                          ,(%constant sfalse))))]
                [else
                 (bind #t (e-rtd)
                   (let ([a (make-tmp 'a)] [rtd-a (make-tmp 'rtd-a)] [rtd-len (make-tmp 'rtd-len)])
                     `(let ([,rtd-a ,(%mref ,e-rtd ,(constant record-type-ancestry-disp))])
                        (let ([,a ,(%mref ,t ,(constant record-type-ancestry-disp))])
                          (let ([,rtd-len ,(%mref ,rtd-a ,(constant vector-type-disp))])
                            (if ,(%inline <= ,rtd-len ,(%mref ,a ,(constant vector-type-disp)))
                                ,(begin
                                   ;; take advantage of being able to use the type field of a vector
                                   ;; as a pointer offset with just shifting:
                                   (safe-assert (zero? (constant type-vector)))
                                   (%inline eq? ,e-rtd ,(%mref ,a
                                                               ,(translate rtd-len (constant vector-length-offset) (constant log2-ptr-bytes))
                                                               ,(fx- (constant vector-data-disp) (constant ptr-bytes)))))
                                ,(%constant sfalse)))))))]))
            (cond
              [assume-record?
               (compare-at-depth e-rtd (%mref ,e ,(constant typed-object-type-disp)) known-depth)]
              [else
               (let ([t (make-tmp 't)])
                 (bind #t (e e-rtd) ;; also bind e-rtd to maintain applicative order in case `and` short-circuits
                   (build-and
                    (%type-check mask-typed-object type-typed-object ,e)
                    `(let ([,t ,(%mref ,e ,(constant typed-object-type-disp))])
                       ,(build-and
                         (%type-check mask-record type-record ,t)
                         (compare-at-depth e-rtd t known-depth))))))]))))
      (define-inline 3 record?
        [(e) (build-record? e)]
        [(e e-rtd)
         (if (constant? (lambda (x)
                          (and (record-type-descriptor? x)
                               (record-type-sealed? x)))
               e-rtd)
             (build-sealed-isa? e e-rtd #f)
             (build-unsealed-isa? e e-rtd #f))])
      (define-inline 3 record-instance?
        [(e e-rtd)
         (if (constant? (lambda (x)
                          (and (record-type-descriptor? x)
                               (record-type-sealed? x)))
               e-rtd)
             (build-sealed-isa? e e-rtd #t)
             (build-unsealed-isa? e e-rtd #t))])
      (define-inline 2 r6rs:record?
        [(e) (build-record? e)])
      (define-inline 2 record?
        [(e) (build-record? e)]
        [(e e-rtd)
         (nanopass-case (L7 Expr) e-rtd
           [(quote ,d)
            (and (record-type-descriptor? d)
                 (if (record-type-sealed? d)
                     (build-sealed-isa? e e-rtd #f)
                     (build-unsealed-isa? e e-rtd #f)))]
           [else #f])])
      (define-inline 2 $sealed-record?
        [(e e-rtd) (build-sealed-isa? e e-rtd #f)])
      (define-inline 2 $sealed-record-instance?
        [(e e-rtd) (build-sealed-isa? e e-rtd #t)])
      (define-inline 3 $record-type-field-count
        [(e) (%inline srl ,(%inline - ,(%mref ,e ,(constant record-type-size-disp))
                                    (immediate ,(fxsll (fx- (constant record-data-disp) (constant record-type-disp))
                                                       (constant fixnum-offset))))
                      ,(%constant log2-ptr-bytes))])
      (define-inline 2 eq-hashtable?
        [(e) (let ([rtd (let () (include "hashtable-types.ss") (record-type-descriptor eq-ht))])
               (let ([e-rtd `(quote ,rtd)])
                 (if (record-type-sealed? rtd)
                     (build-sealed-isa? e e-rtd #f)
                     (build-unsealed-isa? e e-rtd #f))))]))
    (define-inline 2 gensym?
      [(e)
       (bind #t (e)
         (build-and
           (%type-check mask-symbol type-symbol ,e)
           (bind #t ([t (%mref ,e ,(constant symbol-name-disp))])
             `(if ,t
                  ,(build-and (%type-check mask-pair type-pair ,t)
                              (build-and (%mref ,t ,(constant pair-cdr-disp))
                                         (%constant strue)))
                  ,(%constant strue)))))])
    (define-inline 2 uninterned-symbol?
      [(e)
       (bind #t (e)
         (build-and
           (%type-check mask-symbol type-symbol ,e)
           (bind #t ([t (%mref ,e ,(constant symbol-name-disp))])
                 (build-and (%type-check mask-pair type-pair ,t)
                            (build-not (%mref ,t ,(constant pair-cdr-disp)))))))])
    (let ()
      (define build-make-symbol
        (lambda (e-name)
          (bind #t ([t (%constant-alloc type-symbol (constant size-symbol))])
            (%seq
              (set! ,(%mref ,t ,(constant symbol-name-disp)) ,e-name)
              (set! ,(%mref ,t ,(constant symbol-value-disp)) ,(%constant sunbound))
              (set! ,(%mref ,t ,(constant symbol-pvalue-disp))
                (literal
                  ,(make-info-literal #f 'library
                     (lookup-libspec nonprocedure-code)
                     (constant code-data-disp))))
              (set! ,(%mref ,t ,(constant symbol-plist-disp)) ,(%constant snil))
              (set! ,(%mref ,t ,(constant symbol-splist-disp)) ,(%constant snil))
              (set! ,(%mref ,t ,(constant symbol-hash-disp)) ,(%constant sfalse))
              ,t))))
      (define (go e-pname)
        (bind #t ([t (%constant-alloc type-pair (constant size-pair))])
          (%seq
            (set! ,(%mref ,t ,(constant pair-cdr-disp)) ,e-pname)
            (set! ,(%mref ,t ,(constant pair-car-disp)) ,(%constant sfalse))
            ,(build-make-symbol t))))
      (define-inline 3 $gensym
        [() (build-make-symbol (%constant sfalse))]
        [(e-pname) (bind #f (e-pname) (go e-pname))]
        [(e-pname e-uname) #f])
      (define-inline 3 gensym
        [() (build-make-symbol (%constant sfalse))]
        [(e-pname) (and (constant? immutable-string? e-pname) (go e-pname))]
        [(e-pname e-uname) #f])
      (define-inline 2 gensym
        [() (build-make-symbol (%constant sfalse))]
        [(e-pname) (and (constant? immutable-string? e-pname) (go e-pname))]
        [(e-pname e-uname) #f]))
    (define-inline 3 symbol->string
      [(e-sym)
       (bind #t (e-sym)
         (bind #t ([e-name (%mref ,e-sym ,(constant symbol-name-disp))])
           `(if ,e-name
                (if ,(%type-check mask-pair type-pair ,e-name)
                    ,(bind #t ([e-cdr (%mref ,e-name ,(constant pair-cdr-disp))])
                           `(if ,e-cdr
                                ,e-cdr
                                ,(%mref ,e-name ,(constant pair-car-disp))))
                    ,e-name)
                ,(%primcall #f sexpr $gensym->pretty-name ,e-sym))))])
    (define-inline 3 $fxaddress
      [(e) (%inline logand
              ,(let ([n (- (constant primary-type-bits) (constant fixnum-offset))])
                 (if (> n 0) (%inline sra ,e (immediate ,n)) e))
              (immediate ,(- (constant fixnum-factor))))])
    (define-inline 3 $set-timer
      [(e) (bind #f (e)
             (bind #t ([t (build-fix (ref-reg %trap))])
               `(seq
                  (set! ,(ref-reg %trap) ,(build-unfix e))
                  ,t)))])
    (define-inline 3 $get-timer
      [() (build-fix (ref-reg %trap))])
    (constant-case architecture
      [(pb) (void)]
      [else
       (define-inline 3 directory-separator?
         [(e) (if-feature windows
                (bind #t (e)
                  (build-simple-or
                   (%inline eq? ,e (immediate ,(ptr->imm #\/)))
                   (%inline eq? ,e (immediate ,(ptr->imm #\\)))))
                (%inline eq? ,e (immediate ,(ptr->imm #\/))))])])
    (let ()
      (define add-cdrs
        (lambda (n e)
          (if (fx= n 0)
              e
              (add-cdrs (fx- n 1) (%mref ,e ,(constant pair-cdr-disp))))))
      (define-inline 3 list-ref
        [(e-ls e-n)
         (nanopass-case (L7 Expr) e-n
           [(quote ,d)
            (and (and (fixnum? d) (fx< d 4))
                 (%mref ,(add-cdrs d e-ls) ,(constant pair-car-disp)))]
           [else #f])])
      (define-inline 3 list-tail
        [(e-ls e-n)
         (nanopass-case (L7 Expr) e-n
           [(quote ,d) (and (and (fixnum? d) (fx<= d 4)) (add-cdrs d e-ls))]
           [else #f])]))
    (let ()
      (define (go0 src sexpr subtype)
        (%primcall src sexpr $make-eq-hashtable
          (immediate ,(fix (constant hashtable-default-size)))
          (immediate ,(fix subtype))))
      (define (go1 src sexpr e-size subtype)
        (nanopass-case (L7 Expr) e-size
          [(quote ,d)
           ; d must be a fixnum? for $hashtable-size-minlen and a
           ; target-machine fixnum for cross compiling
           (and (and (fixnum? d) (target-fixnum? d) (fx>= d 0))
                (%primcall src sexpr $make-eq-hashtable
                  (immediate ,(fix ($hashtable-size->minlen d)))
                  (immediate ,(fix subtype))))]
          [else #f]))
      (define-inline 3 make-eq-hashtable
        [() (go0 src sexpr (constant eq-hashtable-subtype-normal))]
        [(e-size) (go1 src sexpr e-size (constant eq-hashtable-subtype-normal))])
      (define-inline 3 make-weak-eq-hashtable
        [() (go0 src sexpr (constant eq-hashtable-subtype-weak))]
        [(e-size) (go1 src sexpr e-size (constant eq-hashtable-subtype-weak))])
      (define-inline 3 make-ephemeron-eq-hashtable
        [() (go0 src sexpr (constant eq-hashtable-subtype-ephemeron))]
        [(e-size) (go1 src sexpr e-size (constant eq-hashtable-subtype-ephemeron))]))
    (let ()
      (define-syntax def-put-x
        (syntax-rules ()
          [(_ name x-length)
           (define-inline 3 name
             [(e-bop e-x)
              (bind #t (e-x)
                (build-libcall #f src sexpr name e-bop e-x `(immediate 0)
                  (handle-prim #f #f 3 'x-length (list e-x))))]
             [(e-bop e-x e-start)
              (bind #t (e-x e-start)
                (build-libcall #f src sexpr name e-bop e-x e-start
                  (%inline -
                     ,(handle-prim #f #f 3 'x-length (list e-x))
                     ,e-start)))]
             [(e-bop e-x e-start e-count)
              (build-libcall #f src sexpr name e-bop e-x e-start e-count)])]))
      (def-put-x put-bytevector bytevector-length)
      (def-put-x put-bytevector-some bytevector-length)
      (def-put-x put-string string-length)
      (def-put-x put-string-some string-length))

    (define-inline 3 $read-time-stamp-counter
      [()
       (constant-case architecture
         [(x86)
          (%seq
            ; returns low-order 32 bits in eax, high-order in edx
            (set! ,%eax (inline ,(make-info-kill* (reg-list %edx)) ,%read-time-stamp-counter))
            ,(u32xu32->ptr %edx %eax))]
         [(x86_64)
          (%seq
            ; returns low-order 32 bits in rax, high-order in rdx
            (set! ,%rax (inline ,(make-info-kill* (reg-list %rdx)) ,%read-time-stamp-counter))
            ,(unsigned->ptr
               (%inline logor ,(%inline sll ,%rdx (immediate 32)) ,%rax)
               64))]
         [(arm32 pb) (unsigned->ptr (%inline read-time-stamp-counter) 32)]
         [(arm64 riscv64 loongarch64) (unsigned->ptr (%inline read-time-stamp-counter) 64)]
         [(ppc32)
          (let ([t-hi (make-tmp 't-hi)])
            `(let ([,t-hi (inline ,(make-info-kill* (reg-list %real-zero))
                            ,%read-time-stamp-counter)])
               ,(u32xu32->ptr t-hi %real-zero)))])])

    (define-inline 3 $read-performance-monitoring-counter
      [(e)
       (constant-case architecture
         [(x86)
          (%seq
            (set! ,%eax (inline ,(make-info-kill* (reg-list %edx)) ,%read-performance-monitoring-counter ,(build-unfix e)))
            ,(u32xu32->ptr %edx %eax))]
         [(x86_64)
          (%seq
            (set! ,%rax (inline ,(make-info-kill* (reg-list %rdx)) ,%read-performance-monitoring-counter ,(build-unfix e)))
            ,(unsigned->ptr
               (%inline logor ,(%inline sll ,%rdx (immediate 32)) ,%rax)
               64))]
         [(arm32 ppc32 pb) (unsigned->ptr (%inline read-performance-monitoring-counter ,(build-unfix e)) 32)]
         [(arm64 riscv64 loongarch64) (unsigned->ptr (%inline read-performance-monitoring-counter ,(build-unfix e)) 64)])])

    (define-inline 3 assert-unreachable
      [() (%constant svoid)])

    )) ; expand-primitives module

(set! $np-expand-primitives np-expand-primitives)
)
