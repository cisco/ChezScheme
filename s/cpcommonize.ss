;;; cpcommonize.ss
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

(begin
(define-who commonization-level
  ($make-thread-parameter
    0
    (lambda (x)
      (unless (and (fixnum? x) (<= 0 x 9))
        ($oops who "invalid level ~s" x))
      x)))

(define $cpcommonize
  (let ()
    (import (nanopass))
    (include "base-lang.ss")

    (define-record-type binding
      (nongenerative)
      (sealed #t)
      (fields x (mutable e) size helper-box (mutable helper-b) (mutable helper-arg*))
      (protocol
        (lambda (new)
          (lambda (x e size helper-box)
            (new x e size helper-box #f #f)))))

    (define-language Lcommonize1 (extends Lsrc)
      (terminals
        (+ (fixnum (size))))
      (Expr (e body rtd-expr)
        (- (letrec ([x* e*] ...) body))
        (+ (letrec ([x* e* size] ...) body))))

    (define-language Lcommonize2 (extends Lcommonize1)
      (terminals
        (- (fixnum (size)))
        (+ (binding (b helper-b))))
      (Expr (e body rtd-expr)
        (- (letrec ([x* e* size] ...) body))
        (+ (letrec (helper-b* ...) (b* ...) body))))

    (define-syntax iffalse
      (syntax-rules ()
        [(_ e1 e2) e1 #;(or e1 (begin e2 #f))]))

    (define-syntax iftrue
      (syntax-rules ()
        [(_ e1 e2) e1 #;(let ([t e1]) (and t (begin e2 t)))]))

    (define Lcommonize1-lambda?
      (lambda (e)
        (nanopass-case (Lcommonize1 Expr) e
          [(case-lambda ,preinfo ,cl* ...) #t]
          [else #f])))

    (define-pass cpcommonize0 : Lsrc (ir) -> Lcommonize1 ()
      (Expr : Expr (ir) -> Expr (1)
        [(set! ,maybe-src ,x ,[e size])
         (values `(set! ,maybe-src ,x ,e) (fx+ 1 size))]
        [(seq ,[e1 size1] ,[e2 size2])
         (values `(seq ,e1 ,e2) (fx+ size1 size2))]
        [(if ,[e1 size1] ,[e2 size2] ,[e3 size3])
         (values `(if ,e1 ,e2 ,e3) (fx+ size1 size2 size3))]
        [(foreign (,conv* ...) ,name ,[e size] (,arg-type* ...) ,result-type)
         (values `(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type) (fx+ 1 size))]
        [(fcallable (,conv* ...) ,[e size] (,arg-type* ...) ,result-type)
         (values `(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type) (fx+ 1 size))]
        ; ($top-level-value 'x) adds just 1 to the size
        [(call ,preinfo ,pr (quote ,d))
         (guard (eq? (primref-name pr) '$top-level-value))
         (values `(call ,preinfo ,pr (quote ,d)) 1)]
        ; (let ([x* e*] ...) body) splits into letrec binding unassigned variables to lambdas plus a let for the remaining bindings
        [(call ,preinfo1 (case-lambda ,preinfo2 (clause (,x* ...) ,interface ,[body size])) ,[e* -> e* size*] ...)
         (guard (fx= (length e*) interface))
         (define-record-type fudge (nongenerative) (sealed #t) (fields x e size))
         (let-values ([(lb* ob*) (partition
                                   (lambda (b)
                                     (and (not (prelex-assigned (fudge-x b)))
                                          (Lcommonize1-lambda? (fudge-e b))))
                                   (map make-fudge x* e* size*))])
           (values
             (let ([body (if (null? ob*)
                             body
                             `(call ,preinfo1
                                (case-lambda ,preinfo2
                                  (clause (,(map fudge-x ob*) ...) ,(length ob*) ,body))
                                ,(map fudge-e ob*) ...))])
               (if (null? lb*)
                   body
                   `(letrec ([,(map fudge-x lb*) ,(map fudge-e lb*) ,(map fudge-size lb*)] ...) ,body)))
             (apply fx+ size size*)))]
        [(call ,preinfo ,[e size] ,[e* size*] ...)
         (values `(call ,preinfo ,e ,e* ...) (apply fx+ size size*))]
        [(case-lambda ,preinfo (clause (,x** ...) ,interface* ,[body* size*]) ...)
         (values `(case-lambda ,preinfo (clause (,x** ...) ,interface* ,body*) ...) (apply fx+ 1 size*))]
        [(letrec ([,x* ,[e* size*]] ...) ,[body size])
         (values `(letrec ([,x* ,e* ,size*] ...) ,body) (apply fx+ size size*))]
        [(record-ref ,rtd ,type ,index ,[e size])
         (values `(record-ref ,rtd ,type ,index ,e) (fx+ size 1))]
        [(record-set! ,rtd ,type ,index ,[e1 size1] ,[e2 size2])
         (values `(record-set! ,rtd ,type ,index ,e1 ,e2) (fx+ size1 size2 1))]
        [(record ,rtd ,[rtd-expr size] ,[e* size*] ...)
         (values `(record ,rtd ,rtd-expr ,e* ...) (apply fx+ size size*))]
        [(cte-optimization-loc ,box ,[e size])
         (values `(cte-optimization-loc ,box ,e) size)]
        [(immutable-list (,[e* size*] ...) ,[e size])
         (values `(immutable-list (,e* ...) ,e) (apply fx+ size size*))]
        [(quote ,d) (values `(quote ,d) 1)]
        [(ref ,maybe-src ,x) (values `(ref ,maybe-src ,x) 1)]
        [,pr (values pr 1)]
        [(moi) (values `(moi) 1)]
        [(pariah) (values `(pariah) 0)]
        [(profile ,src) (values `(profile ,src) 0)]
        [else (sorry! who "unhandled record ~s" ir)])
      (let-values ([(e size) (Expr ir)]) e))

    (define-pass cpcommonize1 : Lcommonize1 (ir worthwhile-size) -> Lcommonize2 ()
      (definitions
        (define worthwhile-size?
          (lambda (expr-size)
            (fx>= expr-size worthwhile-size)))
        (define worthwhile-ratio?
          (lambda (expr-size subst-count)
            (or (fx= subst-count 0)
                (fx>= (div expr-size subst-count) 4))))
        (define-record-type subst
          (nongenerative)
          (sealed #t)
          (fields t e1 e2))
        (define-record-type frob
          (nongenerative)
          (sealed #t)
          (fields subst* e b))
        (define ht (make-hashtable values fx=))
        (define make-sym
          (lambda x*
            (string->symbol (apply string-append (map (lambda (x) (if (prelex? x) (symbol->string (prelex-name x)) x)) x*)))))
        (define same-preinfo?
          (lambda (p1 p2)
            ; ignore differences in src and sexpr
            #t))
        (define same-preinfo-lambda?
          (lambda (p1 p2)
            ; ignore differences src, sexpr, and name
            (eq? (preinfo-lambda-libspec p1) (preinfo-lambda-libspec p2))))
        (define-who same-type?
          (lambda (ty1 ty2)
            (nanopass-case (Ltype Type) ty1
              [(fp-integer ,bits1)
               (nanopass-case (Ltype Type) ty2
                 [(fp-integer ,bits2) (= bits1 bits2)]
                 [else #f])]
              [(fp-unsigned ,bits1)
               (nanopass-case (Ltype Type) ty2
                 [(fp-unsigned ,bits2) (= bits1 bits2)]
                 [else #f])]
              [(fp-void)
               (nanopass-case (Ltype Type) ty2
                 [(fp-void) #t]
                 [else #f])]
              [(fp-scheme-object)
               (nanopass-case (Ltype Type) ty2
                 [(fp-scheme-object) #t]
                 [else #f])]
              [(fp-u8*)
               (nanopass-case (Ltype Type) ty2
                 [(fp-u8*) #t]
                 [else #f])]
              [(fp-u16*)
               (nanopass-case (Ltype Type) ty2
                 [(fp-u16*) #t]
                 [else #f])]
              [(fp-u32*)
               (nanopass-case (Ltype Type) ty2
                 [(fp-u32*) #t]
                 [else #f])]
              [(fp-fixnum)
               (nanopass-case (Ltype Type) ty2
                 [(fp-fixnum) #t]
                 [else #f])]
              [(fp-double-float)
               (nanopass-case (Ltype Type) ty2
                 [(fp-double-float) #t]
                 [else #f])]
              [(fp-single-float)
               (nanopass-case (Ltype Type) ty2
                 [(fp-single-float) #t]
                 [else #f])]
              [(fp-ftd ,ftd1)
               (nanopass-case (Ltype Type) ty2
                 [(fp-ftd ,ftd2) (eq? ftd1 ftd2)]
                 [else #f])]
              [else (sorry! who "unhandled foreign type ~s" ty1)])))
        (define okay-to-subst?
          (lambda (e)
            (define free?
              (lambda (x)
                (and (not (prelex-operand x)) #t)))
            (nanopass-case (Lcommonize1 Expr) e
              [(ref ,maybe-src1 ,x1) (and (not (prelex-assigned x1)) (free? x1))]
              [(quote ,d) #t]
              [,pr (all-set? (prim-mask proc) (primref-flags pr))]
              [else #f])))
        (define constant-equal?
          (lambda (x y)
            (define record-equal?
              (lambda (x y e?)
                (let ([rtd ($record-type-descriptor x)])
                  (and (eq? ($record-type-descriptor y) rtd)
                       (let f ([field-name* (csv7:record-type-field-names rtd)] [i 0])
                         (or (null? field-name*)
                             (and (let ([accessor (csv7:record-field-accessor rtd i)])
                                    (e? (accessor x) (accessor y)))
                                  (f (cdr field-name*) (fx+ i 1)))))))))
            (parameterize ([default-record-equal-procedure record-equal?])
              ; equal? should be okay since even mutable constants aren't supposed to be mutated
              (equal? x y))))
        (define same?
          (lambda (e1 e2)
            (nanopass-case (Lcommonize1 Expr) e1
              [(ref ,maybe-src1 ,x1)
               (nanopass-case (Lcommonize1 Expr) e2
                 [(ref ,maybe-src2 ,x2)
                  (or (eq? x1 x2)
                      (eq? (prelex-operand x1) x2))]
                 [else #f])]
              [(quote ,d1)
               (nanopass-case (Lcommonize1 Expr) e2
                 [(quote ,d2) (constant-equal? d1 d2)]
                 [else #f])]
              [,pr1
                (nanopass-case (Lcommonize1 Expr) e2
                  [,pr2 (eq? pr1 pr2)]
                  [else #f])]
              [(moi)
               (nanopass-case (Lcommonize1 Expr) e2
                 [(moi) #t]
                 [else #f])]
              [(pariah)
               (nanopass-case (Lcommonize1 Expr) e2
                 [(pariah) #t]
                 [else #f])]
              [(profile ,src1)
               (nanopass-case (Lcommonize1 Expr) e2
                 [(profile ,src2) (eq? src1 src2)]
                 [else #f])]
              [(call ,preinfo1 ,pr1 (quote ,d1))
               (guard (eq? (primref-name pr1) '$top-level-value))
               (nanopass-case (Lcommonize1 Expr) e2
                 [(call ,preinfo2 ,pr2 (quote ,d2))
                  (guard (eq? (primref-name pr2) '$top-level-value))
                  (and (same-preinfo? preinfo1 preinfo2) (eq? d1 d2))]
                 [else #f])]
              [else #f])))
        (define-who unify
          (lambda (e1 e2)
            (module (with-env)
              (define $with-env
                (lambda (x1* x2* th)
                  (dynamic-wind
                    (lambda () (map (lambda (x1 x2) (prelex-operand-set! x1 x2) (prelex-operand-set! x2 #t)) x1* x2*))
                    th
                    (lambda () (map (lambda (x1 x2) (prelex-operand-set! x1 #f) (prelex-operand-set! x2 #f)) x1* x2*)))))
              (define-syntax with-env
                (syntax-rules ()
                  [(_ x1* x2* e) ($with-env x1* x2* (lambda () e))])))
            (call/cc
              (lambda (return)
                (let ([subst* '()])
                  (define lookup-subst
                    (lambda (e1 e2)
                      (define same-subst?
                        (lambda (x)
                          (and (same? (subst-e1 x) e1) (same? (subst-e2 x) e2))))
                      (cond
                        [(find same-subst? subst*) =>
                         (lambda (subst)
                           (let ([t (subst-t subst)])
                             (set-prelex-multiply-referenced! t #t)
                             t))]
                        [else #f])))
                  (let ([e (with-output-language (Lcommonize1 Expr)
                             (let ()
                               (define fclause
                                 (lambda (cl1 cl2)
                                   (nanopass-case (Lcommonize1 CaseLambdaClause) cl1
                                     [(clause (,x1* ...) ,interface1 ,body1)
                                      (nanopass-case (Lcommonize1 CaseLambdaClause) cl2
                                        [(clause (,x2* ...) ,interface2 ,body2)
                                         (if (fx= interface1 interface2)
                                             (with-env x1* x2*
                                               (with-output-language (Lcommonize1 CaseLambdaClause)
                                                 `(clause (,x1* ...) ,interface1 ,(f body1 body2))))
                                             (return (iffalse #f (printf "lambda interfaces don't match\n")) '()))])])))
                               (define f
                                 (case-lambda
                                   [(e1 e2) (f e1 e2 #f)]
                                   [(e1 e2 call-position?)
                                    (or (cond
                                          [(same? e1 e2) e1]
                                          [(and (not call-position?) (okay-to-subst? e1) (okay-to-subst? e2))
                                           `(ref #f ,(or (lookup-subst e1 e2)
                                                         (let ([t (make-prelex*)])
                                                           (set-prelex-referenced! t #t)
                                                           (set! subst* (cons (make-subst t e1 e2) subst*))
                                                           t)))]
                                          [else
                                            (nanopass-case (Lcommonize1 Expr) e1
                                              [(ref ,maybe-src1 ,x1) #f]
                                              [(quote ,d) #f]
                                              [,pr #f]
                                              [(moi) #f]
                                              [(profile ,src1) #f]
                                              ; reject non-same top-level-value calls with constant symbol so they
                                              ; don't end up being abstracted over the symbol in the residual code
                                              [(call ,preinfo ,pr (quote ,d))
                                               (guard (eq? (primref-name pr) '$top-level-value))
                                               #f]
                                              ; don't allow abstraction of first (type) argument to $object-ref, foreign-ref, etc.,
                                              ; since they can't be inlined without a constant type.
                                              ; ditto for $tc-field's first (field) argument.
                                              ; there are many other primitives we don't catch here for which the compiler generates
                                              ; more efficient code when certain arguments  are constant.
                                              [(call ,preinfo1 ,pr1 (quote ,d1) ,e1* ...)
                                               (guard (memq (primref-name pr1) '($object-ref $swap-object-ref $object-set foreign-ref foreign-set! $tc-field)))
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(call ,preinfo2 ,pr2 (quote ,d2) ,e2* ...)
                                                  (guard (eq? pr2 pr1) (eq? d1 d2))
                                                  (and (same-preinfo? preinfo1 preinfo2)
                                                       (fx= (length e1*) (length e2*))
                                                       `(call ,preinfo1 ,pr1 (quote ,d1) ,(map f e1* e2*) ...))]
                                                 [else #f])]
                                              [(call ,preinfo1 ,e1 ,e1* ...)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(call ,preinfo2 ,e2 ,e2* ...)
                                                  (and (fx= (length e1*) (length e2*))
                                                       (same-preinfo? preinfo1 preinfo2)
                                                       `(call ,preinfo1 ,(f e1 e2 #t) ,(map f e1* e2*) ...))]
                                                 [else #f])]
                                              [(if ,e10 ,e11 ,e12)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(if ,e20 ,e21 ,e22)
                                                  `(if ,(f e10 e20) ,(f e11 e21) ,(f e12 e22))]
                                                 [else #f])]
                                              [(case-lambda ,preinfo1 ,cl1* ...)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(case-lambda ,preinfo2 ,cl2* ...)
                                                  (and (fx= (length cl1*) (length cl2*))
                                                       (same-preinfo-lambda? preinfo1 preinfo2)
                                                       `(case-lambda ,preinfo1 ,(map fclause cl1* cl2*) ...))]
                                                 [else #f])]
                                              [(seq ,e11 ,e12)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(seq ,e21 ,e22) `(seq ,(f e11 e21) ,(f e12 e22))]
                                                 [else #f])]
                                              [(set! ,maybe-src1 ,x1 ,e1)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(set! ,maybe-src2 ,x2 ,e2)
                                                  (and (eq? x1 x2)
                                                       `(set! ,maybe-src1 ,x1 ,(f e1 e2)))]
                                                 [else #f])]
                                              [(letrec ([,x1* ,e1* ,size1*] ...) ,body1)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(letrec ([,x2* ,e2* ,size2*] ...) ,body2)
                                                  (and (fx= (length x2*) (length x1*))
                                                       (andmap fx= size1* size2*)
                                                       (with-env x1* x2*
                                                         `(letrec ([,x1* ,(map f e1* e2*) ,size1*] ...) ,(f body1 body2))))]
                                                 [else #f])]
                                              [(foreign (,conv1* ...) ,name1 ,e1 (,arg-type1* ...) ,result-type1)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(foreign (,conv2* ...) ,name2 ,e2 (,arg-type2* ...) ,result-type2)
                                                  (and (equal? conv1* conv2*)
                                                       (equal? name1 name2)
                                                       (fx= (length arg-type1*) (length arg-type2*))
                                                       (andmap same-type? arg-type1* arg-type2*)
                                                       (same-type? result-type1 result-type2)
                                                       `(foreign (,conv1* ...) ,name1 ,(f e1 e2) (,arg-type1* ...) ,result-type1))]
                                                 [else #f])]
                                              [(fcallable (,conv1* ...) ,e1 (,arg-type1* ...) ,result-type1)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(fcallable (,conv2* ...) ,e2 (,arg-type2* ...) ,result-type2)
                                                  (and (equal? conv1* conv2*)
                                                       (fx= (length arg-type1*) (length arg-type2*))
                                                       (andmap same-type? arg-type1* arg-type2*)
                                                       (same-type? result-type1 result-type2)
                                                       `(fcallable (,conv1* ...) ,(f e1 e2) (,arg-type1* ...) ,result-type1))]
                                                 [else #f])]
                                              [(cte-optimization-loc ,box1 ,e1)
                                               (nanopass-case (Lcommonize1 Expr) e2
                                                 [(cte-optimization-loc ,box2 ,e2)
                                                  (and (eq? box1 box2)
                                                       `(cte-optimization-loc ,box1 ,(f e1 e2)))]
                                                 [else #f])]
                                              [else (sorry! who "unhandled record ~s" e1)])])
                                        (return (iffalse #f (parameterize ([print-level 3] [print-length 5]) (printf "unify failed for ~s and ~s (call-position ~s)\n" e1 e2 call-position?))) '()))]))
                               (f e1 e2)))])
                    (values e subst*)))))))
        (define sort-substs
          ; reestablish original argument order for substituted variables where possible
          ; so the arguments to an abstracted procedure aren't shuffled around in the
          ; call to the generated helper.
          (lambda (subst0* x1* x2*)
            (define (this? x x*) (and (not (null? x*)) (eq? x (car x*))))
            (define (next x*) (if (null? x*) x* (cdr x*)))
            (let-values ([(new-subst* subst*) (let f ([x1* x1*] [x2* x2*] [subst* subst0*] [n (length subst0*)])
                                                (cond
                                                  [(fx= n 0) (values '() subst*)]
                                                  [(find (lambda (subst)
                                                           (define (is-this-arg? e x*)
                                                             (nanopass-case (Lcommonize1 Expr) e
                                                               [(ref ,maybe-src ,x) (this? x x*)]
                                                               [else #f]))
                                                           (or (is-this-arg? (subst-e1 subst) x1*)
                                                               (is-this-arg? (subst-e2 subst) x2*)))
                                                     subst*) =>
                                                   (lambda (subst)
                                                     (let-values ([(new-subst* subst*) (f (next x1*) (next x2*) (remq subst subst*) (fx- n 1))])
                                                       (values (cons subst new-subst*) subst*)))]
                                                  [else
                                                    (let-values ([(new-subst* subst*) (f (next x1*) (next x2*) subst* (fx- n 1))])
                                                      (values (cons (car subst*) new-subst*) (cdr subst*)))]))])
              (safe-assert (null? subst*))
              (safe-assert (fx= (length new-subst*) (length subst0*)))
              new-subst*)))
        (define find-match
          (lambda (b1 ht)
            (and (iffalse (worthwhile-size? (binding-size b1)) (printf "skipping b1: under worthwhile size ~s ~s\n" (binding-size b1) worthwhile-size))
                 (ormap (lambda (b2)
                          (iffalse #f (printf "checking ~s & ~s:" (prelex-name (binding-x b1)) (prelex-name (binding-x b2))))
                          (nanopass-case (Lcommonize1 Expr) (binding-e b1)
                            ; NB: restricting to one clause for now...handling multiple
                            ; NB: clauses should be straightforward with a helper per
                            ; NB: common clause.
                            [(case-lambda ,preinfo1 (clause (,x1* ...) ,interface1 ,body1))
                             ; NB: no rest interface for now.  should be straightforward
                             (guard (fxnonnegative? interface1))
                             (and 
                               (nanopass-case (Lcommonize1 Expr) (binding-e b2)
                                 [(case-lambda ,preinfo2 (clause (,x2* ...) ,interface2 ,body2))
                                  (guard (fxnonnegative? interface2))
                                  (let-values ([(e subst*) (unify body1 body2)])
                                    (and e
                                         (iffalse (worthwhile-ratio? (binding-size b1) (length subst*)) (printf " no, not worthwhile ratio ~s ~s\n" (binding-size b1) (length subst*)))
                                         (let ([subst* (sort-substs subst* x1* x2*)])
                                           (iffalse #f (printf " yes\n"))
                                           (make-frob subst* e b2))))]
                                 [else (iffalse #f (printf " no, b2 does not meet lambda restrictions\n"))]))]
                            [else (iffalse #f (printf " no, b1 does not meet lambda restrictions\n"))]))
                   (hashtable-ref ht (binding-size b1) '())))))
        (define record-helper!
          (lambda (b next e*)
            (binding-helper-b-set! b next)
            (binding-helper-arg*-set! b e*)))
        (define build-helper
          (lambda (t t* body size helper-box)
            (make-binding t
              (with-output-language (Lcommonize1 Expr)
                `(case-lambda ,(make-preinfo-lambda) (clause (,t* ...) ,(length t*) ,body)))
              size
              helper-box)))
        (define commonize-letrec
          (lambda (x* e* size* body) ; e* and body have not been processed
            (define (prune-and-process! b)
              (let ([b* (remq b (hashtable-ref ht (binding-size b) '()))])
                (if (null? b*)
                    (hashtable-delete! ht (binding-size b))
                    (hashtable-set! ht (binding-size b) b*)))
              (unless (binding-helper-b b) (binding-e-set! b (Expr (binding-e b)))))
            (if (null? x*)
                body
                (let ([helper-box (box '())])
                  (let ([b* (map (lambda (x e size) (make-binding x e size helper-box)) x* e* size*)])
                    (let ([body (let f ([b* b*])
                                  (if (null? b*)
                                      (Expr body)
                                      (let ([b (car b*)])
                                        (let ([frob (find-match b ht)])
                                          (if frob
                                              (let* ([outer-b (frob-b frob)]
                                                     [helper-box (binding-helper-box outer-b)]
                                                     [helper-b (let ([t (make-prelex* (make-sym (binding-x b) "&" (binding-x outer-b)))])
                                                                 (build-helper t (map subst-t (frob-subst* frob)) (frob-e frob) (binding-size outer-b) helper-box))])
                                                (set-box! helper-box (cons helper-b (unbox helper-box)))
                                                (record-helper! b helper-b (map subst-e1 (frob-subst* frob)))
                                                (record-helper! outer-b helper-b (map subst-e2 (frob-subst* frob)))
                                                (hashtable-update! ht (binding-size outer-b) (lambda (b*) (cons helper-b (remq outer-b b*))) '())
                                                (f (cdr b*)))
                                              (begin
                                                (hashtable-update! ht (binding-size b) (lambda (b*) (cons b b*)) '())
                                                (let ([body (f (cdr b*))])
                                                  (prune-and-process! b)
                                                  body)))))))])
                      (let ([helper-b* (unbox helper-box)])
                        (for-each prune-and-process! helper-b*)
                        (with-output-language (Lcommonize2 Expr)
                          `(letrec (,helper-b* ...) (,b* ...) ,body))))))))))
      (Expr : Expr (ir) -> Expr ()
        [(letrec ([,x* ,e* ,size*] ...) ,body)
         ; only unassigned lambda bindings post-cpletrec
         (safe-assert (andmap (lambda (x) (not (prelex-assigned x))) x*))
         (safe-assert (andmap (lambda (e) (Lcommonize1-lambda? e)) e*))
         (commonize-letrec x* e* size* body)]
        [(letrec* ([,x* ,e*] ...) ,body)
         ; no letrec* run post-cpletrec
         (assert #f)]))

    (define-pass cpcommonize2 : Lcommonize2 (ir) -> Lsrc ()
      (definitions
        (define build-caller
          (lambda (e helper-b helper-arg*)
            (define-who Arg
              (lambda (e)
               (with-output-language (Lsrc Expr)
                 (nanopass-case (Lcommonize1 Expr) e
                   [(ref ,maybe-src ,x) `(ref ,maybe-src ,x)]
                   [(quote ,d) `(quote ,d)]
                   [else (sorry! who "unexpected helper arg ~s" e)]))))
            (define propagate
              (lambda (alist)
                (lambda (e)
                  (nanopass-case (Lsrc Expr) e
                    [(ref ,maybe-src ,x)
                     (cond
                       [(assq x alist) => cdr]
                       [else e])]
                    [else e]))))
            (nanopass-case (Lcommonize1 Expr) e
              [(case-lambda ,preinfo (clause (,x* ...) ,interface ,body))
               (with-output-language (Lsrc Expr)
                 `(case-lambda ,preinfo
                    (clause (,x* ...) ,interface
                      ,(let loop ([helper-b helper-b] [e* (map Arg helper-arg*)])
                         (if (binding-helper-b helper-b)
                             (nanopass-case (Lcommonize1 Expr) (binding-e helper-b)
                               [(case-lambda ,preinfo (clause (,x* ...) ,interface ,body))
                                (loop (binding-helper-b helper-b) (map (propagate (map cons x* e*)) (map Arg (binding-helper-arg* helper-b))))])
                             `(call ,(make-preinfo)
                                ,(let ([t (binding-x helper-b)])
                                   (if (prelex-referenced t)
                                       (set-prelex-multiply-referenced! t #t)
                                       (set-prelex-referenced! t #t))
                                   `(ref #f ,t))
                                ,e* ...))))))])))
        (define maybe-build-caller
          (lambda (b)
            (let ([helper-b (binding-helper-b b)] [e (binding-e b)])
              (if helper-b
                  (build-caller e helper-b (binding-helper-arg* b))
                  (Expr e))))))
      (Expr : Expr (ir) -> Expr ()
        [(letrec (,helper-b* ...) (,b* ...) ,[body])
         (let loop ([rb* (reverse helper-b*)] [x* (map binding-x b*)] [e* (map maybe-build-caller b*)])
           (if (null? rb*)
               `(letrec ([,x* ,e*] ...) ,body)
               (let ([b (car rb*)] [rb* (cdr rb*)])
                 (if (prelex-referenced (binding-x b))
                     (loop rb* (cons (binding-x b) x*) (cons (maybe-build-caller b) e*))
                     (loop rb* x* e*)))))]))

    (lambda (x)
      (let ([level (commonization-level)])
        (if (fx= level 0)
            x
            (let ([worthwhile-size (expt 2 (fx- 10 level))])
              (cpcommonize2 (cpcommonize1 (cpcommonize0 x) worthwhile-size))))))))
)
