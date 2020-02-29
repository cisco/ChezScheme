;;; cpletrec.ss
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

#|
Notes:
 - cpletrec does not consider a record-ref form or call to a restricted
   primitive, like car, to be pure even at optimize-level 3 because it's
   possible it will be moved ahead of an explicit test within a sequence
   of letrec* bindings.
|#

#|
Handling letrec and letrec*
 - call cpletrec on each rhs recursively to determine the new rhs,
   whether it's pure, and which of the lhs variables are free in it
 - call cpletrec on the body
 - build a graph.  For letrec, create a link from b1 to b2 iff b2 is free
   in b1.  for letrec*, also create a link from b1 to b2 if neither is
   pure and b1 originally appeared before b2.
 - determine the strongly connected components of the graph, partially
   sorted so that SCC1 comes before SCC2 if there exists a binding b2
   in SCC2 that has a link to a binding b1 in SCC1.
 - process each SCC as a separate set of letrec/letrec* bindings:
   - for letrec*, sort the bindings of the SCC by their original relative
     positions.  for letrec, any order will do.
   - if SCC contains a single binding b where LHS(b) is not assigned
     and RHS(b) is a lambda expression, bind using pure letrec,
   - otherwise, if SCC contains a single binding b where LHS(b) is
     not free in RHS(b), bind using let
   - otherwise, partition into lambda bindings lb ... and complex
     bindings cb ... where a binding b is lambda iff LHS(b) is not
     assigned and RHS(b) is a lambda expression.  Generate:
       (let ([LHS(cb) (void)] ...)
         (letrec ([LHS(lb) RHS(cb)] ...)
           (set! LHS(cb) RHS(cb)) ...
           body))
 - assimilate nested pure letrec forms
|#

(define $cpletrec
(let ()
  (import (nanopass))
  (include "base-lang.ss")

  (define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))

  (define-pass lift-profile-forms : Lsrc (ir) -> Lsrc ()
    (definitions
      (with-output-language (Lsrc Expr)
        (define lift-profile-forms
          ; pull out profile forms from simple subforms so the profile
          ; forms won't interfere with downstream optimizations
          (lambda (e* k)
            (define extract-profile
              (lambda (e profile*)
                (define profile?
                  (lambda (e)
                    (nanopass-case (Lsrc Expr) e
                      [(profile ,src) #t]
                      [(seq ,e1 ,e2) (and (profile? e1) (profile? e2))]
                      [else #f])))
                (define simple?
                  (lambda (e)
                    (nanopass-case (Lsrc Expr) e
                      [(quote ,d) #t]
                      [(ref ,maybe-src ,x) #t]
                      [,pr #t]
                      [(call ,preinfo ,pr ,e*) (eq? (primref-name pr) '$top-level-value)]
                      [(case-lambda ,preinfo ,cl* ...) #t]
                      [else #f])))
                (nanopass-case (Lsrc Expr) e
                  [(seq ,e1 ,e2)
                   (guard (and (profile? e1) (simple? e2)))
                   (values e2 (cons e1 profile*))]
                  [else (values e profile*)])))
            (let f ([e* e*] [re* '()] [profile* '()])
              (if (null? e*)
                  (fold-left (lambda (e profile) `(seq ,profile ,e))
                    (k (reverse re*))
                    profile*)
                  (let-values ([(e profile*) (extract-profile (car e*) profile*)])
                    (f (cdr e*) (cons e re*) profile*))))))))
    (Expr : Expr (ir) -> Expr ()
      [(call ,preinfo ,[e] ,[e*] ...)
       (lift-profile-forms (cons e e*)
         (lambda (e*)
           `(call ,preinfo ,(car e*) ,(cdr e*) ...)))]
      [(letrec ([,x* ,[e*]] ...) ,[body])
       (lift-profile-forms e*
         (lambda (e*)
           `(letrec ([,x* ,e*] ...) ,body)))]
      [(letrec* ([,x* ,[e*]] ...) ,[body])
       (lift-profile-forms e*
         (lambda (e*)
           `(letrec* ([,x* ,e*] ...) ,body)))]))

  (define-pass cpletrec : Lsrc (ir) -> Lsrc ()
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
      (define (Expr* e*)
        (if (null? e*)
            (values '() #t)
            (let-values ([(e e-pure?) (Expr (car e*))]
                         [(e* e*-pure?) (Expr* (cdr e*))])
              (values (cons e e*) (and e-pure? e*-pure?)))))
      (with-output-language (Lsrc Expr)
        (define build-seq 
          (lambda (e* body)
            (fold-right (lambda (e body) `(seq ,e ,body)) body e*)))
        (define build-let
          (lambda (call-preinfo lambda-preinfo lhs* rhs* body)
            (if (null? lhs*)
                body
                (let ([interface (length lhs*)])
                  `(call ,call-preinfo
                     (case-lambda ,lambda-preinfo
                       (clause (,lhs* ...) ,interface ,body))
                     ,rhs* ...)))))
        (module (cpletrec-letrec)
          (define-record-type binding
            (fields (immutable lhs) (immutable pos) (mutable rhs) (mutable pure?) (mutable recursive?))
            (nongenerative)
            (protocol
              (lambda (new)
                (lambda (lhs pos)
                  (new lhs pos #f #f #f)))))
          (define-record-type node ; isolate stuff needed for compute-sccs!
            (parent binding)
            (fields (mutable link*) (mutable root) (mutable done))
            (nongenerative)
            (sealed #t)
            (protocol
              (lambda (make-new)
                (lambda (lhs pos)
                  ((make-new lhs pos) '() #f #f)))))
          (define (lambda? x)
            (nanopass-case (Lsrc Expr) x
              [(case-lambda ,preinfo ,cl* ...) #t]
              [else #f]))
          (define (cpletrec-bindings *? lhs* rhs*)
            (let ([all-b* (map make-node lhs* (enumerate lhs*))])
              (let loop ([b* all-b*] [rhs* rhs*] [last-nonpure #f])
                (unless (null? b*)
                  (let ([b (car b*)] [rhs (car rhs*)])
                    (for-each (lambda (lhs) (set-prelex-seen! lhs #f)) lhs*)
                    (let-values ([(rhs pure?) (Expr rhs)])
                      (binding-rhs-set! b rhs)
                      (binding-pure?-set! b pure?)
                      (binding-recursive?-set! b (prelex-seen (binding-lhs b)))
                      (let ([free* (filter (lambda (b) (prelex-seen (binding-lhs b))) all-b*)])
                        (if (or pure? (not *?))
                            (begin
                              (node-link*-set! b free*)
                              (loop (cdr b*) (cdr rhs*) last-nonpure))
                            (begin
                              (node-link*-set! b
                                (if (and last-nonpure (not (memq last-nonpure free*)))
                                    (cons last-nonpure free*)
                                    free*))
                              (loop (cdr b*) (cdr rhs*) b))))))))
              all-b*))
          (define (compute-sccs v*) ; Tarjan's algorithm
            (define scc* '())
            (define (compute-sccs v)
              (define index 0)
              (define stack '())
              (define (tarjan v)
                (let ([v-index index])
                  (node-root-set! v v-index)
                  (set! stack (cons v stack))
                  (set! index (fx+ index 1))
                  (for-each
                    (lambda (v^)
                      (unless (node-done v^)
                        (unless (node-root v^) (tarjan v^))
                        (node-root-set! v (fxmin (node-root v) (node-root v^)))))
                    (node-link* v))
                  (when (fx= (node-root v) v-index)
                    (set! scc*
                      (cons
                        (let f ([ls stack])
                          (let ([v^ (car ls)])
                            (node-done-set! v^ #t)
                            (cons v^ (if (eq? v^ v)
                                         (begin (set! stack (cdr ls)) '())
                                         (f (cdr ls))))))
                        scc*)))))
              (tarjan v))
            (for-each (lambda (v) (unless (node-done v) (compute-sccs v))) v*)
            (reverse scc*))
          (define (grisly-letrec lb* cb* body)
            (let ([rclhs* (fold-right (lambda (b lhs*)
                                        (let ([lhs (binding-lhs b)])
                                          (if (prelex-referenced/assigned lhs)
                                              (cons lhs lhs*)
                                              lhs*)))
                            '() cb*)])
              (build-let (make-preinfo) (make-preinfo-lambda) rclhs* (map (lambda (x) `(quote ,(void))) rclhs*)
                (build-letrec (map binding-lhs lb*) (map binding-rhs lb*)
                  (fold-right (lambda (b body)
                                (let ([lhs (binding-lhs b)] [rhs (binding-rhs b)])
                                  `(seq
                                     ,(if (prelex-referenced lhs)
                                          (begin
                                            (set-prelex-assigned! lhs #t)
                                            `(set! #f ,lhs ,rhs))
                                          rhs)
                                     ,body)))
                    body cb*)))))
          (define build-letrec
            (lambda (lhs* rhs* body)
              (if (null? lhs*)
                  ; dropping source here; could attach to body or add source record
                  body
                  (nanopass-case (Lsrc Expr) body
                    ; assimilate nested letrecs
                    [(letrec ([,x* ,e*] ...) ,body)
                     `(letrec ([,(append lhs* x*) ,(append rhs* e*)] ...) ,body)]
                    [else `(letrec ([,lhs* ,rhs*] ...) ,body)]))))
          (define (expand-letrec b* body)
            (if (null? (cdr b*))
                (let* ([b (car b*)] [lhs (binding-lhs b)] [rhs (binding-rhs b)])
                  (cond
                    [(and (not (prelex-referenced/assigned lhs)) (binding-pure? b)) body]
                    [(and (not (prelex-assigned lhs)) (lambda? rhs))
                     (build-letrec (list lhs) (list rhs) body)]
                    [(not (memq b (node-link* b)))
                     (build-let (make-preinfo) (make-preinfo-lambda) (list lhs) (list rhs) body)]
                    [else (grisly-letrec '() b* body)]))
                (let-values ([(lb* cb*) (partition
                                          (lambda (b)
                                            (and (not (prelex-assigned (binding-lhs b)))
                                                 (lambda? (binding-rhs b))))
                                          b*)])
                  (grisly-letrec lb* cb* body))))
          (define (cpletrec-letrec *? lhs* rhs* body)
            (let ([b* (cpletrec-bindings *? lhs* rhs*)])
              (let-values ([(body body-pure?) (Expr body)])
                (values
                  (let f ([scc* (compute-sccs b*)])
                    (if (null? scc*)
                        body
                        (expand-letrec
                          (if *?
                              (sort
                                (lambda (b1 b2) (fx< (binding-pos b1) (binding-pos b2)))
                                (car scc*))
                              (car scc*))
                          (f (cdr scc*)))))
                  (and body-pure? (andmap binding-pure? b*)))))))))
    (Expr : Expr (ir) -> Expr (#t)
      [(ref ,maybe-src ,x)
       (let ([x (prelex-operand x)])
         (safe-assert (prelex? x))
         (safe-assert (prelex-was-referenced x))
         (when (prelex-referenced x)
           (safe-assert (prelex-was-multiply-referenced x))
           (set-prelex-multiply-referenced! x #t))
         (set-prelex-seen/referenced! x #t)
         (values `(ref ,maybe-src ,x) (not (prelex-was-assigned x))))]
      [(quote ,d) (values ir #t)]
      [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
       (guard (fx= (length e*) interface))
       (with-initialized-ids x*
         (lambda (x*)
           (let-values ([(body body-pure?) (Expr body)])
             (let-values ([(pre* lhs* rhs* pure?)
                           (let f ([x* x*] [e* e*])
                             (if (null? x*)
                                 (values '() '() '() #t)
                                 (let ([x (car x*)])
                                   (let-values ([(e e-pure?) (Expr (car e*))]
                                                [(pre* lhs* rhs* pure?) (f (cdr x*) (cdr e*))])
                                     (if (prelex-referenced/assigned x)
                                         (values pre* (cons x lhs*) (cons e rhs*) (and e-pure? pure?))
                                         (values (if e-pure? pre* (cons e pre*))
                                           lhs* rhs* (and e-pure? pure?)))))))])
               (values
                 (build-seq pre* (build-let preinfo0 preinfo1 lhs* rhs* body))
                 (and body-pure? pure?))))))]
      [(call ,preinfo ,pr ,e* ...)
       (let ()
         (define (arity-okay? arity n)
           (or (not arity)
               (ormap (lambda (a)
                        (or (fx= n a)
                            (and (fx< a 0) (fx>= n (fx- -1 a)))))
                 arity)))
         (let-values ([(e* pure?) (Expr* e*)])
           (values
             `(call ,preinfo ,pr ,e* ...)
             (and pure?
                  (all-set? (prim-mask (or proc pure unrestricted discard)) (primref-flags pr))
                  (arity-okay? (primref-arity pr) (length e*))))))]
      [(call ,preinfo ,[e pure?] ,[e* pure?*] ...)
       (values `(call ,preinfo ,e ,e* ...) #f)]
      [(if ,[e0 e0-pure?] ,[e1 e1-pure?] ,[e2 e2-pure?])
       (values `(if ,e0 ,e1 ,e2) (and e0-pure? e1-pure? e2-pure?))]
      [(case-lambda ,preinfo ,[cl*] ...)
       (values `(case-lambda ,preinfo ,cl* ...) #t)]
      [(seq ,[e1 e1-pure?] ,[e2 e2-pure?])
       (values `(seq ,e1 ,e2) (and e1-pure? e2-pure?))]
      [(set! ,maybe-src ,x ,[e pure?])
       (let ([x (prelex-operand x)])
         (safe-assert (prelex? x))
         (safe-assert (prelex-was-assigned x))
         ; NB: cpletrec-letrec assumes assignments to unreferenced ids are dropped
         (if (prelex-was-referenced x)
             (begin
               (set-prelex-seen/assigned! x #t)
               (values `(set! ,maybe-src ,x ,e) #f))
             (if pure? (values `(quote ,(void)) #t) (values `(seq ,e (quote ,(void))) #f))))]
      [(letrec ([,x* ,e*] ...) ,body)
       (with-initialized-ids x*
         (lambda (x*)
           (cpletrec-letrec #f x* e* body)))]
      [(letrec* ([,x* ,e*] ...) ,body)
       (with-initialized-ids x*
         (lambda (x*)
           (cpletrec-letrec #t x* e* body)))]
      [(foreign (,conv* ...) ,name ,[e pure?] (,arg-type* ...) ,result-type)
       (values `(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type)
         (and (fx= (optimize-level) 3) pure?))]
      [(fcallable (,conv* ...) ,[e pure?] (,arg-type* ...) ,result-type)
       (values `(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type)
         (and (fx= (optimize-level) 3) pure?))]
      [(record-ref ,rtd ,type ,index ,[e pure?])
       (values `(record-ref ,rtd ,type ,index ,e) #f)]
      [(record-set! ,rtd ,type ,index ,[e1 pure1?] ,[e2 pure2?])
       (values `(record-set! ,rtd ,type ,index ,e1 ,e2) #f)]
      [(record ,rtd ,[rtd-expr rtd-pure?] ,e* ...)
       (let-values ([(e* pure?) (Expr* e*)])
         (values
           `(record ,rtd ,rtd-expr ,e* ...)
           (and (and rtd-pure? pure?)
                (andmap
                  (lambda (fld)
                    (and (not (fld-mutable? fld))
                         (eq? (filter-foreign-type (fld-type fld)) 'scheme-object)))
                  (rtd-flds rtd)))))]
      [(record-type ,rtd ,e) (Expr e)]
      [(record-cd ,rcd ,rtd-expr ,e) (Expr e)]
      [(immutable-list (,[e* pure?*] ...) ,[e pure?])
       (values `(immutable-list (,e* ...) ,e) pure?)]
      [,pr (values pr #t)]
      [(moi) (values ir #t)]
      [(pariah) (values ir #t)]
      [(cte-optimization-loc ,box ,[e pure?])
       (values `(cte-optimization-loc ,box ,e) pure?)]
      [(profile ,src) (values ir #f)]
      [else (sorry! who "unhandled record ~s" ir)])
    (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
      [(clause (,x* ...) ,interface ,body)
       (with-initialized-ids x*
         (lambda (x*)
           (let-values ([(body pure?) (Expr body)])
             `(clause (,x* ...) ,interface ,body))))])
    (let-values ([(ir pure?) (Expr ir)]) ir))

(lambda (x)
  (let ([x (if (eq? ($compile-profile) 'source) (lift-profile-forms x) x)])
    (cpletrec x)))
))
