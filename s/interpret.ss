;;; interpret.ss
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

;;; TODO
;;;  - recognize direct close calls in ip2 to avoid creation of closure
;;;    (but not closure pointer) and overhead of call
;;;  - handle let & letrec better
;;;    - use arg regs when available
;;;    - wire up letrec closures, then treat like let (good luck)
;;;  - optimize direct calls when no free vars
;;;    - since closure is just code in this case, can wire it in directly

(let ()
(import (nanopass))
(include "base-lang.ss")
(include "expand-lang.ss")

(define-record-type c-var
  (fields (immutable id) (immutable parent) (mutable index) (mutable loc))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda (id parent)
        (new id parent #f #f)))))

(define list-of-c-var?
  (lambda (x)
    (and (list? x) (andmap c-var? x))))

(define-language Linterp
  (extends Lsrc)
  (terminals
    (- ($prelex (x)))
    (+ (c-var (x))
       (list-of-c-var (free))))
  (Expr (e body rtd-expr)
    (- (case-lambda preinfo cl ...)
       (call preinfo e0 e1 ...)
       (moi)
       (pariah)
       (ref maybe-src x)
       (set! maybe-src x e)
       (profile src))
    (+ x
       (close free cl ...)
       (call e e* ...)
       (set! x e))))

(define ip1
(let ()
(define-record-type c-env
  (fields (immutable prev) (mutable vars))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda (prev)
        (new prev '())))))

(define-pass ip1 : Lsrc (ir) -> Linterp ()
  (definitions
    (define ip1-lambda
      (lambda (clauses env)
        (let ([env (make-c-env env)])
          (let ([bodies
                  (map (lambda (clause)
                         (nanopass-case (Lsrc CaseLambdaClause) clause
                           [(clause (,x* ...) ,interface ,body)
                             (with-vars (vars x* env)
                               (with-output-language (Linterp CaseLambdaClause)
                                 (let ([body (Expr body env)])
                                   `(clause (,vars ...) ,interface ,body))))]
                           [else (errorf 'ip1-lambda "found something unexpected ~s\n" clause)]))
                    clauses)])
            (with-output-language (Linterp Expr)
              `(close ,(ip1-free env) ,bodies ...))))))
    (define ip1-letrec
      (lambda (ids vals body env)
        (with-output-language (Lsrc Expr)
          (define build-let
            (lambda (ids vals body)
              (if (null? ids)
                  body
                  `(call ,(make-preinfo)
                     (case-lambda ,(make-preinfo-lambda)
                       (clause (,ids ...) ,(length ids) ,body))
                     ,vals ...))))
          (Expr (if (null? ids)
                    body
                    (build-let ids (map (lambda (x) `(quote ,(void))) ids)
                      (fold-left (lambda (body id val)
                                   (set-prelex-assigned! id #t)
                                   `(seq (set! #f ,id ,val) ,body))
                        body ids vals)))
            env)))))
  (Expr : Expr (ir [env #f]) -> Expr ()
    [(ref ,maybe-src ,x) (ip1-lookup-lexical x env)]
    [(case-lambda ,preinfo ,cl* ...) (ip1-lambda cl* env)]
    [(call ,preinfo ,[e] ,[e*] ...) `(call ,e ,e* ...)]
    [(set! ,maybe-src ,x ,[e]) `(set! ,(ip1-lookup-lexical x env) ,e)]
    [(letrec ([,x* ,e*] ...) ,body) (ip1-letrec x* e* body env)]
    [(seq ,[e1] ,[e2])
     (nanopass-case (Linterp Expr) e1
       [(quote ,d) e2]
       [else `(seq ,e1 ,e2)])]
    [(moi) `(quote #f)]
    [(pariah) `(quote ,(void))]
    [(profile ,src) `(quote ,(void))]))

;;; When we create a lex from a prelex, we replace the name field of
;;; the prelex id with an initial mapping from environment to the lex
;;; var corresponding to the prelex in the environment.  This mapping is
;;; augmented by lookup-lexical (for references through rebind-free
;;; environments) and trimmed by maybe-free.

(define-syntax with-var
  (syntax-rules ()
    ((_ (var idexp env) e1 e2 ...)
     (let ((id idexp))
       (let ((name (prelex-name id)))
         (let ((var (make-c-var id #f)))
           (prelex-name-set! id (list (cons env var)))
           (let ((tmp (begin e1 e2 ...)))
            ; restore name to leave prelex undamaged; this is necessary at
            ; present because syntax objects may contain the same prelexes
            ; that arrive here as bound variables
             (prelex-name-set! id name)
             tmp)))))))

(define-syntax with-vars
  (syntax-rules ()
    ((_ (vars idsexp env) e1 e2 ...)
     (let f ((ids (reverse idsexp)) (vars '()))
       (if (null? ids)
           (begin e1 e2 ...)
           (with-var (var (car ids) env)
             (f (cdr ids) (cons var vars))))))))

(define ip1-free
  (lambda (e)
    (map (lambda (id)
           (let ((ls (prelex-name id)))
             (prelex-name-set! id (cdr ls))
             (cdar ls)))
         (c-env-vars e))))

(define ip1-lookup-lexical
  (lambda (id e)
    (let ((ls (prelex-name id)))
      (if (eq? (caar ls) e)
          (cdar ls)
          (let ((y (ip1-lookup-lexical id (c-env-prev e))))
            (let ([z (make-c-var id y)])
              (c-env-vars-set! e (cons id (c-env-vars e)))
              (prelex-name-set! id (cons (cons e z) (prelex-name id)))
              z))))))

(lambda (x) (ip1 x))))

(define-syntactic-monad $rt a0 a1 fp cp)

(module (ip2)
(define unexpected-loc
  (lambda (loc)
    ($oops 'interpret-internal "unexpected loc ~s" loc)))

(define ip2
  (lambda (x)
    (define unexpected-record
      (lambda (x)
        ($oops 'interpret-internal "unexpected record ~s" x)))
    (define non-procedure
      (lambda (x)
        ($oops #f "attempt to apply non-procedure ~s" x)))
    (define unbound-or-non-procedure
      (lambda (sym x)
        (if ($unbound-object? x)
            ($oops #f "variable ~:s is not bound" sym)
            (non-procedure x))))
    (define-syntax docall-sym
      (lambda (x)
        (syntax-case x ()
          [(_ sym e1 ...)
           (with-syntax ([(t0 t1 ...) (generate-temporaries #'(e0 e1 ...))])
             #'($rt lambda ()
                 (let ([t0 (#3%$top-level-value sym)] [t1 ($rt e1)] ...)
                   (unless (procedure? t0) (unbound-or-non-procedure sym t0))
                   (t0 t1 ...))))])))
    (define-syntax docall
      (lambda (x)
        (syntax-case x ()
          [(_ e0 e1 ...)
           (with-syntax ([(t0 t1 ...) (generate-temporaries #'(e0 e1 ...))])
             #'($rt lambda ()
                 (let ([t0 e0] [t1 ($rt e1)] ...)
                   (unless (procedure? t0) (non-procedure t0))
                   (t0 t1 ...))))])))
    (define-syntax docallx
      (lambda (x)
        (syntax-case x ()
          [(_ e0 e1 ...)
           (with-syntax ([(t0 t1 ...) (generate-temporaries #'(e0 e1 ...))])
             #'($rt lambda ()
                 (let ([t0 ($rt e0)] [t1 ($rt e1)] ...)
                   (unless (procedure? t0) (non-procedure t0))
                   (t0 t1 ...))))])))
    (define ip2-fat-call
      (lambda (fun args)
        (let ((args (reverse args)))
          ($rt lambda ()
            (let ((fun ($rt fun)))
              (let loop ([args args] [vals '()])
                (if (null? args)
                    (begin
                      (unless (procedure? fun) (non-procedure fun))
                      (apply fun vals))
                    (loop (cdr args) (cons ($rt (car args)) vals)))))))))
    (nanopass-case (Linterp Expr) x
      [,x
       (let ((loc (c-var-loc x)) (i (c-var-index x)))
         (if (prelex-assigned (c-var-id x))
             (case loc
               [(a0) ($rt lambda () (car a0))]
               [(a1) ($rt lambda () (car a1))]
               [(fp) ($rt lambda () (car fp))]
               [(cp) ($rt lambda () (car cp))]
               [(frame) ($rt lambda () (car (list-ref fp i)))]
               [(frame-rest) ($rt lambda () (car (list-tail fp i)))]
               [(closure) ($rt lambda () (car (vector-ref cp i)))]
               [else (unexpected-loc loc)])
             (case loc
               [(a0) ($rt lambda () a0)]
               [(a1) ($rt lambda () a1)]
               [(fp) ($rt lambda () fp)]
               [(cp) ($rt lambda () cp)]
               [(frame) ($rt lambda () (list-ref fp i))]
               [(frame-rest) ($rt lambda () (list-tail fp i))]
               [(closure) ($rt lambda () (vector-ref cp i))]
               [else (unexpected-loc loc)])))]
      [,pr (let ((fun ($top-level-value (primref-name pr))))
             ($rt lambda () fun))]
      [(quote ,d) ($rt lambda () d)]
      [(close ,free ,cl* ...)
       (unless (null? free)
         (if (null? (cdr free))
             (c-var-loc-set! (car free) 'cp)
             (let loop ((free free) (i 0))
               (unless (null? free)
                 (c-var-loc-set! (car free) 'closure)
                 (c-var-index-set! (car free) i)
                 (loop (cdr free) (fx+ i 1))))))
       (or (and (not (null? cl*))
                (null? (cdr cl*))
                (nanopass-case (Linterp CaseLambdaClause) (car cl*)
                  [(clause (,x* ...) ,interface ,body)
                   (if (null? free)
                       (case interface
                         [(0)
                          (let ((body (ip2-body body x* '(a0 a1 fp cp) #f)))
                            ($rt lambda ()
                              (lambda ()
                                ($rt body ([a0 0] [a1 0] [fp 0] [cp 0])))))]
                         [(1)
                          (let ((body (ip2-body body x* '(a0 a1 fp cp) #f)))
                            ($rt lambda ()
                              (lambda (a0)
                                ($rt body ([a1 0] [fp 0] [cp 0])))))]
                         [(2)
                          (let ((body (ip2-body body x* '(a0 a1 fp cp) #f)))
                            ($rt lambda ()
                              (lambda (a0 a1)
                                ($rt body ([fp 0] [cp 0])))))]
                         [(3)
                          (let ((body (ip2-body body x* '(a0 a1 fp cp) #f)))
                            ($rt lambda ()
                              (lambda (a0 a1 fp)
                                ($rt body ([cp 0])))))]
                         [(4)
                          (let ((body (ip2-body body x* '(a0 a1 fp cp) #f)))
                            ($rt lambda ()
                              (lambda (a0 a1 fp cp)
                                ($rt body))))]
                         [else #f])
                       (case interface
                         [(0)
                          (ip2-closure free
                            (let ((body (ip2-body body x* '(a0 a1 fp) #f)))
                              ($rt lambda ()
                                (lambda ()
                                  ($rt body ([a0 0] [a1 0] [fp 0]))))))]
                         [(1)
                          (ip2-closure free
                            (let ((body (ip2-body body x* '(a0 a1 fp) #f)))
                              ($rt lambda ()
                                (lambda (a0)
                                  ($rt body ([a1 0] [fp 0]))))))]
                         [(2)
                          (ip2-closure free
                            (let ((body (ip2-body body x* '(a0 a1 fp) #f)))
                              ($rt lambda ()
                                (lambda (a0 a1)
                                  ($rt body ([fp 0]))))))]
                         [(3)
                          (ip2-closure free
                            (let ((body (ip2-body body x* '(a0 a1 fp) #f)))
                              ($rt lambda ()
                                (lambda (a0 a1 fp)
                                  ($rt body)))))]
                         [else #f]))]))
          ; we could use cp if no closure; we could use fp if max interface
          ; is small enough.  we don't bother with either presently.
           (let ((m (let min? ((cl* cl*) (m (length '(a0 a1))))
                      (if (null? cl*)
                          m
                          (nanopass-case (Linterp CaseLambdaClause) (car cl*)
                            [(clause (,x* ...) ,interface ,body)
                             (min? (cdr cl*)
                               (min (if (fx< interface 0)
                                        (fx- -1 interface)
                                        interface)
                                 m))])))))
             (define adjust-interface
               (lambda (x)
                 (if (fx< x 0)
                     (fx+ x m)
                     (fx- x m))))
             (let ((body (let f ((cl* cl*))
                           (if (null? cl*)
                               ($rt lambda (args nargs)
                                 ($oops #f "incorrect number of arguments to #<procedure>"))
                               (nanopass-case (Linterp CaseLambdaClause) (car cl*)
                                 [(clause (,x* ...) ,interface ,body)
                                  (ip2-prelude
                                    (ip2-body body x* '(a0 a1)
                                      (fx< interface 0))
                                    (list-tail x* m)
                                    (list-tail '(a0 a1) m)
                                    (adjust-interface interface)
                                    (f (cdr cl*)))])))))
               (case m
                 [(0)
                  (ip2-closure free
                    ($rt lambda ()
                      (lambda args
                        ($rt body ([a0 0] [a1 0] [fp 0]) args (length args)))))]
                 [(1)
                  (ip2-closure free
                    ($rt lambda ()
                      (lambda (a0 . args)
                        ($rt body ([a1 0] [fp 0]) args (length args)))))]
                 [(2)
                  (ip2-closure free
                    ($rt lambda ()
                      (lambda (a0 a1 . args)
                        ($rt body ([fp 0]) args (length args)))))]))))]
      [(set! ,x ,e)
       (let ((e (ip2 e)))
         (let ((loc (c-var-loc x)) (i (c-var-index x)))
           (case loc
             [(a0) ($rt lambda () (set-car! a0 ($rt e)))]
             [(a1) ($rt lambda () (set-car! a1 ($rt e)))]
             [(fp) ($rt lambda () (set-car! fp ($rt e)))]
             [(cp) ($rt lambda () (set-car! cp ($rt e)))]
             [(frame) ($rt lambda () (set-car! (list-ref fp i) ($rt e)))]
             [(frame-rest)
              ($rt lambda () (set-car! (list-tail fp i) ($rt e)))]
             [(closure) ($rt lambda () (set-car! (vector-ref cp i) ($rt e)))]
             [else (unexpected-loc loc)])))]
      [(if ,e0 ,e1 ,e2)
       (let ((e0 (ip2 e0)) (e1 (ip2 e1)) (e2 (ip2 e2)))
         ($rt lambda ()
           ($rt (if ($rt e0) e1 e2))))]
      [(call ,e ,e* ...)
       (let ((e* (map (lambda (x) (ip2 x)) e*)))
         (or (nanopass-case (Linterp Expr) e
               [,pr
                (case (length e*)
                  [(0)
                   (let ((e ($top-level-value (primref-name pr))))
                     ($rt lambda () (e)))]
                  [(1)
                   (apply
                     (lambda (x1)
                       (let ((e ($top-level-value (primref-name pr))))
                         ($rt lambda () (e ($rt x1)))))
                     e*)]
                  [(2)
                   (apply
                     (lambda (x1 x2)
                       (let ((e ($top-level-value (primref-name pr))))
                         ($rt lambda () (e ($rt x1) ($rt x2)))))
                     e*)]
                  [(3)
                   (apply
                     (lambda (x1 x2 x3)
                       (let ((e ($top-level-value (primref-name pr))))
                         ($rt lambda ()
                           (e ($rt x1) ($rt x2) ($rt x3)))))
                     e*)]
                  [else #f])]
               [(call ,e1 ,e1* ...)
                (nanopass-case (Linterp Expr) e1
                  [,pr (and (eq? (primref-name pr) '$top-level-value)
                            (fx= (length e*) 1)
                            (nanopass-case (Linterp Expr) (car e1*)
                              [(quote ,d)
                               (and (symbol? d)
                                    (case (length e*)
                                      [(0) (docall-sym d)]
                                      [(1)
                                       (apply
                                         (lambda (x1)
                                           (docall-sym d x1))
                                         e*)]
                                      [(2)
                                       (apply
                                         (lambda (x1 x2)
                                           (docall-sym d x1 x2))
                                         e*)]
                                      [(3)
                                       (apply
                                         (lambda (x1 x2 x3)
                                           (docall-sym d x1 x2 x3))
                                         e*)]
                                      [else #f]))]
                              [else #f]))]
                  [else #f])]
               [else #f])
             (let ((e (ip2 e)))
               (case (length e*)
                 [(0) (docallx e)]
                 [(1)
                  (apply
                    (lambda (x1) (docallx e x1))
                    e*)]
                 [(2)
                  (apply
                    (lambda (x1 x2) (docallx e x1 x2))
                    e*)]
                 [(3)
                  (apply
                    (lambda (x1 x2 x3) (docallx e x1 x2 x3))
                    e*)]
                 [else (ip2-fat-call e e*)]))))]
      [(seq ,e1 ,e2)
       (let ((e1 (ip2 e1)) (e2 (ip2 e2)))
         ($rt lambda () ($rt e1) ($rt e2)))]
      [(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type)
       (unless $compiler-is-loaded?
         ($oops 'interpret "cannot compile foreign-procedure: compiler is not loaded"))
       (let ([p ($compile-backend
                  (let ((t (make-prelex* 'tmp)))
                    (set-prelex-referenced! t #t)
                    (with-output-language (Lsrc Expr)
                      `(case-lambda ,(make-preinfo-lambda)
                         (clause (,t) 1
                           (foreign (,conv* ...) ,name (ref #f ,t)
                             (,arg-type* ...) ,result-type))))))])
         (let ([e (ip2 e)])
           ($rt lambda () ((p) ($rt e)))))]
      [(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type)
       (unless $compiler-is-loaded?
         ($oops 'interpret "cannot compile foreign-callable: compiler is not loaded"))
       (let ([p ($compile-backend
                  (let ((t (make-prelex* 'tmp)))
                    (set-prelex-referenced! t #t)
                    (with-output-language (Lsrc Expr)
                      `(case-lambda ,(make-preinfo-lambda)
                         (clause (,t) 1
                           (fcallable (,conv* ...) (ref #f ,t) (,arg-type* ...) ,result-type))))))])
         (let ([e (ip2 e)])
           ($rt lambda () ((p) ($rt e)))))]
      [else (unexpected-record x)])))

(define ip2-prelude
  (lambda (body vars regs i next)
    (define set-args
      (lambda (vars regs body rest?)
        (if (null? regs)
            ($rt lambda (args) ($rt body ([fp args])))
            (let ((reg (car regs)))
              (if (null? (cdr vars))
                  (if rest?
                      (case reg
                        [(a0) ($rt lambda (args) ($rt body ([a0 args])))]
                        [(a1) ($rt lambda (args) ($rt body ([a1 args])))]
                        [(fp) ($rt lambda (args) ($rt body ([fp args])))]
                        [(cp) ($rt lambda (args) ($rt body ([cp args])))]
                        [else (unexpected-loc reg)])
                      (case reg
                        [(a0) ($rt lambda (args) ($rt body ([a0 (car args)])))]
                        [(a1) ($rt lambda (args) ($rt body ([a1 (car args)])))]
                        [(fp) ($rt lambda (args) ($rt body ([fp (car args)])))]
                        [(cp) ($rt lambda (args) ($rt body ([cp (car args)])))]
                        [else (unexpected-loc reg)]))
                  (let ((body (set-args (cdr vars) (cdr regs) body rest?)))
                    (case reg
                      [(a0) ($rt lambda (args)
                              ($rt body ([a0 (car args)]) (cdr args)))]
                      [(a1) ($rt lambda (args)
                              ($rt body ([a1 (car args)]) (cdr args)))]
                      [(fp) ($rt lambda (args)
                              ($rt body ([fp (car args)]) (cdr args)))]
                      [(cp) ($rt lambda (args)
                              ($rt body ([cp (car args)]) (cdr args)))]
                      [else (unexpected-loc reg)])))))))
    (if (fx>= i 0)
        (if (fx= i 0)
            ($rt lambda (args nargs)
              (if (fx= nargs 0)
                  ($rt body)
                  ($rt next () args nargs)))
            (let ((body (set-args vars regs body #f)))
              ($rt lambda (args nargs)
                (if (fx= nargs i)
                    ($rt body () args)
                    ($rt next () args nargs)))))
        (let ((body (set-args vars regs body #t)))
          (if (fx= i -1)
              ($rt lambda (args nargs) ($rt body () args))
              (let ((i (fx- -1 i)))
                ($rt lambda (args nargs)
                  (if (fx>= nargs i)
                      ($rt body () args)
                      ($rt next () args nargs)))))))))

(define ip2-closure
  (lambda (free code)
    (let ([free (map (lambda (var)
                       (let* ((var (c-var-parent var))
                              (loc (c-var-loc var))
                              (i (c-var-index var)))
                         (case loc
                           [(a0) ($rt lambda () a0)]
                           [(a1) ($rt lambda () a1)]
                           [(fp) ($rt lambda () fp)]
                           [(cp) ($rt lambda () cp)]
                           [(frame) ($rt lambda () (list-ref fp i))]
                           [(frame-rest) ($rt lambda () (list-tail fp i))]
                           [(closure) ($rt lambda () (vector-ref cp i))]
                           [else (unexpected-loc loc)])))
                     free)])
      (let ((nfree (length free)))
        (case nfree
          [(0) ($rt lambda () ($rt code ([cp 0])))]
          [(1)
           (apply
             (lambda (x1)
               ($rt lambda () ($rt code ([cp ($rt x1)]))))
             free)]
          [(2)
           (apply
             (lambda (x1 x2)
               ($rt lambda ()
                 ($rt code ([cp (vector ($rt x1) ($rt x2))]))))
             free)]
          [(3)
           (apply
             (lambda (x1 x2 x3)
               ($rt lambda ()
                 ($rt code ([cp (vector ($rt x1) ($rt x2) ($rt x3))]))))
             free)]
          [(4)
           (apply
             (lambda (x1 x2 x3 x4)
               ($rt lambda ()
                 ($rt code ([cp (vector ($rt x1) ($rt x2) ($rt x3) ($rt x4))]))))
             free)]
          [else
           ($rt lambda ()
             (let ((v (make-vector nfree ($rt (car free)))))
               (do ((i 1 (fx+ i 1)) (free (cdr free) (cdr free)))
                   ((null? free))
                 (vector-set! v i ($rt (car free))))
               ($rt code ([cp v]))))])))))

(define ip2-body
  (lambda (body invars regs rest?)
    ; set locations
     (let loop ((vars invars) (regs regs) (i 0))
       (cond
         [(null? vars)
         ; process the body and wrap in consers for assigned variables
          (do ((vars invars (cdr vars))
               (body (ip2 body)
                     (let ((var (car vars)))
                       (if (prelex-assigned (c-var-id var))
                           (case (c-var-loc var)
                             [(a0)
                              ($rt lambda ()
                                ($rt body ([a0 (cons a0 (void))])))]
                             [(a1)
                              ($rt lambda ()
                                ($rt body ([a1 (cons a1 (void))])))]
                             [(fp)
                              ($rt lambda ()
                                ($rt body ([fp (cons fp (void))])))]
                             [(cp)
                              ($rt lambda ()
                                ($rt body ([cp (cons cp (void))])))]
                             [(frame)
                              (let ((i (c-var-index var)))
                                ($rt lambda ()
                                  (let ((ls (list-tail fp i)))
                                    (set-car! ls (cons (car ls) (void))))
                                  ($rt body)))]
                             [(frame-rest)
                              (let ((i (fx- (c-var-index var) 1)))
                                ($rt lambda ()
                                  (let ((ls (list-tail fp i)))
                                    (set-cdr! ls (cons (cdr ls) (void))))
                                  ($rt body)))])
                           body))))
              ((null? vars) body))]
         [(not (null? regs))
          (c-var-loc-set! (car vars) (car regs))
          (loop (cdr vars) (cdr regs) i)]
         [(and rest? (null? (cdr vars)))
          (cond
            [(fx= i 0)
            ; using fp here instead of the equivalent frame-rest[0]
            ; eliminates need for special-casing frame-rest[0] elsewhere.
             (c-var-loc-set! (car vars) 'fp)
             (loop (cdr vars) regs i)]
            [else
             (c-var-loc-set! (car vars) 'frame-rest)
             (c-var-index-set! (car vars) i)
             (loop (cdr vars) regs (fx+ i 1))])]
         [else
          (c-var-loc-set! (car vars) 'frame)
          (c-var-index-set! (car vars) i)
          (loop (cdr vars) regs (fx+ i 1))])))))

(define-pass interpret-Lexpand : Lexpand (ir situation for-import? importer ofn eoo) -> * (val)
  (definitions
    (define (ibeval x1)
      ($rt (parameterize ([$target-machine (machine-type)] [$sfd #f])
             (let* ([x2 ($pass-time 'cpvalid (lambda () ($cpvalid x1)))]
                    [x2a (let ([cpletrec-ran? #f])
                           (let ([x ((run-cp0)
                                     (lambda (x)
                                       (set! cpletrec-ran? #t)
                                       (let ([x ($pass-time 'cp0 (lambda () ($cp0 x #f)))])
                                         ($pass-time 'cpletrec
                                           (lambda () ($cpletrec x)))))
                                     x2)])
                             (if cpletrec-ran? x ($pass-time 'cpletrec (lambda () ($cpletrec x))))))]
                    [x2b ($pass-time 'cpcheck (lambda () ($cpcheck x2a)))]
                    [x2b ($pass-time 'cpcommonize (lambda () ($cpcommonize x2b)))])
               (when eoo (pretty-print ($uncprep x2b) eoo))
               (let ([x ($pass-time 'ip1 (lambda () (ip1 x2b)))])
                 ($pass-time 'ip2 (lambda () (ip2 x))))))
        ([a0 0] [a1 0] [fp 0] [cp 0]))))
  (Inner : Inner (ir) -> * (val)
    [,lsrc (ibeval lsrc)]
    [(program ,uid ,body)
     (ibeval ($build-invoke-program uid body))]
    [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
     (ibeval ($build-install-library/ct-code uid export-id* import-code visit-code))]
    [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
     (ibeval ($build-install-library/rt-code uid dl* db* dv* de* body))]
    [(library/rt-info ,linfo/rt) ($install-library/rt-desc linfo/rt for-import? importer ofn)]
    [(library/ct-info ,linfo/ct) ($install-library/ct-desc linfo/ct for-import? importer ofn)]
    [(program-info ,pinfo) ($install-program-desc pinfo)]
    [else (sorry! who "unexpected language form ~s" ir)])
  (Outer : Outer (ir) -> * (val)
    ; can't use cata since (Outer outer1) might return 0 or more than one value
    [(group ,outer1 ,outer2) (Outer outer1) (Outer outer2)]
    [(visit-only ,inner) (unless (eq? situation 'revisit) (Inner inner))]
    [(revisit-only ,inner) (unless (eq? situation 'visit) (Inner inner))]
    [(recompile-info ,rcinfo) (void)]
    [,inner (Inner inner)]
    [else (sorry! who "unexpected language form ~s" ir)])
  (Outer ir))

(set! interpret
  (rec interpret
    (case-lambda
      [(x)
       (interpret x
         (if (eq? (subset-mode) 'system)
             ($system-environment)
             (interaction-environment)))]
      [(x0 env-spec)
       (unless (environment? env-spec) ($oops 'interpret "~s is not an environment" env-spec))
       (let ([x1 ($pass-time 'expand
                   (lambda ()
                     (parameterize ([$target-machine (machine-type)] [$sfd #f])
                       (expand x0 env-spec #t))))])
         ($uncprep x1 #t) ; populate preinfo sexpr fields
         (when (and (expand-output) (not ($noexpand? x0)))
           (pretty-print ($uncprep x1) (expand-output)))
         (interpret-Lexpand x1 'load #f #f #f (and (not ($noexpand? x0)) (expand/optimize-output))))])))

(set! $interpret-backend
  (lambda (x situation for-import? importer ofn)
    (interpret-Lexpand x situation for-import? importer ofn (expand/optimize-output))))
(current-eval interpret)
)

