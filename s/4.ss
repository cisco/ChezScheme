"4.ss"
;;; 4.ss
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

(define apply
  (let ()
    (define-syntax build-apply
      (lambda (x)
        (syntax-case x ()
          [(_ () cl ...)
           #'(case-lambda
               [(p r)
                (unless (procedure? p)
                  ($oops #f "attempt to apply non-procedure ~s" p))
                (let ([n ($list-length r 'apply)])
                  (case n
                    [(0) (p)]
                    [(1) (p (car r))]
                    [(2) (p (car r) (cadr r))]
                    [(3) (let ([y1 (cdr r)]) (p (car r) (car y1) (cadr y1)))]
                    [else ($apply p n r)]))]
               cl ...
               [(p x . r)
                (unless (procedure? p)
                   ($oops #f "attempt to apply non-procedure ~s" p))
                (let ([r (cons x ($apply list* ($list-length r 'apply) r))])
                   ($apply p ($list-length r 'apply) r))])]
          [(_ (s1 s2 ...) cl ...)
           (with-syntax ((m (length #'(s1 s2 ...))))
             #'(build-apply
                 (s2 ...)
                 [(p s1 s2 ... r)
                  (unless (procedure? p)
                    ($oops #f "attempt to apply non-procedure ~s" p))
                  (let ([n ($list-length r 'apply)])
                    (case n
                      [(0) (p s1 s2 ...)]
                      [(1) (p s1 s2 ... (car r))]
                      [(2) (p s1 s2 ... (car r) (cadr r))]
                      [(3) (let ([y1 (cdr r)])
                              (p s1 s2 ... (car r) (car y1) (cadr y1)))]
                      [else ($apply p (fx+ n m) (list* s1 s2 ... r))]))]
                 cl ...))])))
    (build-apply (x1 x2 x3 x4))))

(define ormap)
(define andmap)
(define map)
(define for-each)
(define fold-left)
(define fold-right)
(let ()
  (define length-error
     (lambda (who l1 l2)
        ($oops who "lists ~s and ~s differ in length" l1 l2)))

  (define nonprocedure-error
     (lambda (who what)
        ($oops who "~s is not a procedure" what)))

  (define length-check
     (lambda (who first rest)
        (let ([n ($list-length first who)])
           (let loop ([rest rest])
              (cond
                 [(null? rest) n]
                 [(fx= ($list-length (car rest) who) n) (loop (cdr rest))]
                 [else (length-error who first (car rest))])))))

  (define mutation-error
    (lambda (who)
      ($oops who "input list was altered during operation")))

 ; getcxrs returns the cdrs of ls and their cars
  (define getcxrs
    (lambda (ls who)
      (if (null? ls)
          (values '() '())
          (let-values ([(cdrs cars) (getcxrs (cdr ls) who)])
            (let ([d (cdar ls)])
              (unless (pair? d) (mutation-error who))
              (values (cons d cdrs) (cons (car d) cars)))))))

  (let ()
    (define-syntax do-ormap
      (syntax-rules ()
        [(_ who)
         (case-lambda
           [(f ls)
            (unless (procedure? f) (nonprocedure-error who f))
            (and (not (null? ls))
                 (let ormap ([n ($list-length ls who)] [ls ls])
                   (if (fx= n 1)
                       (f (car ls))
                       (or (f (car ls))
                           (let ([ls (cdr ls)])
                             (unless (pair? ls) (mutation-error who))
                             (ormap (fx- n 1) ls))))))]
           [(f ls . more)
            (unless (procedure? f) (nonprocedure-error who f))
            (let ([n (length-check who ls more)])
              (and (not (fx= n 0))
                   (let ormap ([n n] [ls ls] [more more] [cars (map car more)])
                     (if (fx= n 1)
                         (apply f (car ls) cars)
                         (or (apply f (car ls) cars)
                             (let ([ls (cdr ls)])
                               (unless (pair? ls) (mutation-error who))
                               (let-values ([(cdrs cars) (getcxrs more who)])
                                 (ormap (fx- n 1) ls cdrs cars))))))))])]))
    (set-who! ormap (do-ormap who))
    (set-who! exists (do-ormap who)))

  (let ()
    (define-syntax do-andmap
      (syntax-rules ()
        [(_ who)
         (case-lambda
           [(f ls)
            (unless (procedure? f) (nonprocedure-error who f))
            (or (null? ls)
                (let andmap ([n ($list-length ls who)] [ls ls])
                  (if (fx= n 1)
                      (f (car ls))
                      (and (f (car ls))
                           (let ([ls (cdr ls)])
                             (unless (pair? ls) (mutation-error who))
                             (andmap (fx- n 1) ls))))))]
           [(f ls . more)
            (unless (procedure? f) (nonprocedure-error who f))
            (let ([n (length-check who ls more)])
              (or (fx= n 0)
                  (let andmap ([n n] [ls ls] [more more] [cars (map car more)])
                    (if (fx= n 1)
                        (apply f (car ls) cars)
                        (and (apply f (car ls) cars)
                             (let ([ls (cdr ls)])
                               (unless (pair? ls) (mutation-error who))
                               (let-values ([(cdrs cars) (getcxrs more who)])
                                 (andmap (fx- n 1) ls cdrs cars))))))))])]))
    (set-who! andmap (do-andmap who))
    (set-who! for-all (do-andmap who)))

  (set! map
    (case-lambda
      [(f ls)
       (unless (procedure? f) (nonprocedure-error 'map f))
       ($list-length ls 'map)
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls)]
      [(f ls1 ls2)
       (unless (procedure? f) (nonprocedure-error 'map f))
       (unless (fx= ($list-length ls1 'map) ($list-length ls2 'map))
         (length-error 'map ls1 ls2))
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls1 ls2)]
      [(f ls . more)
       (unless (procedure? f) (nonprocedure-error 'map f))
       (length-check 'map ls more)
       (let map ([f f] [ls ls] [more more])
         (if (null? ls)
             '()
            ; cdr first to avoid getting sick if f mutates input
             (let ([tail (map f (cdr ls) (#3%map cdr more))])
               (cons (apply f (car ls) (#3%map car more)) tail))))]))

  (set! $map
   ; same as map but errors are reported as coming from who
    (case-lambda
      [(who f ls)
       (unless (procedure? f) (nonprocedure-error who f))
       ($list-length ls who)
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls)]
      [(who f ls1 ls2)
       (unless (procedure? f) (nonprocedure-error who f))
       (unless (fx= ($list-length ls1 who) ($list-length ls2 who))
         (length-error who ls1 ls2))
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls1 ls2)]
      [(who f ls . more)
       (unless (procedure? f) (nonprocedure-error who f))
       (length-check who ls more)
       (let map ([f f] [ls ls] [more more])
         (if (null? ls)
             '()
            ; cdr first to avoid getting sick if f mutates input
             (let ([tail (map f (cdr ls) (#3%map cdr more))])
               (cons (apply f (car ls) (#3%map car more)) tail))))]))

  (set! for-each
    (case-lambda
      [(f ls)
       (unless (procedure? f) (nonprocedure-error 'for-each f))
       (unless (null? ls)
         (let for-each ([n ($list-length ls 'for-each)] [ls ls])
           (if (fx= n 1)
               (f (car ls))
               (begin
                 (f (car ls))
                 (let ([ls (cdr ls)])
                   (unless (pair? ls) (mutation-error 'for-each))
                   (for-each (fx- n 1) ls))))))]
      [(f ls . more)
       (unless (procedure? f) (nonprocedure-error 'for-each f))
       (let ([n (length-check 'for-each ls more)])
         (unless (fx= n 0)
           (let for-each ([n n] [ls ls] [more more] [cars (map car more)])
             (if (fx= n 1)
                 (apply f (car ls) cars)
                 (begin
                   (apply f (car ls) cars)
                   (let ([ls (cdr ls)])
                     (unless (pair? ls) (mutation-error 'for-each))
                     (let-values ([(cdrs cars) (getcxrs more 'for-each)])
                       (for-each (fx- n 1) ls cdrs cars))))))))]))

  (set! fold-left 
    (case-lambda
      [(combine nil ls)
       (unless (procedure? combine) (nonprocedure-error 'fold-left combine))
       (cond
         [(null? ls) nil]
         [else
          ($list-length ls 'fold-left)
          (let fold-left ([ls ls] [acc nil])
            (let ([cdrls (cdr ls)])
              (if (pair? cdrls)
                  (fold-left cdrls (combine acc (car ls)))
                  (if (null? cdrls)
                      (combine acc (car ls))
                      (mutation-error 'fold-left)))))])]
      [(combine nil ls . more)
       (unless (procedure? combine) (nonprocedure-error 'fold-left combine))
       (length-check 'fold-left ls more)
       (if (null? ls)
           nil
           (let fold-left ([ls ls] [more more] [cars (map car more)] [acc nil])
             (let ([cdrls (cdr ls)])
               (if (null? cdrls)
                   (apply combine acc (car ls) cars)
                   (let ([acc (apply combine acc (car ls) cars)])
                     (unless (pair? cdrls) (mutation-error 'fold-left))
                     (let-values ([(cdrs cars) (getcxrs more 'fold-left)])
                       (fold-left cdrls cdrs cars acc)))))))]))

  (set! fold-right 
    (case-lambda
      [(combine nil ls)
       (unless (procedure? combine) (nonprocedure-error 'fold-right combine))
       ($list-length ls 'fold-right)
      ; #3%fold-right naturally does cdrs first to avoid mutation sickness
       (#3%fold-right combine nil ls)]
      [(combine nil ls1 ls2)
       (unless (procedure? combine) (nonprocedure-error 'fold-right combine))
       (unless (fx= ($list-length ls1 'map) ($list-length ls2 'map))
         (length-error 'fold-right ls1 ls2))
      ; #3%fold-right naturally does cdrs first to avoid mutation sickness
       (#3%fold-right combine nil ls1 ls2)]
      [(combine nil ls . more)
       (unless (procedure? combine) (nonprocedure-error 'fold-right combine))
       (length-check 'fold-right ls more)
       (let fold-right ([combine combine] [nil nil] [ls ls] [more more])
         (if (null? ls)
             nil
             (apply combine (car ls)
               (#3%fold-right cons 
                 (list (fold-right combine nil (cdr ls) (map cdr more)))
                 (map car more)))))]))
)

(let ()
  (define disable/enable (make-winder #f disable-interrupts enable-interrupts))

  (define (dwind in body out)
    (let ((old-winders ($current-winders)))
      (in)
      ($current-winders (cons (make-winder #f in out) old-winders))
      (call-with-values
        body
        (case-lambda
          [(x)
           ($current-winders old-winders)
           (out)
           x]
          [args
           ($current-winders old-winders)
           (out)
           (apply values args)]))))

  (define (cwind in body out)
    (let* ((old-winders ($current-winders))
           [d/e+old-winders (cons disable/enable old-winders)])
      (disable-interrupts)
      ($current-winders d/e+old-winders)
      (in)
      ($current-winders (cons (make-winder #t in out) old-winders))
      (enable-interrupts)
      (call-with-values
        body
        (case-lambda
          [(x)
           (disable-interrupts)
           ($current-winders d/e+old-winders)
           (out)
           ($current-winders old-winders)
           (enable-interrupts)
           x]
          [args
           (disable-interrupts)
           ($current-winders d/e+old-winders)
           (out)
           ($current-winders old-winders)
           (enable-interrupts)
           (apply values args)]))))

  (define (check-args in body out)
    (unless (procedure? in)
      ($oops 'dynamic-wind "~s is not a procedure" in))
    (unless (procedure? body)
      ($oops 'dynamic-wind "~s is not a procedure" body))
    (unless (procedure? out)
      ($oops 'dynamic-wind "~s is not a procedure" out)))

  (set! dynamic-wind
    (case-lambda
      [(in body out)
       (check-args in body out)
       (dwind in body out)]
      [(critical? in body out)
       (check-args in body out)
       (if critical?
           (cwind in body out)
           (dwind in body out))]))

  (set-who! #(r6rs: dynamic-wind)
    (lambda (in body out)
      (#2%dynamic-wind in body out)))

  (set! $do-wind
    (lambda (old new)
      (define common-tail
        (lambda (x y)
          (let ([lx (length x)] [ly (length y)])
            (do ([x (if (fx> lx ly) (list-tail x (fx- lx ly)) x) (cdr x)]
                 [y (if (fx> ly lx) (list-tail y (fx- ly lx)) y) (cdr y)])
                ((eq? x y) x)))))
      (let ([tail (common-tail old new)])
        (let f ((old old))
          (unless (eq? old tail)
            (let ([w (car old)] [old (cdr old)])
              (if (winder-critical? w)
                  (begin
                    (disable-interrupts)
                    ($current-winders (cons disable/enable old))
                    ((winder-out w))
                    ($current-winders old)
                    (enable-interrupts))
                  (begin
                    ($current-winders old)
                    ((winder-out w))))
              (f old))))
        (let f ([new new])
          (unless (eq? new tail)
            (let ([w (car new)])
              (f (cdr new))
              (if (winder-critical? w)
                  (begin
                    (disable-interrupts)
                    ($current-winders (cons disable/enable (cdr new)))
                    ((winder-in w))
                    ($current-winders new)
                    (enable-interrupts))
                  (begin
                    ((winder-in w))
                    ($current-winders new)))))))))
)


;;; make-promise and force

(define $make-promise
  (lambda (thunk)
    (unless (procedure? thunk)
      ($oops '$make-promise "~s is not a procedure" thunk))
    (let ([value (void)] [set? #f])
      (lambda ()
        (case set?
          [(single) value]
          [(multiple) (apply values value)]
          [else
           (call-with-values
             thunk
             (case-lambda
               [(x)
                (case set?
                  [(single) value]
                  [(multiple) (apply values value)]
                  [(#f) (set! value x)
                        (set! set? 'single)
                        x])]
               [x
                (case set?
                  [(single) value]
                  [(multiple) (apply values value)]
                  [(#f) (set! value x)
                        (set! set? 'multiple)
                        (apply values x)])]))])))))

(define force
   (lambda (promise)
      (unless (procedure? promise)
         ($oops 'force "~s is not a procedure" promise))
      (promise)))
