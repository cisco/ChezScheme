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

(begin
(define-who apply
  (let ()
    (define-syntax build-apply
      (lambda (x)
        (syntax-case x ()
          [(_ () cl ...)
           #'(case-lambda
               [(p r)
                (unless (procedure? p)
                  ($oops #f "attempt to apply non-procedure ~s" p))
                (let ([n ($list-length r who)])
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
                (let ([r (cons x ($apply list* ($list-length r who) r))])
                   ($apply p ($list-length r who) r))])]
          [(_ (s1 s2 ...) cl ...)
           (with-syntax ((m (length #'(s1 s2 ...))))
             #'(build-apply
                 (s2 ...)
                 [(p s1 s2 ... r)
                  (unless (procedure? p)
                    ($oops #f "attempt to apply non-procedure ~s" p))
                  (let ([n ($list-length r who)])
                    (case n
                      [(0) (p s1 s2 ...)]
                      [(1) (p s1 s2 ... (car r))]
                      [(2) (p s1 s2 ... (car r) (cadr r))]
                      [(3) (let ([y1 (cdr r)])
                              (p s1 s2 ... (car r) (car y1) (cadr y1)))]
                      [else ($apply p (fx+ n m) (list* s1 s2 ... r))]))]
                 cl ...))])))
    (build-apply (x1 x2 x3 x4))))

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

  (set-who! map
    (case-lambda
      [(f ls)
       (unless (procedure? f) (nonprocedure-error who f))
       ($list-length ls who)
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls)]
      [(f ls1 ls2)
       (unless (procedure? f) (nonprocedure-error who f))
       (unless (fx= ($list-length ls1 who) ($list-length ls2 who))
         (length-error who ls1 ls2))
      ; library map cdrs first to avoid getting sick if f mutates input
       (#3%map f ls1 ls2)]
      [(f ls . more)
       (unless (procedure? f) (nonprocedure-error who f))
       (length-check who ls more)
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

  (set-who! for-each
    (case-lambda
      [(f ls)
       (unless (procedure? f) (nonprocedure-error who f))
       (unless (null? ls)
         (let for-each ([n ($list-length ls who)] [ls ls])
           (if (fx= n 1)
               (f (car ls))
               (begin
                 (f (car ls))
                 (let ([ls (cdr ls)])
                   (unless (pair? ls) (mutation-error who))
                   (for-each (fx- n 1) ls))))))]
      [(f ls . more)
       (unless (procedure? f) (nonprocedure-error who f))
       (let ([n (length-check who ls more)])
         (unless (fx= n 0)
           (let for-each ([n n] [ls ls] [more more] [cars (map car more)])
             (if (fx= n 1)
                 (apply f (car ls) cars)
                 (begin
                   (apply f (car ls) cars)
                   (let ([ls (cdr ls)])
                     (unless (pair? ls) (mutation-error who))
                     (let-values ([(cdrs cars) (getcxrs more who)])
                       (for-each (fx- n 1) ls cdrs cars))))))))]))

  (set-who! fold-left 
    (case-lambda
      [(combine nil ls)
       (unless (procedure? combine) (nonprocedure-error who combine))
       (cond
         [(null? ls) nil]
         [else
          ($list-length ls who)
          (let fold-left ([ls ls] [acc nil])
            (let ([cdrls (cdr ls)])
              (if (pair? cdrls)
                  (fold-left cdrls (combine acc (car ls)))
                  (if (null? cdrls)
                      (combine acc (car ls))
                      (mutation-error who)))))])]
      [(combine nil ls . more)
       (unless (procedure? combine) (nonprocedure-error who combine))
       (length-check who ls more)
       (if (null? ls)
           nil
           (let fold-left ([ls ls] [more more] [cars (map car more)] [acc nil])
             (let ([cdrls (cdr ls)])
               (if (null? cdrls)
                   (apply combine acc (car ls) cars)
                   (let ([acc (apply combine acc (car ls) cars)])
                     (unless (pair? cdrls) (mutation-error who))
                     (let-values ([(cdrs cars) (getcxrs more who)])
                       (fold-left cdrls cdrs cars acc)))))))]))

  (set-who! fold-right 
    (case-lambda
      [(combine nil ls)
       (unless (procedure? combine) (nonprocedure-error who combine))
       ($list-length ls who)
      ; #3%fold-right naturally does cdrs first to avoid mutation sickness
       (#3%fold-right combine nil ls)]
      [(combine nil ls1 ls2)
       (unless (procedure? combine) (nonprocedure-error who combine))
       (unless (fx= ($list-length ls1 who) ($list-length ls2 who))
         (length-error who ls1 ls2))
      ; #3%fold-right naturally does cdrs first to avoid mutation sickness
       (#3%fold-right combine nil ls1 ls2)]
      [(combine nil ls . more)
       (unless (procedure? combine) (nonprocedure-error who combine))
       (length-check who ls more)
       (let fold-right ([combine combine] [nil nil] [ls ls] [more more])
         (if (null? ls)
             nil
             (apply combine (car ls)
               (#3%fold-right cons 
                 (list (fold-right combine nil (cdr ls) (map cdr more)))
                 (map car more)))))]))
)


(let ()
  (include "types.ss")
  
  (define disable/enable (make-winder disable-interrupts enable-interrupts '()))

  (define (dwind in body out)
    (let ((old-winders ($current-winders)))
      (in)
      ($current-winders (cons (make-winder in out ($current-attachments)) old-winders))
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
      ($current-winders (cons (make-critical-winder in out ($current-attachments)) old-winders))
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
              (if (critical-winder? w)
                  (begin
                    (disable-interrupts)
                    ($current-winders (cons disable/enable old))
                    ($current-attachments (winder-attachments w))
                    ((winder-out w))
                    ($current-winders old)
                    (enable-interrupts))
                  (begin
                    ($current-winders old)
                    ($current-attachments (winder-attachments w))
                    ((winder-out w))))
              (f old))))
        (let f ([new new])
          (unless (eq? new tail)
            (let ([w (car new)])
              (f (cdr new))
              (if (critical-winder? w)
                  (begin
                    (disable-interrupts)
                    ($current-winders (cons disable/enable (cdr new)))
                    ($current-attachments (winder-attachments w))
                    ((winder-in w))
                    ($current-winders new)
                    (enable-interrupts))
                  (begin
                    ($current-attachments (winder-attachments w))
                    ((winder-in w))
                    ($current-winders new)))))))))
  )

(set! continuation-marks?
      (let* ([mark-cache-key '#{cache-key mpsiogk70mywwtrxxqyso7lht-0}]
             [mark-not-found-key mark-cache-key])
        (define-record-type continuation-marks
          (fields (immutable markss))
          (nongenerative #{continuation-marks c32ju6acq6xzgmortvhjrnb9b-1})
          (opaque #t)
          (sealed #t))

        ;; a mark table as an attachment is an association list from keys to
        ;; values; the first thing in the list may be a cache, which should
        ;; be discarded if the mark table is updated

        (define (do-continuation-marks-first markss key none-v)
          (let loop ([markss markss] [slow-markss markss] [i 0])
            (define (result found)
              ;; cache at half the depth we had to look:
              (when (> i 10)
                (let* ([marks (car slow-markss)]
                       [has-cache? (and (pair? marks)
                                        (eq? mark-cache-key (caar marks)))]
                       [cache (if has-cache?
                                  (cdar marks)
                                  '())]
                       [new-cache (update-cache cache key found)])
                  ;; there's a small chance that multiple threads will try to update
                  ;; `slow-markss` at the same time, in which case one update can get
                  ;; lost, but it's ok to lose a cache update
                  (set-car! slow-markss (cons (cons mark-cache-key new-cache)
                                              (if has-cache? (cdr marks) marks)))))
              (if (eq? found mark-not-found-key)
                  none-v
                  found))
            (cond
              [(null? markss) (result mark-not-found-key)]
              [else
               (let ([a (let ([marks (car markss)])
                          (or (assq key marks)
                              (and (pair? marks)
                                   (eq? mark-cache-key (caar marks))
                                   (assq key (cdar marks)))))])
                 (if a
                     (result (cdr a))
                     (loop (cdr markss)
                           (if (fxodd? i)
                               (cdr slow-markss)
                               slow-markss)
                           (fx+ i 1))))])))

        (define update-cache 
          (lambda (cache key val)
            (cond
              [(null? cache) (list (ephemeron-cons key val))]
              [else
               (let ([k (caar cache)])
                 (cond
                   [(eq? k key) (cons (ephemeron-cons key val) (cdr cache))]
                   [(eq? k #!bwp) (update-cache (cdr cache) key val)]
                   [else (cons (car cache) (update-cache (cdr cache) key val))]))])))

        (set-who! $update-mark
          (lambda (marks key val)
            (cond
              [(null? marks) (list (cons key val))]
              [else
               (let loop ([marks (if (eq? (caar marks) mark-cache-key)
                                     (cdr marks)
                                     marks)])
                 (cond
                   [(null? marks) (list (cons key val))]
                   [(eq? (caar marks) key) (cons (cons key val) (cdr marks))]
                   [else (cons (car marks) (loop (cdr marks)))]))])))

        (set-who! current-continuation-marks
          (lambda ()
            (make-continuation-marks ($current-attachments))))

        (set-who! continuation-next-marks
          (lambda (c)
            (unless ($continuation? c)
              ($oops who "~s is not a continuation" c))
            (make-continuation-marks ($continuation-attachments c))))

        (set-who! continuation-marks->iterator
          (rec continuation-marks->iterator
               (case-lambda
                [(set keys) (continuation-marks->iterator set keys #f)]
                [(set keys none-val)
                 (unless (continuation-marks? set)
                   ($oops who "~s is not a continuation mark sequence" set))
                 (unless (vector? keys)
                   ($oops who "~s is not a vector" keys))
                 (let gen ([markss (continuation-marks-markss set)])
                   (lambda ()
                     (let loop ([markss markss])
                       (cond
                         [(null? markss) (values #f (gen '()))]
                         [else
                          (let* ([marks (car markss)]
                                 [hit? #f]
                                 [vec (vector-map (lambda (key)
                                                    (let ([a (assq key marks)])
                                                      (if a
                                                          (begin
                                                            (set! hit? #t)
                                                            (cdr a))
                                                          none-val)))
                                                  keys)])
                            (if hit?
                                (values vec
                                        (gen (cdr markss)))
                                (loop (cdr markss))))]))))])))

        (set-who! continuation-marks->list
          (lambda (set key)
            (unless (continuation-marks? set)
              ($oops who "~s is not a continuation mark sequence" set))
            (let loop ([markss (continuation-marks-markss set)])
              (cond
                [(null? markss) '()]
                [else
                 (let ([marks (car markss)])
                   (let ([a (assq key marks)])
                     (if a
                         (cons (cdr a) (loop (cdr markss)))
                         (loop (cdr markss)))))]))))

        (set-who! continuation-marks-first
          (rec continuation-marks-first
               (case-lambda
                [(set key) (continuation-marks-first set key #f)]
                [(set key none-v)
                 (unless (continuation-marks? set)
                   ($oops who "~s is not a continuation mark sequence" set))
                 (do-continuation-marks-first (continuation-marks-markss set) key none-v)])))

        ;; co0 shortcuts an immediate `(current-continuation-marks)`:
        (set-who! $continuation-marks-first
          (case-lambda
           [(key) (do-continuation-marks-first ($current-attachments) key #f)]
           [(key none-v) (do-continuation-marks-first ($current-attachments) key none-v)]))

        (set-who! call-with-immediate-continuation-mark
          (rec call-with-immediate-continuation-mark
               (case-lambda
                [(key proc) (call-with-immediate-continuation-mark key #f proc)]
                [(key none-v proc)
                 (unless (procedure? proc)
                   ($oops who "~s is not a procedure" proc))
                 ($call-getting-continuation-attachment
                  '()
                  (lambda (marks)
                    (proc (let ([a (assq key marks)])
                            (if a
                                (cdr a)
                                none-v)))))])))

        (set-who! call-in-continuation
          (case-lambda
           [(c proc)
            (unless ($continuation? c) ($oops who "~s is not a continuation" c))
            (unless (procedure? proc) ($oops who "~s is not a procedure" proc))
            ($call-in-continuation c proc)]
           [(c set proc)
            (unless ($continuation? c) ($oops who "~s is not a continuation" c))
            (unless (continuation-marks? set) ($oops who "~s is not a continuation mark sequence" set))
            (unless (procedure? proc) ($oops who "~s is not a procedure" proc))
            (let ([markss (continuation-marks-markss set)]
                  [c-markss ($continuation-attachments c)])
              (unless (or (eq? markss c-markss)
                          (and (pair? markss) (eq? (cdr markss) c-markss)))
                ($oops who "~s is not an extension of the marks of ~s" set c))
              ($call-in-continuation c markss proc))]))

        continuation-marks?))

;;; make-promise and force

(define-who $make-promise
  (lambda (thunk)
    (unless (procedure? thunk)
      ($oops who "~s is not a procedure" thunk))
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

(define-who force
   (lambda (promise)
      (unless (procedure? promise)
         ($oops who "~s is not a procedure" promise))
      (promise)))
)
