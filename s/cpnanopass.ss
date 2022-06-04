;;; cpnanopass.ss
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
  (include "np-languages.ss")

  (define track-dynamic-closure-counts ($make-thread-parameter #f (lambda (x) (and x #t))))

  (define track-static-closure-counts
    ($make-thread-parameter
      #f
      (lambda (x)
        (include "types.ss")
        (cond
          [(or (not x) (static-closure-info? x)) x]
          [(eq? x #t) (make-static-closure-info)]
          [else ($oops '$trace-static-closure-counts "~s is not a static-closure-info record or #f" x)]))))

  (module ()
    (include "types.ss")

    (set-who! $dynamic-closure-counts
      (lambda ()
        (vector
          (profile-counter-count #{raw-ref-count bhowt6w0coxl0s2y-1})
          (profile-counter-count #{raw-create-count bhowt6w0coxl0s2y-2})
          (profile-counter-count #{raw-alloc-count bhowt6w0coxl0s2y-3})
          (profile-counter-count #{ref-count bhowt6w0coxl0s2y-4})
          (profile-counter-count #{pair-create-count bhowt6w0coxl0s2y-5})
          (profile-counter-count #{vector-create-count bhowt6w0coxl0s2y-6})
          (profile-counter-count #{vector-alloc-count bhowt6w0coxl0s2y-8})
          (profile-counter-count #{padded-vector-alloc-count bhowt6w0coxl0s2y-11})
          (profile-counter-count #{closure-create-count bhowt6w0coxl0s2y-7})
          (profile-counter-count #{closure-alloc-count bhowt6w0coxl0s2y-9})
          (profile-counter-count #{padded-closure-alloc-count bhowt6w0coxl0s2y-10}))))

    (set-who! $clear-dynamic-closure-counts
      (lambda ()
        (profile-counter-count-set! #{raw-ref-count bhowt6w0coxl0s2y-1} 0)
        (profile-counter-count-set! #{raw-create-count bhowt6w0coxl0s2y-2} 0)
        (profile-counter-count-set! #{raw-alloc-count bhowt6w0coxl0s2y-3} 0)
        (profile-counter-count-set! #{ref-count bhowt6w0coxl0s2y-4} 0)
        (profile-counter-count-set! #{pair-create-count bhowt6w0coxl0s2y-5} 0)
        (profile-counter-count-set! #{vector-create-count bhowt6w0coxl0s2y-6} 0)
        (profile-counter-count-set! #{vector-alloc-count bhowt6w0coxl0s2y-8} 0)
        (profile-counter-count-set! #{padded-vector-alloc-count bhowt6w0coxl0s2y-11} 0)
        (profile-counter-count-set! #{closure-create-count bhowt6w0coxl0s2y-7} 0)
        (profile-counter-count-set! #{closure-alloc-count bhowt6w0coxl0s2y-9} 0)
        (profile-counter-count-set! #{padded-closure-alloc-count bhowt6w0coxl0s2y-10} 0))))

  (define-syntax traceit
    (syntax-rules (x)
      [(_ name) (set! name (let ([t name]) (trace-lambda name args (apply t args))))]))

  (define-syntax architecture
    (let ([fn (format "~a.ss" (constant architecture))])
      (with-source-path 'architecture fn
        (lambda (fn)
          (let* ([p ($open-file-input-port 'include fn)]
                 [sfd ($source-file-descriptor fn p)]
                 [p (transcoded-port p (current-transcoder))])
            (let ([do-read ($make-read p sfd 0)])
              (let* ([regs (do-read)] [inst (do-read)] [asm (do-read)])
                (when (eof-object? asm) ($oops #f "too few expressions in ~a" fn))
                (unless (eof-object? (do-read)) ($oops #f "too many expressions in ~a" fn))
                (close-input-port p)
                (lambda (x)
                  (syntax-case x (registers instructions assembler)
                    [(k registers) (datum->syntax #'k regs)]
                    [(k instructions) (datum->syntax #'k inst)]
                    [(k assembler) (datum->syntax #'k asm)])))))))))

  ; version in cmacros uses keyword as template and should
  ; probably be changed to use the id
  (define-syntax define-who
    (lambda (x)
      (syntax-case x ()
        [(_ (id . args) b1 b2 ...)
         (identifier? #'id)
         #'(define-who id (lambda args b1 b2 ...))]
        [(_ id e)
         (identifier? #'id)
         (with-implicit (id who)
           #'(define id (let ([who 'id]) e)))])))

  (module (get-passes pass xpass pass-time?)
    (define-syntax passes-loc (make-compile-time-value (box '())))
    (define-syntax get-passes
      (lambda (x)
        (lambda (r)
          (syntax-case x ()
            [(_) #`(unbox (quote #,(datum->syntax #'* (r #'passes-loc))))]))))
    (module (pass)
      (define ir-printer
        (lambda (unparser)
          (lambda (val*)
            (safe-assert (not (null? val*)))
            (pretty-print (flatten-seq (unparser (car val*)))))))
      (define values-printer
        (lambda (val*)
          (if (null? val*)
              (printf "no output\n")
              (pretty-print (car val*)))))
      (define-syntax pass
        (syntax-rules ()
          [(_ (pass-name ?arg ...) ?unparser)
           (identifier? #'pass-name)
           (let ([pass-name (pass-name ?arg ...)])
             (lambda args (xpass pass-name (ir-printer ?unparser) args)))]
          [(_ pass-name ?unparser)
           (identifier? #'pass-name)
           (lambda args (xpass pass-name (ir-printer ?unparser) args))]
          [(_ (pass-name ?arg ...))
           (identifier? #'pass-name)
           (let ([pass-name (pass-name ?arg ...)])
             (lambda args (xpass pass-name values-printer args)))]
          [(_ pass-name)
           (identifier? #'pass-name)
           (lambda args (xpass pass-name values-printer args))])))
    (module (xpass pass-time?)
      (define-threaded pass-time?)
      (define $xpass
        (lambda (printer pass-name pass arg*)
          (let-values ([val* (let ([th (lambda () (apply pass arg*))])
                               (if pass-time? ($pass-time pass-name th) (th)))])
            (when (memq pass-name (tracer))
              (printf "output of ~s:\n" pass-name)
              (printer val*))
            (apply values val*))))
      (define-syntax xpass
        (lambda (x)
          (syntax-case x ()
            [(_ pass-name ?printer ?args)
             (lambda (r)
               (let ([loc (r #'passes-loc)])
                 (set-box! loc (cons (datum pass-name) (unbox loc))))
               #`($xpass ?printer 'pass-name pass-name ?args))]))))
    (define flatten-seq
      (lambda (x)
        (define helper
          (lambda (x*)
            (if (null? x*)
                '()
                (let ([x (car x*)])
                  (if (and (pair? x) (eq? (car x) 'seq))
                      (append (helper (cdr x)) (helper (cdr x*)))
                      (cons (flatten-seq x) (helper (cdr x*))))))))
        (cond
          [(null? x) '()]
          [(and (pair? x) (eq? (car x) 'seq))
           (let ([x* (helper (cdr x))])
             (if (fx= (length x*) 1)
                 (car x*)
                 (cons 'begin x*)))]
          [(and (pair? x) (eq? (car x) 'quote)) x]
          [(list? x) (map flatten-seq x)]
          [else x]))))

  (define compose
    (lambda (v p . p*)
      (let loop ([v* (list v)] [p p] [p* p*])
        (if (null? p*)
            (apply p v*)
            (let-values ([v* (apply p v*)])
              (loop v* (car p*) (cdr p*)))))))

  (define-syntax with-virgin-quasiquote
    (lambda (x)
      (syntax-case x ()
        [(k e1 e2 ...)
         #`(let-syntax ([#,(datum->syntax #'k 'quasiquote)
                         (syntax-rules () [(_ x) `x])])
               e1 e2 ...)])))

  (define valid-pass?
    (lambda (x)
      (memq x (get-passes))))

  (define last-pass              ; potentially not thread-safe, but currently unused
    (make-parameter #f
      (lambda (x)
        (unless (or (eq? x #f) (valid-pass? x))
          (errorf 'last-pass "~s is not a valid pass" x))
        x)))

  (define tracer              ; potentially not thread-safe, but currently unused
    (let ([ls '()])
      (case-lambda
        [() ls]
        [(x)
         (cond
           [(or (null? x) (not x)) (set! ls '())]
           [(eq? x #t) (set! ls (get-passes))]
           [(valid-pass? x) (set! ls (cons x ls))]
           [(list? x) (for-each tracer x)]
           [else (errorf 'tracer "invalid trace list or pass name: ~s" x)])])))

  (define maybe-cons
    (lambda (x ls)
      (if x (cons x ls) ls)))

  (define unannotate
    (lambda (x)
      (if (annotation? x)
          (annotation-expression x)
          x)))

  (let ()
    (import (nanopass) np-languages)

    (define signed-32?
      (let ([n (bitwise-arithmetic-shift-left 1 (fx- 32 1))])
        (let ([low (- n)] [high (- n 1)])
          (if (fixnum? low)
              (lambda (x) (and (fixnum? x) (fx<= low x high)))
              (lambda (x) (or (fixnum? x) (<= low x high)))))))

    (define nodups
      (lambda x**
        (let ([x* (apply append x**)])
          (let ([ans (andmap (lambda (x) (and (not (uvar-seen? x)) (uvar-seen! x #t) #t)) x*)])
            (for-each (lambda (x) (uvar-seen! x #f)) x*)
            ans))))

    (define chunked-bytevector-bitcount
     ; assumes "chunked" bytevector a multiple of 2 in size
      (let ([bitcount-bv (make-bytevector #x10000)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i #x10000))
          (bytevector-u8-set! bitcount-bv i (fxbit-count i)))
        (lambda (bv)
          (let loop ([n (bytevector-length bv)] [count 0])
            (if (fx= n 0)
                count
                (let ([n (fx- n 2)])
                  (loop n (fx+ (bytevector-u8-ref bitcount-bv
                                 (bytevector-u16-native-ref bv n))
                            count))))))))

    (module (empty-tree full-tree tree-extract tree-for-each tree-fold-left tree-bit-set? tree-bit-set tree-bit-unset tree-bit-count tree-same? tree-merge)
      ; tree -> fixnum | (tree-node tree tree)
      ; 0 represents any tree or subtree with no bits set, and a tree or subtree
      ; with no bits set is always 0
      (define empty-tree 0)

      ; any tree or subtree with all bits set
      (define full-tree #t)

      (define (full-fixnum size) (fxsrl (most-positive-fixnum) (fx- (fx- (fixnum-width) 1) size)))

      (define compute-split
        (lambda (size)
          (fxsrl size 1)
          ; 2015/03/15 rkd: tried the following under the theory that we'd allocate
          ; fewer nodes.  for example, say fixmun-width is 30 and size is 80.  if we
          ; split 40/40 we create two nodes under the current node.  if instead we
          ; split 29/51 we create just one node and one fixnum under the current
          ; node.  this worked as planned; however, it reduced the number of nodes
          ; created by only 3.3% on the x86 and made compile times slightly worse.
          #;(if (fx<= size (fx* (fx- (fixnum-width) 1) 3)) (fx- (fixnum-width) 1) (fxsrl size 1))))

      (meta-cond
        [(fx= (optimize-level) 3)
         (module (make-tree-node tree-node? tree-node-left tree-node-right)
           (define make-tree-node cons)
           (define tree-node? pair?)
           (define tree-node-left car)
           (define tree-node-right cdr))]
        [else
         (module (make-tree-node tree-node? tree-node-left tree-node-right)
           (define-record-type tree-node
             (nongenerative)
             (sealed #t)
             (fields left right)
             (protocol
               (lambda (new)
                 (lambda (left right)
                   (new left right)))))
           (record-writer (record-type-descriptor tree-node)
             (lambda (r p wr)
               (define tree-node->s-exp
                 (lambda (tn)
                   (with-virgin-quasiquote
                     (let ([left (tree-node-left tn)] [right (tree-node-right tn)])
                       `(tree-node
                          ,(if (tree-node? left) (tree-node->s-exp left) left)
                          ,(if (tree-node? right) (tree-node->s-exp right) right))))))
               (wr (tree-node->s-exp r) p))))])

      (define tree-extract ; assumes empty-tree is 0
        (lambda (st size v)
          (let extract ([st st] [size size] [offset 0] [x* '()])
            (cond
              [(fixnum? st)
                (do ([st st (fxsrl st 1)]
                     [offset offset (fx+ offset 1)]
                     [x* x* (if (fxodd? st) (cons (vector-ref v offset) x*) x*)])
                 ((fx= st 0) x*))]
              [(eq? st full-tree)
               (do ([size size (fx- size 1)]
                    [offset offset (fx+ offset 1)]
                    [x* x* (cons (vector-ref v offset) x*)])
                 ((fx= size 0) x*))]
              [else
                (let ([split (compute-split size)])
                  (extract (tree-node-right st) (fx- size split) (fx+ offset split)
                    (extract (tree-node-left st) split offset x*)))]))))

      (define tree-for-each ; assumes empty-tree is 0
        (lambda (st size start end action)
          (let f ([st st] [size size] [start start] [end end] [offset 0])
            (cond
              [(fixnum? st)
               (unless (eq? st empty-tree)
                 (do ([st (fxbit-field st start end) (fxsrl st 1)] [offset (fx+ offset start) (fx+ offset 1)])
                     ((fx= st 0))
                   (when (fxodd? st) (action offset))))]
              [(eq? st full-tree)
               (do ([start start (fx+ start 1)] [offset offset (fx+ offset 1)])
                   ((fx= start end))
                 (action offset))]
              [else
               (let ([split (compute-split size)])
                 (when (fx< start split)
                   (f (tree-node-left st) split start (fxmin end split) offset))
                 (when (fx> end split)
                   (f (tree-node-right st) (fx- size split) (fxmax (fx- start split) 0) (fx- end split) (fx+ offset split))))]))))

      (define tree-fold-left ; assumes empty-tree is 0
        (lambda (proc size init st)
          (let f ([st st] [size size] [offset 0] [init init])
            (cond
              [(fixnum? st)
                (do ([st st (fxsrl st 1)]
                     [offset offset (fx+ offset 1)]
                     [init init (if (fxodd? st) (proc init offset) init)])
                 ((fx= st 0) init))]
              [(eq? st full-tree)
               (do ([size size (fx- size 1)]
                    [offset offset (fx+ offset 1)]
                    [init init (proc init offset)])
                 ((fx= size 0) init))]
              [else
                (let ([split (compute-split size)])
                  (f (tree-node-left st) split offset
                    (f (tree-node-right st) (fx- size split) (fx+ offset split) init)))]))))

      (define tree-bit-set? ; assumes empty-tree is 0
        (lambda (st size bit)
          (let loop ([st st] [size size] [bit bit])
            (cond
              [(fixnum? st)
                (and (not (eqv? st empty-tree))
                     ; fxlogbit? is unnecessarily general, so roll our own
                    (fxlogtest st (fxsll 1 bit)))]
              [(eq? st full-tree) #t]
              [else
                (let ([split (compute-split size)])
                  (if (fx< bit split)
                      (loop (tree-node-left st) split bit)
                      (loop (tree-node-right st) (fx- size split) (fx- bit split))))]))))

      (define tree-bit-set ; assumes empty-tree is 0
        (lambda (st size bit)
          ; set bit in tree.  result is eq? to tr if result is same as tr.
          (cond
            [(eq? st full-tree) st]
            [(fx< size (fixnum-width))
             (let ([st (fxlogbit1 bit st)])
               (if (fx= st (full-fixnum size))
                   full-tree
                   st))]
            [else
              (let ([split (compute-split size)])
                (if (eqv? st empty-tree)
                    (if (fx< bit split)
                        (make-tree-node (tree-bit-set empty-tree split bit) empty-tree)
                        (make-tree-node empty-tree (tree-bit-set empty-tree (fx- size split) (fx- bit split))))
                    (let ([lst (tree-node-left st)] [rst (tree-node-right st)])
                      (if (fx< bit split)
                          (let ([new-lst (tree-bit-set lst split bit)])
                            (if (eq? new-lst lst)
                                st
                                (if (and (eq? new-lst full-tree) (eq? rst full-tree))
                                    full-tree
                                    (make-tree-node new-lst rst))))
                          (let ([new-rst (tree-bit-set rst (fx- size split) (fx- bit split))])
                            (if (eq? new-rst rst)
                                st
                                (if (and (eq? lst full-tree) (eq? new-rst full-tree))
                                    full-tree
                                    (make-tree-node lst new-rst))))))))])))

      (define tree-bit-unset ; assumes empty-tree is 0
        (lambda (st size bit)
          ; reset bit in tree.  result is eq? to tr if result is same as tr.
          (cond
            [(fixnum? st)
             (if (eqv? st empty-tree)
                 empty-tree
                 (fxlogbit0 bit st))]
            [(eq? st full-tree)
             (if (fx< size (fixnum-width))
                 (fxlogbit0 bit (full-fixnum size))
                 (let ([split (compute-split size)])
                   (if (fx< bit split)
                       (make-tree-node (tree-bit-unset full-tree split bit) full-tree)
                       (make-tree-node full-tree (tree-bit-unset full-tree (fx- size split) (fx- bit split))))))]
            [else
              (let ([split (compute-split size)] [lst (tree-node-left st)] [rst (tree-node-right st)])
                (if (fx< bit split)
                    (let ([new-lst (tree-bit-unset lst split bit)])
                      (if (eq? new-lst lst)
                          st
                          (if (and (eq? new-lst empty-tree) (eq? rst empty-tree))
                              empty-tree
                              (make-tree-node new-lst rst))))
                    (let ([new-rst (tree-bit-unset rst (fx- size split) (fx- bit split))])
                      (if (eq? new-rst rst)
                          st
                          (if (and (eq? lst empty-tree) (eq? new-rst empty-tree))
                              empty-tree
                              (make-tree-node lst new-rst))))))])))

      (define tree-bit-count ; assumes empty-tree is 0
        (lambda (st size)
          (cond
            [(fixnum? st) (fxbit-count st)]
            [(eq? st full-tree) size]
            [else
              (let ([split (compute-split size)])
                (fx+
                  (tree-bit-count (tree-node-left st) split)
                  (tree-bit-count (tree-node-right st) (fx- size split))))])))

      (define tree-same? ; assumes empty-tree is 0
        (lambda (st1 st2)
          (or (eq? st1 st2) ; assuming fixnums and full trees are eq-comparable
              (and (tree-node? st1)
                   (tree-node? st2)
                   (tree-same? (tree-node-left st1) (tree-node-left st2))
                   (tree-same? (tree-node-right st1) (tree-node-right st2))))))

      (define tree-merge
       ; merge tr1 and tr2.  result is eq? to tr1 if result is same as tr1.
        (lambda (st1 st2 size)
          (cond
            [(or (eq? st1 st2) (eq? st2 empty-tree)) st1]
            [(eq? st1 empty-tree) st2]
            [(or (eq? st1 full-tree) (eq? st2 full-tree)) full-tree]
            [(fixnum? st1)
             (safe-assert (fixnum? st2))
             (let ([st (fxlogor st1 st2)])
               (if (fx= st (full-fixnum size))
                   full-tree
                   st))]
            [else
             (let ([lst1 (tree-node-left st1)]
                   [rst1 (tree-node-right st1)]
                   [lst2 (tree-node-left st2)]
                   [rst2 (tree-node-right st2)])
               (let ([split (compute-split size)])
                 (let ([l (tree-merge lst1 lst2 split)] [r (tree-merge rst1 rst2 (fx- size split))])
                 (cond
                   [(and (eq? l lst1) (eq? r rst1)) st1]
                   [(and (eq? l lst2) (eq? r rst2)) st2]
                   [(and (eq? l full-tree) (eq? r full-tree)) full-tree]
                   [else (make-tree-node l r)]))))]))))

    (define-syntax tc-disp
      (lambda (x)
        (syntax-case x ()
          [(_ name)
           (case (datum name)
             [(%ac0) (constant tc-ac0-disp)]
             [(%ac1) (constant tc-ac1-disp)]
             [(%sfp) (constant tc-sfp-disp)]
             [(%cp) (constant tc-cp-disp)]
             [(%esp) (constant tc-esp-disp)]
             [(%ap) (constant tc-ap-disp)]
             [(%eap) (constant tc-eap-disp)]
             [(%trap) (constant tc-trap-disp)]
             [(%xp) (constant tc-xp-disp)]
             [(%yp) (constant tc-yp-disp)]
             [else #f])])))

    (define-syntax define-reserved-registers
      (lambda (x)
        (syntax-case x ()
          [(_ [regid alias ... callee-save? mdinfo] ...)
           (syntax-case #'(regid ...) (%tc %sfp) [(%tc %sfp . others) #t] [_ #f])
           #'(begin
               (begin
                 (define regid (make-reg 'regid 'mdinfo (tc-disp regid) callee-save?))
                 (module (alias ...) (define x regid) (define alias x) ...))
               ...)])))

    (define-syntax define-allocable-registers
      (lambda (x)
        (assert (fx<= (constant asm-arg-reg-cnt) (constant asm-arg-reg-max)))
        (syntax-case x ()
          [(_ regvec arg-registers extra-registers with-initialized-registers [regid reg-alias ... callee-save? mdinfo] ...)
           (with-syntax ([((tc-disp ...) (arg-regid ...) (extra-regid ...))
                          (syntax-case #'(regid ...) (%ac0 %xp %ts %td)
                            [(%ac0 %xp %ts %td other ...)
                             (let f ([other* #'(other ...)]
                                     [rtc-disp* '()]
                                     [arg-offset (constant tc-arg-regs-disp)]
                                     [rextra* '()])
                               (if (null? other*)
                                   (if (fx= (length rextra*) (constant asm-arg-reg-max))
                                       (let ([extra* (reverse rextra*)])
                                         (list
                                           (list*
                                             (constant tc-ac0-disp)
                                             (constant tc-xp-disp)
                                             (constant tc-ts-disp)
                                             (constant tc-td-disp)
                                             (reverse rtc-disp*))
                                           (list-head extra* (constant asm-arg-reg-cnt))
                                           (list-tail extra* (constant asm-arg-reg-cnt))))
                                       (syntax-error x (format "asm-arg-reg-max extra registers are not specified ~s" (syntax->datum rextra*))))
                                   (let ([other (car other*)])
                                     (if (memq (syntax->datum other) '(%ac1 %yp %cp %ret))
                                         (f (cdr other*) (cons #`(tc-disp #,other) rtc-disp*)
                                            arg-offset rextra*)
                                         (f (cdr other*) (cons arg-offset rtc-disp*)
                                            (fx+ arg-offset (constant ptr-bytes)) (cons other rextra*))))))]
                            [_ (syntax-error x "missing or out-of-order required registers")])]
                         [(regid-loc ...) (generate-temporaries #'(regid ...))])
             #'(begin
                 (define-syntax define-squawking-parameter
                   (syntax-rules ()
                     [(_ (id (... ...)) loc)
                      (begin
                        (define loc ($make-thread-parameter #f))
                        (define-syntax id
                          (lambda (q)
                            (unless (identifier? q) (syntax-error q))
                            #`(let ([x (loc)])
                                (unless x (syntax-error #'#,q "uninitialized"))
                                x)))
                        (... ...))]
                     [(_ id loc) (define-squawking-parameter (id) loc)]))
                 (define-squawking-parameter (regid reg-alias ...) regid-loc)
                 ...
                 (define-squawking-parameter regvec regvec-loc)
                 (define-squawking-parameter arg-registers arg-registers-loc)
                 (define-squawking-parameter extra-registers extra-registers-loc)
                 (define-syntax with-initialized-registers
                   (syntax-rules ()
                     [(_ b1 b2 (... ...))
                      (parameterize ([regid-loc (make-reg 'regid 'mdinfo tc-disp callee-save?)] ...)
                        (parameterize ([regvec-loc (vector regid ...)]
                                       [arg-registers-loc (list arg-regid ...)]
                                       [extra-registers-loc (list extra-regid ...)])
                          (let () b1 b2 (... ...))))]))))])))

    (define-syntax define-machine-dependent-registers
      (lambda (x)
        (syntax-case x ()
          [(_ [regid alias ... callee-save? mdinfo] ...)
           #'(begin
               (begin
                 (define regid (make-reg 'regid 'mdinfo #f callee-save?))
                 (module (alias ...) (define x regid) (define alias x) ...))
               ...)])))

    (define-syntax define-registers
      (lambda (x)
        (syntax-case x (reserved allocable machine-dependent)
          [(k (reserved [rreg rreg-alias ... rreg-callee-save? rreg-mdinfo] ...)
              (allocable [areg areg-alias ... areg-callee-save? areg-mdinfo] ...)
              (machine-dependent [mdreg mdreg-alias ... mdreg-callee-save? mdreg-mdinfo] ...))
           (with-implicit (k regvec arg-registers extra-registers real-register? with-initialized-registers)
             #`(begin
                 (define-reserved-registers [rreg rreg-alias ... rreg-callee-save? rreg-mdinfo] ...)
                 (define-allocable-registers regvec arg-registers extra-registers with-initialized-registers [areg areg-alias ... areg-callee-save? areg-mdinfo] ...)
                 (define-machine-dependent-registers [mdreg mdreg-alias ... mdreg-callee-save? mdreg-mdinfo] ...)
                 (define-syntax real-register?
                   (with-syntax ([real-reg* #''(rreg ... rreg-alias ... ... areg ... areg-alias ... ... mdreg ... mdreg-alias ... ...)])
                     (syntax-rules ()
                       [(_ e) (memq e real-reg*)])))))])))

    (architecture registers)

    ; pseudo register used for mref's with no actual index
    (define %zero (make-reg 'zero #f #f #f))

    ; define %ref-ret to be sfp[0] on machines w/no ret register
    (define-syntax %ref-ret
      (lambda (x)
        (meta-cond
          [(real-register? '%ret) #'%ret]
          [else (with-syntax ([%mref (datum->syntax x '%mref)])
                  #'(%mref ,%sfp 0))])))

    (define make-Ldoargerr
      (lambda ()
        (make-libspec-label 'doargerr (lookup-libspec doargerr)
          (reg-list %ret %ac0 %cp))))
    (define make-Ldomvleterr
      (lambda ()
        (make-libspec-label 'domvleterr (lookup-libspec domvleterr)
          (reg-list %ret %ac0))))
    (define make-Lcall-error
      (lambda ()
        (make-libspec-label 'call-error (lookup-libspec call-error)
          (reg-list %ret %cp))))

    (module (frame-vars get-fv)
      (define-threaded frame-vars)
      (define get-fv
        (lambda (x)
          (let ([n (vector-length frame-vars)])
            (when (fx>= x n)
              (let ([new-vec (make-vector (fxmax (fx+ x 1) (fx* n 2)) #f)])
                (let loop ([n n])
                  (unless (fx= n 0)
                    (let ([n (fx- n 1)])
                      (vector-set! new-vec n (vector-ref frame-vars n))
                      (loop n))))
                (set! frame-vars new-vec))))
          (or (vector-ref frame-vars x)
              (let ([fv ($make-fv x)])
                (vector-set! frame-vars x fv)
                fv)))))

    (define-syntax reg-cons*
      (lambda (x)
        (syntax-case x ()
          [(_ ?reg ... ?reg*)
           (fold-right
             (lambda (reg reg*)
               (if (real-register? (syntax->datum reg))
                   #`(cons #,reg #,reg*)
                   reg*))
             #'?reg* #'(?reg ...))])))

    (define-syntax reg-list
      (syntax-rules ()
        [(_ ?reg ...) (reg-cons* ?reg ... '())]))

    (define-syntax with-saved-ret-reg
      (lambda (x)
        (syntax-case x ()
          [(k ?e)
           (if (real-register? '%ret)
               (with-implicit (k %seq %mref)
                 #'(%seq
                     (set! ,(%mref ,%sfp 0) ,%ret)
                     ,?e
                     (set! ,%ret ,(%mref ,%sfp 0))))
               #'?e)])))

    (module (restore-scheme-state save-scheme-state with-saved-scheme-state)
      (define-syntax build-reg-list
        ; TODO: create reg records at compile time, and build these lists at compile time
        ; TODO: include ts & td
        ; TODO: specify three lists: things that need to be saved/restored via the thread context,
        ; things that need to be saved/restored somehow, and things that can be trashed
        (lambda (x)
          (syntax-case x (base-in in out)
            [(_ orig-x (base-in base-inreg ...) (in inreg ...) (out outreg ...))
             (let ([all* '(%ts %td %ac0 %ac1 %cp %xp %yp scheme-args extra-regs)]
                   [in* (datum (inreg ...))]
                   [out* (datum (outreg ...))])
               (define remove*
                 (lambda (x* ls)
                   (if (null? x*)
                       ls
                       (remove* (cdr x*) (remq (car x*) ls)))))
               (let ([bogus* (remove* all* in*)])
                 (unless (equal? bogus* '()) (syntax-error #'orig-x (format "bogus in registers ~s" bogus*))))
               (let ([bogus* (remove* all* out*)])
                 (unless (equal? bogus* '()) (syntax-error #'orig-x (format "bogus out registers ~s" bogus*))))
               (unless (equal? (remove* in* out*) out*)
                 (syntax-error #'orig-x "non-empty intersection"))
               (let ([other* (remove* in* (remove* out* all*))])
                 (unless (null? other*)
                   (syntax-error #'orig-x (format "registers not mentioned: ~s" other*))))
               (with-syntax ([(in ...) (datum->syntax #'*
                                         (filter (lambda (x) (real-register? x))
                                           (append (datum (base-inreg ...)) in*)))])
                 #`(cons* (ref-reg in) ...
                     #,(if (memq 'scheme-args in*)
                           (if (memq 'extra-regs in*)
                               #'(append arg-registers extra-registers)
                               #'arg-registers)
                           (if (memq 'extra-regs in*)
                               #'extra-registers
                               #''())))))])))
      (define-syntax get-tcslot
        (lambda (x)
          (syntax-case x ()
            [(_ k reg)
             (with-implicit (k in-context %mref)
               #'(in-context Lvalue
                   (%mref ,%tc ,(reg-tc-disp reg))))])))
      (define-syntax $save-scheme-state
        (lambda (x)
          (syntax-case x ()
            [(_ k orig-x in out)
             (with-implicit (k quasiquote)
               ; although eap might be changed by dirty writes, and esp might be changed by
               ; one-shot continuation handling, we always write through to the tc so that
               ; we never need to save eap or esp and also so that eap, which serves as the
               ; base of the current dirty list, is always accurate, even when an invalid
               ; memory reference or invalid instruction occurs.  so we leave eap and esp
               ; out of the save list (but not the restore list below).
               #'(let ([regs-to-save (build-reg-list orig-x (base-in %sfp %ap %trap) in out)])
                   (fold-left (lambda (body reg)
                                `(seq (set! ,(get-tcslot k reg) ,reg) ,body))
                     `(nop) regs-to-save)))])))
      (define-syntax $restore-scheme-state
        (lambda (x)
          (syntax-case x ()
            [(_ k orig-x in out)
             (with-implicit (k quasiquote)
               #'(let ([regs-to-restore (build-reg-list orig-x (base-in %sfp %ap %trap %eap %esp) in out)])
                   (fold-left (lambda (body reg)
                                `(seq (set! ,reg ,(get-tcslot k reg)) ,body))
                     `(nop) regs-to-restore)))])))
      (define-syntax save-scheme-state
        (lambda (x)
          (syntax-case x ()
            [(k in out) #`($save-scheme-state k #,x in out)])))
      (define-syntax restore-scheme-state
        (lambda (x)
          (syntax-case x ()
            [(k in out) #`($restore-scheme-state k #,x in out)])))
      (define-syntax with-saved-scheme-state
        (lambda (x)
          (syntax-case x ()
            [(k in out ?body)
             (with-implicit (k quasiquote %seq)
               #`(%seq
                   ,($save-scheme-state k #,x in out)
                   ,?body
                   ,($restore-scheme-state k #,x in out)))]))))

    (define-record-type ctci ; compile-time version of code-info
      (nongenerative)
      (sealed #t)
      (fields (mutable live) (mutable rpi*) (mutable closure-fv-names))
      (protocol
        (lambda (new)
          (lambda ()
            (new #f '() #f)))))

    (define-record-type ctrpi ; compile-time version of rp-info
      (nongenerative)
      (sealed #t)
      (fields label src sexpr mask))

    (define-threaded next-lambda-seqno)

    (define-record-type info-lambda (nongenerative)
      (parent info)
      (sealed #t)
      (fields src sexpr libspec interface* (mutable dcl*) (mutable flags) (mutable fv*) (mutable name)
        (mutable well-known?) (mutable closure-rep) ctci (mutable pinfo*) seqno)
      (protocol
        (lambda (pargs->new)
          (define next-seqno
            (lambda ()
              (let ([seqno next-lambda-seqno])
                (set! next-lambda-seqno (fx+ seqno 1))
                seqno)))
          (rec cons-info-lambda
            (case-lambda
              [(src sexpr libspec interface*) (cons-info-lambda src sexpr libspec interface* #f 0)]
              [(src sexpr libspec interface* name) (cons-info-lambda src sexpr libspec interface* name 0)]
              [(src sexpr libspec interface* name flags)
               ((pargs->new) src sexpr libspec interface*
                (map (lambda (iface) (make-direct-call-label 'dcl)) interface*)
                (if (eq? (subset-mode) 'system) (fxlogor flags (constant code-flag-system)) flags)
                '() name #f 'closure (and (generate-inspector-information) (make-ctci)) '() (next-seqno))])))))

    (define-record-type info-call (nongenerative)
      (parent info)
      (sealed #t)
      (fields src sexpr (mutable check?) pariah? error?)
      (protocol
        (lambda (pargs->new)
          (lambda (src sexpr check? pariah? error?)
            ((pargs->new) src sexpr check? pariah? error?)))))

    (define-record-type info-newframe (nongenerative)
      (parent info)
      (sealed #t)
      (fields
        src
        sexpr
        cnfv*
        nfv*
        nfv**
        (mutable weight)
        (mutable call-live*)
        (mutable frame-words)
        (mutable local-save*))
      (protocol
        (lambda (pargs->new)
          (lambda (src sexpr cnfv* nfv* nfv**)
            ((pargs->new) src sexpr cnfv* nfv* nfv** 0 #f #f #f)))))

    (define-record-type info-kill* (nongenerative)
      (parent info)
      (fields kill*))

    (define-record-type info-kill*-live* (nongenerative)
      (parent info-kill*)
      (fields live*)
      (protocol
        (lambda (new)
          (case-lambda
            [(kill* live*)
              ((new kill*) live*)]
            [(kill*)
              ((new kill*) (reg-list))]))))

    (define-record-type info-asmlib (nongenerative)
      (parent info-kill*-live*)
      (sealed #t)
      (fields libspec save-ra?)
      (protocol
        (lambda (new)
          (case-lambda
            [(kill* libspec save-ra? live*)
             ((new kill* live*) libspec save-ra?)]
            [(kill* libspec save-ra?)
             ((new kill*) libspec save-ra?)]))))

    (module (intrinsic-info-asmlib intrinsic-return-live* intrinsic-entry-live* dorest-intrinsics)
      ; standing on our heads here to avoid referencing registers at
      ; load time...would be cleaner if registers were immutable,
      ; i.e., mutable fields (direct and inherited from var) were kept
      ; in separate tables...but that might add more cost to register
      ; allocation, which is already expensive.
      (define-record-type intrinsic (nongenerative)
        (sealed #t)
        (fields libspec get-kill* get-live* get-rv*))
      (define intrinsic-info-asmlib
        (lambda (intrinsic save-ra?)
          (make-info-asmlib ((intrinsic-get-kill* intrinsic))
            (intrinsic-libspec intrinsic)
            save-ra?
            ((intrinsic-get-live* intrinsic)))))
      (define intrinsic-return-live*
        ; used a handful of times, just while compiling library.ss...don't bother optimizing
        (lambda (intrinsic)
          (fold-left (lambda (live* kill) (remq kill live*))
            (vector->list regvec) ((intrinsic-get-kill* intrinsic)))))
      (define intrinsic-entry-live*
        ; used a handful of times, just while compiling library.ss...don't bother optimizing
        (lambda (intrinsic) ; return-live* - rv + live*
          (fold-left (lambda (live* live) (if (memq live live*) live* (cons live live*)))
            (fold-left (lambda (live* rv) (remq rv live*))
              (intrinsic-return-live* intrinsic)
              ((intrinsic-get-rv* intrinsic)))
            ((intrinsic-get-live* intrinsic)))))
      (define-syntax declare-intrinsic
        (syntax-rules (unquote)
          [(_ name entry-name (kill ...) (live ...) (rv ...))
           (begin
             (define name
               (make-intrinsic
                 (lookup-libspec entry-name)
                 (lambda () (reg-list kill ...))
                 (lambda () (reg-list live ...))
                 (lambda () (reg-list rv ...))))
             (export name))]))
      ; must include in kill ... any register explicitly assigned by the intrinsic
      ; plus additional registers as needed to avoid spilled unspillables.  the
      ; list could be machine-dependent but at this point it doesn't matter.
      (declare-intrinsic dofargint32 dofargint32 (%ts %td %xp) (%ac0) (%ac0))
      (constant-case ptr-bits
        [(32) (declare-intrinsic dofargint64 dofargint64 (%ts %td %xp) (%ac0) (%ac0 %ac1))]
        [(64) (declare-intrinsic dofargint64 dofargint64 (%ts %td %xp) (%ac0) (%ac0))])
      (declare-intrinsic dofretint32 dofretint32 (%ts %td %xp) (%ac0) (%ac0))
      (constant-case ptr-bits
        [(32) (declare-intrinsic dofretint64 dofretint64 (%ts %td %xp) (%ac0 %ac1) (%ac0))]
        [(64) (declare-intrinsic dofretint64 dofretint64 (%ts %td %xp) (%ac0) (%ac0))])
      (declare-intrinsic dofretuns32 dofretuns32 (%ts %td %xp) (%ac0) (%ac0))
      (constant-case ptr-bits
        [(32) (declare-intrinsic dofretuns64 dofretuns64 (%ts %td %xp) (%ac0 %ac1) (%ac0))]
        [(64) (declare-intrinsic dofretuns64 dofretuns64 (%ts %td %xp) (%ac0) (%ac0))])
      (declare-intrinsic dofretu8* dofretu8* (%ac0 %ts %td %cp %ac1) (%ac0) (%xp))
      (declare-intrinsic dofretu16* dofretu16* (%ac0 %ts %td %cp %ac1) (%ac0) (%xp))
      (declare-intrinsic dofretu32* dofretu32* (%ac0 %ts %td %cp %ac1) (%ac0) (%xp))
      (declare-intrinsic get-room get-room () (%xp) (%xp))
      (declare-intrinsic scan-remembered-set scan-remembered-set () () ())
      (declare-intrinsic dooverflow dooverflow () () ())
      (declare-intrinsic dooverflood dooverflood () (%xp) ())
      ; a dorest routine takes all of the register and frame arguments from the rest
      ; argument forward and also modifies the rest argument.  for the rest argument,
      ; this is a wash (it's live both before and after).  the others should also be
      ; listed as live.  it's inconvenient and currently unnecessary to do so.
      ; (actually currently impossible to list the infinite set of frame arguments)
      (define-syntax dorest-intrinsic-max (identifier-syntax 5))
      (export dorest-intrinsic-max)
      (define (list-xtail ls n)
        (if (or (null? ls) (fx= n 0))
            ls
            (list-xtail (cdr ls) (fx1- n))))
      (define dorest-intrinsics
        (let ()
          (define-syntax dorests
            (lambda (x)
              #`(vector #,@
                  (let f ([i 0])
                    (if (fx> i dorest-intrinsic-max)
                        '()
                        (cons #`(make-intrinsic
                                  (lookup-libspec #,(construct-name #'k "dorest" i))
                                  (lambda () (reg-list %ac0 %xp %ts %td))
                                  (lambda () (reg-cons* %ac0 (list-xtail arg-registers #,i)))
                                  (lambda () (let ([ls (list-xtail arg-registers #,i)]) (if (null? ls) '() (list (car ls))))))
                          (f (fx+ i 1))))))))
          dorests)))

    (define-record-type info-alloc (nongenerative)
      (parent info)
      (sealed #t)
      (fields tag save-flrv? save-ra?))

    (define-record-type info-foreign (nongenerative)
      (parent info)
      (sealed #t)
      (fields conv* arg-type* result-type (mutable name))
      (protocol
        (lambda (pargs->new)
          (lambda (conv* arg-type* result-type)
            ((pargs->new) conv* arg-type* result-type #f)))))

    (define-record-type info-literal (nongenerative)
      (parent info)
      (sealed #t)
      (fields indirect? type addr offset))

    (define-record-type info-lea (nongenerative)
      (parent info)
      (sealed #t)
      (fields offset))

    (define-record-type info-load (nongenerative)
      (parent info)
      (sealed #t)
      (fields type swapped?))

    (define-record-type info-loadfl (nongenerative)
      (parent info)
      (sealed #t)
      (fields flreg))

    (define-record-type info-condition-code (nongenerative)
      (parent info)
      (sealed #t)
      (fields type reversed? invertible?))

    (define-record-type info-c-simple-call (nongenerative)
      (parent info-kill*-live*)
      (sealed #t)
      (fields save-ra? entry)
      (protocol
       (lambda (new)
         (case-lambda
          [(save-ra? entry) ((new '() '()) save-ra? entry)]
          [(live* save-ra? entry) ((new '() live*) save-ra? entry)]))))

    (define-record-type info-c-return (nongenerative)
      (parent info)
      (sealed #t)
      (fields offset))

    (module ()
      (record-writer (record-type-descriptor info-load)
        (lambda (x p wr)
          (fprintf p "#<info-load ~s>" (info-load-type x))))
      (record-writer (record-type-descriptor info-lambda)
        (lambda (x p wr)
          (fprintf p "#<info-lambda ~s ~s ~s ~s ~s>"
            (info-lambda-libspec x) (info-lambda-interface* x) (info-lambda-name x)
            (info-lambda-well-known? x)
            (info-lambda-fv* x))))
      (record-writer (record-type-descriptor info-foreign)
        (lambda (x p wr)
          (fprintf p "#<info-foreign~@[ ~a~]>" (info-foreign-name x))))
      (record-writer (record-type-descriptor info-literal)
        (lambda (x p wr)
          (fprintf p "#<literal ~s>" (info-literal-addr x))))
    )

    (define-pass cpnanopass : Lsrc (ir) -> L1 ()
      (definitions
        (define-syntax with-uvars
          (syntax-rules ()
            [(_ (x* id*) b1 b2 ...)
             (and (identifier? #'x*) (identifier? #'id*))
             (let ([uvar* (map prelex->uvar id*)] [name* (map prelex-name id*)])
               (dynamic-wind
                 (lambda () (for-each prelex-name-set! id* uvar*))
                 (lambda () (let ([x* uvar*]) b1 b2 ...))
                 (lambda () (for-each prelex-name-set! id* name*))))]))
        (define extract-uvar
          (lambda (id)
            (let ([x (prelex-name id)])
              (unless (uvar? x)
                (sorry! 'extract-uvar "~s is not a uvar" x))
              x))))
      (CaseLambdaExpr : Expr (ir x) -> CaseLambdaExpr ()
        [(case-lambda ,preinfo (clause (,x** ...) ,interface* ,body*) ...)
         (let ([info (make-info-lambda (preinfo-src preinfo) (preinfo-sexpr preinfo) (preinfo-lambda-libspec preinfo) interface*
                       (preinfo-lambda-name preinfo) (preinfo-lambda-flags preinfo))])
           (when x (uvar-info-lambda-set! x info))
           `(case-lambda ,info
              ,(map (lambda (x* interface body)
                      (with-uvars (uvar* x*)
                        (in-context CaseLambdaClause
                          `(clause (,uvar* ...) ,interface ,(Expr body)))))
                 x** interface* body*) ...))]
        [(case-lambda ,preinfo ,cl* ...)
         (sorry! who "found unreachable clause" ir)])
      (Expr : Expr (ir) -> Expr ()
        [(ref ,maybe-src ,x) (extract-uvar x)]
        [(set! ,maybe-src ,x ,[e]) `(set! ,(extract-uvar x) ,e)]
        [(case-lambda ,preinfo ,cl* ...) (CaseLambdaExpr ir #f)]
        [(letrec ([,x* ,e*] ...) ,body)
         (with-uvars (uvar* x*)
           (let ([e* (map CaseLambdaExpr e* uvar*)])
             `(letrec ([,uvar* ,e*] ...) ,(Expr body))))]
        [(call ,preinfo ,e ,[e*] ...)
         `(call ,(make-info-call (preinfo-src preinfo) (preinfo-sexpr preinfo) (fx< (optimize-level) 3) #f #f)
            ,(Expr e) ,e* ...)]
        [(foreign (,conv* ...) ,name ,[e] (,arg-type* ...) ,result-type)
         (let ([info (make-info-foreign conv* arg-type* result-type)])
           (info-foreign-name-set! info name)
           `(foreign ,info ,e))]
        [(fcallable (,conv* ...) ,[e] (,arg-type* ...) ,result-type)
         `(fcallable ,(make-info-foreign conv* arg-type* result-type) ,e)])
      (CaseLambdaExpr ir #f))

    (define find-matching-clause
      (lambda (len x** interface* body* kfixed kvariable kfail)
        (let f ([x** x**] [interface* interface*] [body* body*])
          (if (null? interface*)
              (kfail)
              (let ([interface (car interface*)])
                (if (fx< interface 0)
                    (let ([nfixed (fxlognot interface)])
                      (if (fx>= len nfixed)
                          (kvariable nfixed (car x**) (car body*))
                          (f (cdr x**) (cdr interface*) (cdr body*))))
                    (if (fx= interface len)
                        (kfixed (car x**) (car body*))
                        (f (cdr x**) (cdr interface*) (cdr body*)))))))))

    (define-syntax define-$type-check
      (lambda (x)
        (syntax-case x ()
          [(k L) (with-implicit (k $type-check)
                   #'(define $type-check
                       (lambda (mask type expr)
                         (with-output-language L
                           (cond
                             [(fx= type 0) (%inline log!test ,expr (immediate ,mask))]
                             [(= mask (constant byte-constant-mask)) (%inline eq? ,expr (immediate ,type))]
                             [else (%inline type-check? ,expr (immediate ,mask) (immediate ,type))])))))])))

    (define-syntax %type-check
      (lambda (x)
        (syntax-case x ()
          [(k mask type expr)
           (with-implicit (k $type-check quasiquote)
             #'($type-check (constant mask) (constant type) `expr))])))

    (define-syntax %typed-object-check ; NB: caller must bind e
      (lambda (x)
        (syntax-case x ()
          [(k mask type expr)
           (with-implicit (k quasiquote %type-check %constant %mref)
             #'`(if ,(%type-check mask-typed-object type-typed-object expr)
                    ,(%type-check mask type
                       ,(%mref expr ,(constant typed-object-type-disp)))
                    ,(%constant sfalse)))])))

    (define-syntax %seq
      (lambda (x)
        (syntax-case x ()
          [(k e1 ... e2)
           (with-implicit (k quasiquote)
             #``#,(fold-right (lambda (x body) #`(seq #,x #,body))
                    #'e2 #'(e1 ...)))])))

    (define-syntax %mref
      (lambda (x)
        (syntax-case x ()
          [(k e0 e1 imm)
           (with-implicit (k quasiquote)
             #'`(mref e0 e1 imm))]
          [(k e0 imm)
           (with-implicit (k quasiquote)
             #'`(mref e0 ,%zero imm))])))

    (define-syntax %inline
      (lambda (x)
        (syntax-case x ()
          [(k name e ...)
           (with-implicit (k quasiquote)
             #'`(inline ,null-info ,(%primitive name) e ...))])))

    (define-syntax %lea
      (lambda (x)
        (syntax-case x ()
          [(k base offset)
           (with-implicit (k quasiquote)
             #'`(inline ,(make-info-lea offset) ,%lea1 base))]
          [(k base index offset)
           (with-implicit (k quasiquote)
             #'`(inline ,(make-info-lea offset) ,%lea2 base index))])))

    (define-syntax %constant
      (lambda (x)
        (syntax-case x ()
          [(k x)
           (with-implicit (k quasiquote)
             #'`(immediate ,(constant x)))])))

    (define-syntax %tc-ref
      (lambda (x)
        (define-who field-type
          (lambda (struct field)
            (cond
              [(assq field (getprop struct '*fields* '())) =>
               (lambda (a)
                 (apply
                   (lambda (field type disp len) type)
                   a))]
              [else ($oops who "undefined field ~s-~s" struct field)])))
        (syntax-case x ()
          [(k field) #'(k ,%tc field)]
          [(k e-tc field)
           (if (memq (field-type 'tc (datum field)) '(ptr void* uptr iptr))
               (with-implicit (k %mref)
                 #`(%mref e-tc
                     #,(lookup-constant
                         (string->symbol
                           (format "tc-~a-disp" (datum field))))))
               (syntax-error x "non-ptr-size tc field"))])))

    (define-syntax %constant-alloc
      (lambda (x)
        (syntax-case x ()
          [(k tag size) #'(k tag size #f #f)]
          [(k tag size save-flrv?) #'(k tag size save-flrv? #f)]
          [(k tag size save-flrv? save-asm-ra?)
           (with-implicit (k quasiquote)
             #'`(alloc
                  ,(make-info-alloc (constant tag) save-flrv? save-asm-ra?)
                  (immediate ,(c-alloc-align size))))])))

    (define-pass np-recognize-let : L1 (ir) -> L2 ()
      (definitions
        (define seqs-and-profiles?
          (lambda (e)
            (nanopass-case (L1 Expr) e
              [(profile ,src) #t]
              [(seq ,e1 ,e2) (and (seqs-and-profiles? e1) (seqs-and-profiles? e2))]
              [else #f])))
        (define Profile
          (lambda (e)
            (let f ([e e] [profile* '()])
              (nanopass-case (L1 Expr) e
                [(seq ,e1 ,e2)
                 (guard (seqs-and-profiles? e1))
                 (f e2 (cons e1 profile*))]
                [else (values e profile*)]))))
        (define build-seq (lambda (e1 e2) (with-output-language (L2 Expr) `(seq ,(Expr e1) ,e2))))
        (define build-seq* (lambda (e* e) (fold-right build-seq e e*))))
      (Expr : Expr (ir) -> Expr ()
        [(call ,info1 ,[Profile : e profile1*] ,[e*] ...)
         (nanopass-case (L1 Expr) e
           [(case-lambda ,info2 (clause (,x* ...) ,interface ,[Expr : body]))
            (guard (fx= (length e*) interface))
            (build-seq* profile1* `(let ([,x* ,e*] ...) ,body))]
           [(letrec ([,x1 ,[Expr : le*]]) ,[Profile : body profile2*])
            ; can't use a guard, since body isn't bound in guard.
            (if (eq? body x1)
                (build-seq* profile1*
                  (build-seq* profile2*
                    `(letrec ([,x1 ,le*]) (call ,info1 ,x1 ,e* ...))))
                `(call ,info1 ,(build-seq* profile1* (Expr e)) ,e* ...))]
           [else
            `(call ,info1 ,(build-seq* profile1* (Expr e)) ,e* ...)])]))

    (define-pass np-discover-names : L2 (ir) -> L3 ()
      (definitions
        (define ->name
          (lambda (x)
            (cond
              [(uvar? x) (->name (uvar-name x))]
              [(string? x) x]
              [(symbol? x)
               (let ([name ($symbol-name x)])
                 (if (pair? name) (cdr name) name))]
              [(eq? #f x) #f]
              [else (error 'np-discover-names "x is not a name" x)]))))
      (Expr : Expr (ir name moi) -> Expr ()
        [(letrec ([,x* ,le*] ...) ,[body])
         (let ([le* (map (lambda (le x) (CaseLambdaExpr le (->name x) moi)) le* x*)])
           `(letrec ([,x* ,le*] ...) ,body))]
        [(let ([,x* ,e*] ...) ,[body])
         (let ([e* (map (lambda (e x) (Expr e (->name x) moi)) e* x*)])
           `(let ([,x* ,e*] ...) ,body))]
        ; handle top-level set! (i.e. $set-top-level-value)
        [(call ,info ,pr (quote ,d) ,e0)
         (guard (and (eq? (primref-name pr) '$set-top-level-value!) (symbol? d)))
         (let ([e0 (Expr e0 (->name d) moi)])
           `(call ,info ,pr (quote ,d) ,e0))]
        [(call ,info ,[e0 #f moi -> e0] ,[e1* #f moi -> e1*] ...)
         `(call ,info ,e0 ,e1* ...)]
        [(if ,[e0 #f moi -> e0] ,[e1] ,[e2])
         `(if ,e0 ,e1 ,e2)]
        [(seq ,[e0 #f moi -> e0] ,[e1])
         `(seq ,e0 ,e1)]
        [(foreign ,info ,[e #f moi -> e])
         (when name (info-foreign-name-set! info name))
         `(foreign ,info ,e)]
        [(fcallable ,info ,[e #f moi -> e])
         (info-foreign-name-set! info name)
         `(fcallable ,info ,e)]
        [(set! ,x ,e0)
         (let ([e0 (Expr e0 (->name x) moi)]) `(set! ,x ,e0))]
        [(moi) `(quote ,moi)])
      (CaseLambdaExpr : CaseLambdaExpr (ir [name #f] [moi #f]) -> CaseLambdaExpr ()
        [(case-lambda ,info ,[cl #f name -> cl] ...)
         (unless (info-lambda-name info) (info-lambda-name-set! info name))
         `(case-lambda ,info ,cl ...)])
      (CaseLambdaClause : CaseLambdaClause (ir name moi) -> CaseLambdaClause ()))

    (define-pass np-convert-assignments : L3 (ir) -> L4 ()
      (definitions
        (define-syntax %primcall
          (lambda (x)
            (syntax-case x ()
              [(k src sexpr prim arg ...)
               (identifier? #'prim)
               (with-implicit (k quasiquote)
                 #``(call ,(make-info-call src sexpr #f #f #f)
                      ,(lookup-primref 3 'prim)
                      arg ...))])))
        (define unbound-object ($unbound-object))
        (define partition-assigned
          (lambda (x*)
            (if (null? x*)
                (values '() '() '())
                (let ([x (car x*)] [x* (cdr x*)])
                  (let-values ([(x* t* a*) (partition-assigned x*)])
                    (if (uvar-assigned? x)
                        (let ([t (make-tmp 't)])
                          (uvar-assigned! x #f)
                          (values (cons t x*) (cons t t*) (cons x a*)))
                        (values (cons x x*) t* a*)))))))
        (define handle-assigned
          (lambda (x* body k)
            (let-values ([(x* t* a*) (partition-assigned x*)])
              (k x* (if (null? a*)
                        body
                        (with-output-language (L4 Expr)
                          `(let ([,a* ,(map (lambda (t) (%primcall #f #f cons ,t (quote ,unbound-object))) t*)] ...)
                             ,body))))))))
      (Expr : Expr (ir) -> Expr ()
        [,x (if (uvar-assigned? x) (%primcall #f #f car ,x) x)]
        [(set! ,x ,[e]) (%primcall #f #f set-car! ,x ,e)]
        [(let ([,x* ,[e*]] ...) ,[body])
         (handle-assigned x* body
           (lambda (x* body)
             `(let ([,x* ,e*] ...) ,body)))])
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) ,interface ,[body])
         (handle-assigned x* body
           (lambda (x* body)
             `(clause (,x* ...) ,interface ,body)))]))

    ; for use only after mdcl field has been added to the call syntax
    (define-syntax %primcall
      (lambda (x)
        (syntax-case x ()
          [(k src sexpr prim arg ...)
           (identifier? #'prim)
           (with-implicit (k quasiquote)
             #``(call ,(make-info-call src sexpr #f #f #f) #f
                  ,(lookup-primref 3 'prim)
                  arg ...))])))

    (define-pass np-sanitize-bindings : L4 (ir) -> L4 ()
      ; must come before suppress-procedure-checks and recognize-mrvs
      ; since it sets up uvar-info-lambda, but after convert-assignments
      (definitions
        (define maybe-build-let
          (lambda (x* e* body)
            (if (null? x*)
                body
                (with-output-language (L4 Expr)
                  `(let ([,x* ,e*] ...) ,body)))))
        (define maybe-build-letrec
          (lambda (x* e* body)
            (if (null? x*)
                body
                (with-output-language (L4 Expr)
                  `(letrec ([,x* ,e*] ...) ,body))))))
      (Expr : Expr (ir) -> Expr ()
        [(let ([,x* ,[e*]] ...) ,[body])
         (with-values
           (let f ([x* x*] [e* e*])
             (if (null? x*)
                 (values '() '() '() '())
                 (let-values ([(ex* ee* lx* le*) (f (cdr x*) (cdr e*))])
                   (nanopass-case (L4 Expr) (car e*)
                     [(case-lambda ,info ,cl ...)
                      (uvar-info-lambda-set! (car x*) info)
                      (values ex* ee* (cons (car x*) lx*) (cons (car e*) le*))]
                     [else (values (cons (car x*) ex*) (cons (car e*) ee*) lx* le*)]))))
           (lambda (ex* ee* lx* le*)
             (maybe-build-let ex* ee*
               (maybe-build-letrec lx* le*
                 body))))]))

    (define-pass np-suppress-procedure-checks : L4 (ir) -> L4 ()
      ; N.B. check must be done after e and e* have been evaluated, so we attach
      ; a flag to the call syntax rather than introducing explicit checks.
      ; if we could introduce explicit checks instead, we could avoid doing
      ; so along some branches of an if in call context, even if others
      ; need the check.  c'est la vie.
      (Proc : Expr (ir) -> * (#f)
        [,x (uvar-info-lambda x)]
        [(quote ,d) (procedure? d)]
        [,pr #t]
        [(seq ,[] ,[* suppress?]) suppress?]
        [(if ,[] ,[* suppress1?] ,[* suppress2?]) (and suppress1? suppress2?)]
        [(letrec ([,x* ,[]] ...) ,[* suppress?]) suppress?]
        [(let ([,x* ,[]] ...) ,[* suppress?]) suppress?]
        [(case-lambda ,info ,[] ...) #t]
        [else #f])
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> * ()
        [(case-lambda ,info ,[] ...) (values)])
      (CaseLambdaClause : CaseLambdaClause (ir) -> * ()
        [(clause (,x* ...) ,interface ,[]) (values)])
      ; NB: explicitly handling every form because the nanopass infrastructure can't autofill when the output is *
      (Expr : Expr (ir) -> * ()
        [,x (values)]
        [(quote ,d) (values)]
        [(case-lambda ,info ,[] ...) (values)]
        [(call ,info0
           (call ,info1 ,pr (quote ,d))
           ,[] ...)
         (guard (and (eq? (primref-name pr) '$top-level-value) (symbol? d)))
         (info-call-check?-set! info0 #f)
         (info-call-check?-set! info1 #f)
         (values)]
        [(call ,info ,[* suppress?] ,[] ...)
         (when suppress? (info-call-check?-set! info #f))
         (values)]
        [(if ,[] ,[] ,[]) (values)]
        [(seq ,[] ,[]) (values)]
        [,pr (values)]
        [(let ([,x ,[]] ...) ,[]) (values)]
        [(letrec ([,x ,[]] ...) ,[]) (values)]
        [(foreign ,info ,[]) (values)]
        [(fcallable ,info ,[]) (values)]
        [(profile ,src) (values)]
        [(pariah) (values)]
        [else (sorry! who "unhandled expression ~s" ir)])
      (begin (CaseLambdaExpr ir) ir))

    (define-pass np-recognize-mrvs : L4 (ir) -> L4.5 ()
      (definitions
        (define insert-procedure-check
          (lambda (check? tmp e)
            (with-output-language (L4.5 Expr)
              (if check?
                  `(seq
                     (if ,(%primcall #f #f procedure? ,tmp)
                         (quote ,(void))
                         ,(%primcall #f #f $oops (quote #f) (quote "attempt to apply non-procedure ~s") ,tmp))
                     ,e)
                  e)))))
      (Expr : Expr (ir) -> Expr ()
        [(call ,info ,pr ,e1 ,e2)
         (guard (eq? (primref-name pr) 'call-with-values))
         (let ([check? (not (all-set? (prim-mask unsafe) (primref-flags pr)))])
           (Producer e1 check? (info-call-src info) (info-call-sexpr info)
             (lambda (e1 src sexpr)
               (Consumer e2 e1 check? src sexpr))))]
        [(call ,info ,[e] ,[e*] ...) `(call ,info #f ,e ,e* ...)])
      (Producer : Expr (ir check? src sexpr k) -> Expr ()
        [,x (k `(call ,(make-info-call src sexpr check? #f #f) #f ,x) src sexpr)]
        [(case-lambda ,info (clause (,x** ...) ,interface* ,body*) ...)
         (find-matching-clause 0 x** interface* body*
           (lambda (x* body) (k (Expr body) src sexpr))
           (lambda (nfixed x* body) `(let ([,(car x*) (quote ())]) ,(k (Expr body) src sexpr)))
           (lambda ()
             (let ([tmp (make-tmp 'tp)])
               (uvar-info-lambda-set! tmp info)
               `(letrec ([,tmp ,(Expr ir)])
                  ,(k tmp src sexpr)))))]
        [(seq ,[Expr : e1] ,[Producer : e2]) `(seq ,e1 ,e2)]
        [(let ([,x* ,[Expr : e*]] ...) ,[Producer : e]) `(let ([,x* ,e*] ...) ,e)]
        [(letrec ([,x* ,[le*]] ...) ,[Producer : e]) `(letrec ([,x* ,le*] ...) ,e)]
        [,pr (k `(call ,(make-info-call src sexpr #f #f #f) #f ,pr) src sexpr)]
        [else (let ([tmp (make-tmp 'tp)])
                ; force last part of producer to be evaluated before consumer, to
                ; avoid interleaved evaluation of producer and consumer
                `(let ([,tmp ,(Expr ir)])
                   ,(k `(call ,(make-info-call #f #f check? #f #f) #f ,tmp) src sexpr)))])
      (Consumer : Expr (ir producer-or check? src sexpr) -> Expr ()
        ; generate same code for single-value let-values as for let
        [(case-lambda ,info (clause (,x) ,interface ,[Expr : body]))
         (guard (= interface 1))
         `(let ([,x ,producer-or]) ,body)]
        [(case-lambda ,info (clause (,x** ...) ,interface* ,[Expr : body*]) ...)
         `(mvlet ,producer-or ((,x** ...) ,interface* ,body*) ...)]
        [,x (cond
              [(uvar-info-lambda x) =>
               (lambda (info)
                 (define make-tmps
                   (lambda (n)
                     (do ([n (if (fx< n 0) (fx- n) n) (fx- n 1)]
                          [tmp* '() (cons (make-tmp 't) tmp*)])
                         ((fx= n 0) tmp*))))
                 (let ([interface* (info-lambda-interface* info)])
                   (let ([info* (map (lambda (dcl) (make-info-call src sexpr #f #f #f)) (info-lambda-dcl* info))]
                         [x* (make-list (length interface*) x)]
                         [x** (map make-tmps interface*)])
                     `(mvlet ,producer-or
                        ((,x** ...) ,interface* (call ,info* ,(info-lambda-dcl* info) ,x* ,x** ...))
                        ...))))]
              [else (insert-procedure-check check? x `(mvcall ,(make-info-call src sexpr #f #f #f) ,producer-or ,x))])]
        [(seq ,[Expr : e1] ,[Consumer : e2]) `(seq ,e1 ,e2)]
        [(let ([,x* ,[Expr : e*]] ...) ,[Consumer : e]) `(let ([,x* ,e*] ...) ,e)]
        [(letrec ([,x* ,[le*]] ...) ,[Consumer : e]) `(letrec ([,x* ,le*] ...) ,e)]
        [,pr `(mvcall ,(make-info-call src sexpr #f #f #f) ,producer-or ,pr)]
        [(quote ,d) (guard (procedure? d)) `(mvcall ,(make-info-call src sexpr #f #f #f) ,producer-or (quote ,d))]
        [else (let ([tmp (make-tmp 'tc)])
                ; force consumer expression to be evaluated before producer body
                ; this includes references to top-level variables: since they can
                ; be altered by the producer, we can use a pvalue call
                `(let ([,tmp ,(Expr ir)])
                   ,(insert-procedure-check check? tmp
                      `(mvcall ,(make-info-call src sexpr #f #f #f) ,producer-or ,tmp))))]))

    (define-pass np-expand-foreign : L4.5 (ir) -> L4.75 ()
      (Expr : Expr (ir) -> Expr ()
        [(foreign ,info ,[e])
         (let ([iface (length (info-foreign-arg-type* info))]
               [t (make-tmp 'tentry 'uptr)]
               [t* (map (lambda (x) (make-tmp 't)) (info-foreign-arg-type* info))])
           (let ([lambda-info (make-info-lambda #f #f #f (list iface) (info-foreign-name info))])
             `(let ([,t ,e])
                (case-lambda ,lambda-info
                  (clause (,t* ...) ,iface
                    (foreign-call ,info ,t ,t* ...))))))]
        [(fcallable ,info ,[e])
         (%primcall #f #f $instantiate-code-object
           (fcallable ,info)
           (quote 0) ; hard-wiring "cookie" to 0
           ,e)]))

    (define-pass np-recognize-loops : L4.75 (ir) -> L4.875 ()
      ; TODO: also recognize andmap/for-all, ormap/exists, for-each
      ;       and remove inline handlers
      (definitions
        (define make-assigned-tmp
          (lambda (x)
            (let ([t (make-tmp 'tloop)])
              (uvar-assigned! t #t)
              t))))
      (Expr : Expr (ir [tail* '()]) -> Expr ()
        [,x (uvar-referenced! x #t) (uvar-loop! x #f) x]
        [(letrec ([,x1 (case-lambda ,info1
                         (clause (,x* ...) ,interface
                           ,body))])
           (call ,info2 ,mdcl ,x2 ,e* ...))
         (guard (eq? x2 x1) (eq? (length e*) interface))
         (uvar-referenced! x1 #f)
         (uvar-loop! x1 #t)
         (let ([tref?* (map uvar-referenced? tail*)])
           (for-each (lambda (x) (uvar-referenced! x #f)) tail*)
           (let ([e* (map (lambda (e) (Expr e '())) e*)]
                 [body (Expr body (cons x1 tail*))])
             (let ([body-tref?* (map uvar-referenced? tail*)])
               (for-each (lambda (x tref?) (when tref? (uvar-referenced! x #t))) tail* tref?*)
               (if (uvar-referenced? x1)
                   (if (uvar-loop? x1)
                       (let ([t* (map make-assigned-tmp x*)])
                         `(let ([,t* ,e*] ...)
                            (loop ,x1 (,t* ...)
                              (let ([,x* ,t*] ...)
                                ,body))))
                       (begin
                         (for-each (lambda (x body-tref?)
                                     (when body-tref? (uvar-loop! x #f)))
                           tail* body-tref?*)
                         `(letrec ([,x1 (case-lambda ,info1
                                          (clause (,x* ...) ,interface
                                            ,body))])
                            (call ,info2 ,mdcl ,x2 ,e* ...))))
                   `(let ([,x* ,e*] ...) ,body)))))]
        [(letrec ([,x* ,[le*]] ...) ,[body])
         `(letrec ([,x* ,le*] ...) ,body)]
        [(call ,info ,mdcl ,x ,[e* '() -> e*] ...)
         (guard (memq x tail*))
         (uvar-referenced! x #t)
         (let ([interface* (info-lambda-interface* (uvar-info-lambda x))])
           (unless (and (fx= (length interface*) 1) (fx= (length e*) (car interface*)))
             (uvar-loop! x #f)))
         `(call ,info ,mdcl ,x ,e* ...)]
        [(call ,info ,mdcl ,[e '() -> e] ,[e* '() -> e*] ...)
         `(call ,info ,mdcl ,e ,e* ...)]
        [(foreign-call ,info ,[e '() -> e] ,[e* '() -> e*] ...)
         `(foreign-call ,info ,e ,e* ...)]
        [(fcallable ,info) `(fcallable ,info)]
        [(label ,l ,[body]) `(label ,l ,body)]
        [(mvlet ,[e '() -> e] ((,x** ...) ,interface* ,[body*]) ...)
         `(mvlet ,e ((,x** ...) ,interface* ,body*) ...)]
        [(mvcall ,info ,[e1 '() -> e1] ,[e2 '() -> e2])
         `(mvcall ,info ,e1 ,e2)]
        [(let ([,x ,[e* '() -> e*]] ...) ,[body])
         `(let ([,x ,e*] ...) ,body)]
        [(case-lambda ,info ,[cl] ...) `(case-lambda ,info ,cl ...)]
        [(quote ,d) `(quote ,d)]
        [(if ,[e0 '() -> e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
        [(seq ,[e0 '() -> e0] ,[e1]) `(seq ,e0 ,e1)]
        [(profile ,src) `(profile ,src)]
        [(pariah) `(pariah)]
        [,pr pr]
        [else ($oops who "unexpected Expr ~s" ir)]))

    (define-pass np-name-anonymous-lambda : L4.875 (ir) -> L5 ()
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ())
      (Expr : Expr (ir) -> Expr ()
        [(case-lambda ,info ,[cl] ...)
         (let ([anon (make-tmp (or (let ([name (info-lambda-name info)])
                                     (and name (string->symbol name)))
                                   'anon))])
           (uvar-info-lambda-set! anon info)
           `(letrec ([,anon (case-lambda ,info ,cl ...)])
              ,anon))])
      (nanopass-case (L4.875 CaseLambdaExpr) ir
        [(case-lambda ,info ,[CaseLambdaClause : cl] ...) `(case-lambda ,info ,cl ...)]))

    (define-pass np-convert-closures : L5 (x) -> L6 ()
      (definitions
        (define-record-type clinfo
          (nongenerative)
          (sealed #t)
          (fields lid (mutable mask) (mutable fv*))
          (protocol (lambda (n) (lambda (index) (n index 0 '())))))
        (module (with-offsets)
          (define set-offsets!
            (lambda (x* index)
              (do ([x* x* (cdr x*)] [index index (fx+ index 1)])
                ((null? x*) index)
                (var-index-set! (car x*) index))))
          (define-syntax with-offsets
            (syntax-rules ()
              [(_ index ?x* ?e1 ?e2 ...)
               (identifier? #'index)
               (let ([x* ?x*])
                 (let ([index (set-offsets! x* index)])
                   (let ([v (begin ?e1 ?e2 ...)])
                     (for-each (lambda (x) (var-index-set! x #f)) x*)
                     v)))])))
        (define record-ref!
          (lambda (x clinfo)
            (let ([index (var-index x)])
              (unless index (sorry! who "variable ~a lost its binding" x))
              (when (fx< index (clinfo-lid clinfo))
                (let ([mask (clinfo-mask clinfo)])
                  (unless (bitwise-bit-set? mask index)
                    (clinfo-mask-set! clinfo (bitwise-copy-bit mask index 1))
                    (clinfo-fv*-set! clinfo (cons x (clinfo-fv* clinfo))))))))))
      (Expr : Expr (ir index clinfo) -> Expr ()
        [,x (record-ref! x clinfo) x]
        [(letrec ([,x* ,le*] ...) ,body)
         (with-offsets index x*
           (let loop ([le* le*] [rle* '()] [rfv** '()])
             (if (null? le*)
                 `(closures ([,x* (,(reverse rfv**) ...) ,(reverse rle*)] ...)
                    ,(Expr body index clinfo))
                 (let-values ([(le fv*) (CaseLambdaExpr (car le*) index clinfo)])
                   (loop (cdr le*) (cons le rle*) (cons fv* rfv**))))))]
        [(let ([,x* ,[e*]] ...) ,body)
         (with-offsets index x*
           `(let ([,x* ,e*] ...) ,(Expr body index clinfo)))]
        [(mvlet ,[e] ((,x** ...) ,interface* ,body*) ...)
         `(mvlet ,e
            ((,x** ...)
             ,interface*
             ,(let f ([x** x**] [body* body*])
                (if (null? x**)
                    '()
                    (cons
                      (with-offsets index (car x**)
                        (Expr (car body*) index clinfo))
                      (f (cdr x**) (cdr body*))))))
            ...)]
        [(loop ,x (,x* ...) ,body)
         (with-offsets index (cons x x*)
           `(loop ,x (,x* ...) ,(Expr body index clinfo)))])
      (CaseLambdaExpr : CaseLambdaExpr (ir index outer-clinfo) -> CaseLambdaExpr ()
        [(case-lambda ,info ,cl* ...)
         (let ([clinfo (make-clinfo index)])
           (let ([cl* (map (lambda (cl) (CaseLambdaClause cl index clinfo)) cl*)])
             (let ([fv* (clinfo-fv* clinfo)])
               (for-each (lambda (x) (record-ref! x outer-clinfo)) fv*)
               (values
                 `(case-lambda ,info ,cl* ...)
                 fv*))))])
      (CaseLambdaClause : CaseLambdaClause (ir index parent-clinfo) -> CaseLambdaClause ()
        [(clause (,x* ...) ,interface ,body)
         (let ([clinfo (make-clinfo index)])
           (with-offsets index x*
             (let ([body (Expr body index clinfo)])
               (let ([fv* (clinfo-fv* clinfo)])
                 (for-each (lambda (x) (record-ref! x parent-clinfo)) fv*)
                 `(clause (,x* ...) ,(if (null? fv*) #f (make-cpvar)) ,interface ,body)))))])
      (let-values ([(le fv*) (CaseLambdaExpr x 0 (make-clinfo 0))])
        (unless (null? fv*) (sorry! who "found unbound variables ~s" fv*))
        le))

    (define-pass np-optimize-direct-call : L6 (ir) -> L6 ()
      (definitions
        (define find-matching-clause
          (lambda (len info kfixed kvariable kfail)
            (if info
                (let f ([interface* (info-lambda-interface* info)] [dcl* (info-lambda-dcl* info)])
                  (if (null? interface*)
                      (kfail)
                      (let ([interface (car interface*)])
                        (if (fx< interface 0)
                            (let ([nfixed (fxlognot interface)])
                              (if (fx>= len nfixed)
                                  (kvariable nfixed (car dcl*))
                                  (f (cdr interface*) (cdr dcl*))))
                            (if (fx= interface len)
                                (kfixed (car dcl*))
                                (f (cdr interface*) (cdr dcl*)))))))
                (kfail)))))
      (CaseLambdaExpr1 : CaseLambdaExpr (ir) -> * ()
        [(case-lambda ,info ,cl* ...)
         (info-lambda-well-known?-set! info #t)])
      (CaseLambdaExpr2 : CaseLambdaExpr (ir) -> CaseLambdaExpr ())
      (Expr : Expr (ir) -> Expr ()
        [,x (let ([info (uvar-info-lambda x)])
              (when info (info-lambda-well-known?-set! info #f))
              x)]
        [(closures ([,x* (,x** ...) ,le*] ...) ,body)
         (for-each CaseLambdaExpr1 le*)
         `(closures ([,x* (,x** ...) ,(map CaseLambdaExpr2 le*)] ...) ,(Expr body))]
        [(loop ,x (,x* ...) ,body)
         (uvar-location-set! x 'loop)
         (let ([body (Expr body)])
           (uvar-location-set! x #f)
           `(loop ,x (,x* ...) ,body))]
        [(call ,info ,mdcl ,x ,[e*] ...)
         (guard (not (eq? (uvar-location x) 'loop)))
         (if mdcl
             (begin
              ; already a direct-call produced, e.g., by recognize-mrvs
               (direct-call-label-referenced-set! mdcl #t)
               `(call ,info ,mdcl ,x ,e* ...))
             (find-matching-clause (length e*) (uvar-info-lambda x)
               (lambda (dcl)
                 (direct-call-label-referenced-set! dcl #t)
                 `(call ,info ,dcl ,x ,e* ...))
               (lambda (nfixed dcl)
                 (direct-call-label-referenced-set! dcl #t)
                 (let ([fixed-e* (list-head e* nfixed)] [rest-e* (list-tail e* nfixed)])
                   (let ([t* (map (lambda (x) (make-tmp 't)) fixed-e*)])
                     ; evaluate fixed-e* first, before the rest list is created.  rest-e* should
                     ; be evaluated before as well assuming later passes handle calls correctly
                     `(let ([,t* ,fixed-e*] ...)
                        (call ,info ,dcl ,x ,t* ...
                          ,(%primcall #f #f list ,rest-e* ...))))))
               (lambda () `(call ,info #f ,(Expr x) ,e* ...))))])
      (CaseLambdaExpr2 ir))

    ; this pass doesn't change the language, but it does add an extragrammatical
    ; restriction: each letrec is now strongly connected
    (define-pass np-identify-scc : L6 (ir) -> L6 ()
      (definitions
        ; returns a list of lists of strongly connected bindings sorted so that
        ; if a binding in some list binding1* binds a variable x that is in the
        ; free list of a binding in some other list binding2*, binding1* comes
        ; before binding2*.
        (define-record-type binding
          (fields le x x* (mutable link*) (mutable root) (mutable done))
          (nongenerative)
          (sealed #t)
          (protocol
            (lambda (new)
              (lambda (le x x*)
                (let ([b (new le x x* '() #f #f)])
                  (uvar-location-set! x b)
                  b)))))
        (define (compute-sccs v*) ; Tarjan's algorithm
          ; adapted from cpletrec
          (define scc* '())
          (define (compute-sccs v)
            (define index 0)
            (define stack '())
            (define (tarjan v)
              (let ([v-index index])
                (binding-root-set! v v-index)
                (set! stack (cons v stack))
                (set! index (fx+ index 1))
                (for-each
                  (lambda (v^)
                    (unless (binding-done v^)
                      (unless (binding-root v^) (tarjan v^))
                      (binding-root-set! v (fxmin (binding-root v) (binding-root v^)))))
                  (binding-link* v))
                (when (fx= (binding-root v) v-index)
                  (set! scc*
                    (cons
                      (let f ([ls stack])
                        (let ([v^ (car ls)])
                          (binding-done-set! v^ #t)
                          (cons v^ (if (eq? v^ v)
                                       (begin (set! stack (cdr ls)) '())
                                       (f (cdr ls))))))
                      scc*)))))
            (tarjan v))
          (for-each (lambda (v) (unless (binding-done v) (compute-sccs v))) v*)
          (reverse scc*)))
      (Expr : Expr (ir) -> Expr ()
        [(closures ([,x* (,x** ...) ,[le*]] ...) ,[body])
         ; create bindings and set each uvar's location to the corresponding binding
         (let ([b* (map make-binding le* x* x**)])
           ; establish links from each binding to the bindings of its free variables
           (for-each
             (lambda (b)
               (binding-link*-set! b
                 (fold-left
                   (lambda (link* x)
                     (let ([loc (uvar-location x)])
                       (if (binding? loc)
                           (cons loc link*)
                           link*)))
                   '() (binding-x* b))))
             b*)
           ; reset uvar locations
           (for-each (lambda (b) (uvar-location-set! (binding-x b) #f)) b*)
           ; sort bindings into strongly connected components, then
           ; create one closure for each not-well-known binding,
           ; and one for all well-known bindings
           (let f ([b** (compute-sccs b*)])
             (if (null? b**)
                 body
                 (let ([b* (car b**)])
                   `(closures ([,(map binding-x b*) (,(map binding-x* b*) ...) ,(map binding-le b*)] ...)
                      ,(f (cdr b**)))))))]))

    (module (np-expand-closures np-expand/optimize-closures)
      (define sort-bindings
        ; sort-bindings uses the otherwise unneeded info-lambda-seqno to put labels
        ; bindings in the same order whether we run np-expand/optimize-closures or
        ; just np-expand-closures, thus reducing code/icache layout differences and,
        ; when there are few other differences, eliminating spurious differences
        ; in run times.  ultimately, we should try laying code objects out
        ; in some order that minimizes cache misses, whether at compile,
        ; load, or collection time.
        (lambda (l* le*)
          (define seqno
            (lambda (p)
              (let ([le (cdr p)])
                (nanopass-case (L7 CaseLambdaExpr) le
                  [(case-lambda ,info ,cl* ...) (info-lambda-seqno info)]
                  [else 0]))))
          (let ([ls (sort (lambda (x y) (< (seqno x) (seqno y))) (map cons l* le*))])
            (values (map car ls) (map cdr ls)))))

      (define-pass np-expand-closures : L6 (ir) -> L7 ()
        (definitions
          (define gl* '())
          (define gle* '())
          (define-record-type closure
            (nongenerative)
            (sealed #t)
            (fields name label (mutable free*)))
          (define-syntax with-uvar-location
            (syntax-rules ()
              [(_ ?uvar ?expr ?e)
               (let ([uvar ?uvar])
                 (let ([old (uvar-location uvar)])
                   (uvar-location-set! uvar ?expr)
                   (let ([v ?e])
                     (uvar-location-set! uvar old)
                     v)))]))
          (with-output-language (L7 Expr)
            (define with-locations
              (lambda (free* mcp body)
                (if mcp
                    (let f ([free* free*] [i (constant closure-data-disp)])
                      (if (null? free*)
                          (Expr body)
                          (with-uvar-location (car free*) (%mref ,mcp ,i)
                            (f (cdr free*) (fx+ i (constant ptr-bytes))))))
                    (Expr body))))
            (module (create-bindings create-inits)
              (define (build-free-ref x) (or (uvar-location x) x))
              (define create-bindings
                (lambda (c* body)
                  (fold-right
                    (lambda (c body)
                      `(let ([,(closure-name c) ,(%constant-alloc type-closure
                                                      (fx* (fx+ (length (closure-free* c)) 1) (constant ptr-bytes)))])
                         ,(%seq
                            (set! ,(%mref ,(closure-name c) ,(constant closure-code-disp))
                              (label-ref ,(closure-label c) ,(constant code-data-disp)))
                            ,body)))
                    body
                    c*)))
              (define create-inits
                (lambda (c* body)
                  (fold-right
                    (lambda (c body)
                      (let f ([x* (closure-free* c)] [i (constant closure-data-disp)])
                        (if (null? x*)
                            body
                            (%seq
                              (set! ,(%mref ,(closure-name c) ,i) ,(build-free-ref (car x*)))
                              ,(f (cdr x*) (fx+ i (constant ptr-bytes)))))))
                    body c*))))))
        (CaseLambdaExpr : CaseLambdaExpr (ir c) -> CaseLambdaExpr ()
          [(case-lambda ,info ,[cl*] ...)
           (info-lambda-fv*-set! info (closure-free* c))
           (info-lambda-closure-rep-set! info 'closure)
           `(case-lambda ,info ,cl* ...)])
        (CaseLambdaClause : CaseLambdaClause (ir c) -> CaseLambdaClause ()
          [(clause (,x* ...) ,mcp ,interface ,body)
           `(clause (,x* ...) ,mcp ,interface
              ,(with-locations (if c (closure-free* c) '()) mcp body))])
        (Expr : Expr (ir) -> Expr ()
          [(closures ([,x* (,x** ...) ,le*] ...) ,body)
           (let* ([l* (map (lambda (x) (make-local-label (uvar-name x))) x*)]
                  [c* (map make-closure x* l* x**)])
             (let ([le* (map CaseLambdaExpr le* c*)] [body (Expr body)])
               (set! gl* (append l* gl*))
               (set! gle* (append le* gle*))
               (create-bindings c* (create-inits c* body))))]
          [,x (or (uvar-location x) x)]
          [(fcallable ,info)
           (let ([label (make-local-label 'fcallable)])
             (set! gl* (cons label gl*))
             (set! gle* (cons (in-context CaseLambdaExpr `(fcallable ,info ,label)) gle*))
             `(label-ref ,label 0))])
        (nanopass-case (L6 CaseLambdaExpr) ir
          [(case-lambda ,info ,[CaseLambdaClause : cl #f -> cl] ...)
           (let ([l (make-local-label 'main)])
             (let-values ([(gl* gle*) (sort-bindings gl* gle*)])
               `(labels ([,gl* ,gle*] ... [,l (case-lambda ,info ,cl ...)]) ,l)))]))

      (define-pass np-expand/optimize-closures : L6 (ir) -> L7 ()
        (definitions
          (module (add-original-closures! add-final-closures!
                    add-ref-counter add-create-and-alloc-counters
                    add-raw-counters with-raw-closure-ref-counter
                    with-was-closure-ref)
            (include "types.ss")
            (define add-create-and-alloc-counters
              (lambda (c* e)
                (if (track-dynamic-closure-counts)
                    (let f ([c* c*] [pair-count 0] [vector-count 0] [closure-count 0]
                                    [vector-alloc-amount 0] [closure-alloc-amount 0]
                                    [padded-vector-alloc-amount 0] [padded-closure-alloc-amount 0])
                      (if (null? c*)
                          (add-counter '#{pair-create-count bhowt6w0coxl0s2y-5} pair-count
                            (add-counter '#{vector-create-count bhowt6w0coxl0s2y-6} vector-count
                              (add-counter '#{closure-create-count bhowt6w0coxl0s2y-7} closure-count
                                (add-counter '#{vector-alloc-count bhowt6w0coxl0s2y-8} vector-alloc-amount
                                  (add-counter '#{closure-alloc-count bhowt6w0coxl0s2y-9} closure-alloc-amount
                                    (add-counter '#{padded-vector-alloc-count bhowt6w0coxl0s2y-11} padded-vector-alloc-amount
                                      (add-counter '#{padded-closure-alloc-count bhowt6w0coxl0s2y-10} padded-closure-alloc-amount
                                        e)))))))
                          (let ([c (car c*)])
                            (case (closure-type c)
                              [(pair) (f (cdr c*) (fx+ pair-count 1) vector-count closure-count
                                        vector-alloc-amount closure-alloc-amount padded-vector-alloc-amount
                                        padded-closure-alloc-amount)]
                              [(vector)
                               (let ([n (fx+ (length (closure-free* c)) 1)])
                                 (f (cdr c*) pair-count (fx+ vector-count 1) closure-count
                                   (fx+ vector-alloc-amount n) closure-alloc-amount
                                   (fx+ padded-vector-alloc-amount (fxsll (fxsra (fx+ n 1) 1) 1))
                                   padded-closure-alloc-amount))]
                              [(closure)
                               (let ([n (fx+ (length (closure-free* c)) 1)])
                                 (f (cdr c*) pair-count vector-count (fx+ closure-count 1)
                                   vector-alloc-amount (fx+ closure-alloc-amount n)
                                   padded-vector-alloc-amount
                                   (fx+ padded-closure-alloc-amount (fxsll (fxsra (fx+ n 1) 1) 1))))]
                              [else (f (cdr c*) pair-count vector-count closure-count
                                      vector-alloc-amount closure-alloc-amount padded-vector-alloc-amount
                                      padded-closure-alloc-amount)]))))
                    e)))
            (define add-counter
              (lambda (counter amount e)
                (with-output-language (L7 Expr)
                  (%seq
                    ,(%inline inc-profile-counter
                       ,(%mref
                          (literal ,(make-info-literal #t 'object counter (constant symbol-value-disp)))
                          ,(constant record-data-disp))
                       (quote ,amount))
                    ,e))))
            (define add-ref-counter
              (lambda (e)
                (if (track-dynamic-closure-counts)
                    (add-counter '#{ref-count bhowt6w0coxl0s2y-4} 1 e)
                    e)))
            (define-syntax with-raw-closure-ref-counter
              (syntax-rules ()
                [(_ ?x ?e1 ?e2 ...)
                 (let ([expr (begin ?e1 ?e2 ...)])
                   (if (and (track-dynamic-closure-counts) (uvar-was-closure-ref? ?x))
                       (add-counter '#{raw-ref-count bhowt6w0coxl0s2y-1} 1 expr)
                       expr))]))
            (define add-raw-counters
              (lambda (free** e)
                (if (track-dynamic-closure-counts)
                    (let f ([x** free**] [alloc 0] [raw 0])
                      (if (null? x**)
                          (add-counter '#{raw-create-count bhowt6w0coxl0s2y-2} (length free**)
                            (add-counter '#{raw-alloc-count bhowt6w0coxl0s2y-3} alloc
                              (add-counter '#{raw-ref-count bhowt6w0coxl0s2y-1} raw e)))
                          (let ([x* (car x**)])
                            (f (cdr x**) (fx+ alloc (length x*) 1)
                              (fold-left
                                (lambda (cnt x) (if (uvar-was-closure-ref? x) (fx+ cnt 1) cnt))
                                raw x*)))))
                    e)))
            (define-syntax with-was-closure-ref
              (syntax-rules ()
                [(_ ?x* ?e1 ?e2 ...)
                 (let f ([x* ?x*])
                   (if (or (null? x*) (not (track-dynamic-closure-counts)))
                       (begin ?e1 ?e2 ...)
                       (let ([x (car x*)])
                         (let ([old-was-cr? (uvar-was-closure-ref? x)])
                           (uvar-was-closure-ref! x #t)
                           (let ([expr (f (cdr x*))])
                             (uvar-was-closure-ref! x old-was-cr?)
                             expr)))))]))
            (define add-original-closures!
              (lambda (free**)
                (cond
                  [(track-static-closure-counts) =>
                   (lambda (ci)
                     (static-closure-info-raw-closure-count-set! ci
                       (fold-left (lambda (count free*)
                                    (static-closure-info-raw-free-var-count-set! ci
                                      (+ (static-closure-info-raw-free-var-count ci)
                                         (length free*)))
                                    (+ count 1))
                         (static-closure-info-raw-closure-count ci) free**)))])))
            (define add-final-closures!
              (lambda (c*)
                (cond
                  [(track-static-closure-counts) =>
                   (lambda (ci)
                     (for-each
                       (lambda (c)
                         (let ([type (closure-type c)])
                           (if (closure-wk? c)
                               (case type
                                 [(constant)
                                   (static-closure-info-wk-empty-count-set! ci
                                     (+ (static-closure-info-wk-empty-count ci) 1))]
                                 [(singleton)
                                   (static-closure-info-wk-single-count-set! ci
                                     (+ (static-closure-info-wk-single-count ci) 1))]
                                 [(pair)
                                   (static-closure-info-wk-pair-count-set! ci
                                     (+ (static-closure-info-wk-pair-count ci) 1))]
                                 [(vector)
                                   (static-closure-info-wk-vector-count-set! ci
                                     (+ (static-closure-info-wk-vector-count ci) 1))
                                   (static-closure-info-wk-vector-free-var-count-set! ci
                                     (+ (static-closure-info-wk-vector-free-var-count ci)
                                        (length (closure-free* c))))]
                                 [(borrowed)
                                   (static-closure-info-wk-borrowed-count-set! ci
                                     (+ (static-closure-info-wk-borrowed-count ci) 1))]
                                 [(closure)
                                   (static-closure-info-nwk-closure-count-set! ci
                                     (+ (static-closure-info-nwk-closure-count ci) 1))
                                   (static-closure-info-nwk-closure-free-var-count-set! ci
                                     (+ (static-closure-info-nwk-closure-free-var-count ci)
                                        (length (closure-free* c))))]
                                 [else (sorry! who "unexpected well-known closure type ~s" type)])
                               (case type
                                 [(constant)
                                   (static-closure-info-nwk-empty-count-set! ci
                                     (+ (static-closure-info-nwk-empty-count ci) 1))]
                                 [(closure)
                                   (static-closure-info-nwk-closure-count-set! ci
                                     (+ (static-closure-info-nwk-closure-count ci) 1))
                                   (static-closure-info-nwk-closure-free-var-count-set! ci
                                     (+ (static-closure-info-nwk-closure-free-var-count ci)
                                        (length (closure-free* c))))]
                                 [else (sorry! who "unexpected non-well-known closure type ~s" type)]))))
                       c*))]))))
          (define gl* '())
          (define gle* '())
          (define-record-type binding
            (fields l x x*)
            (nongenerative)
            (sealed #t)
            (protocol
              (lambda (new)
                (lambda (l x x*)
                  (new l x x*)))))
          (define binding-well-known?
            (lambda (b)
              (info-lambda-well-known?
                (uvar-info-lambda
                  (binding-x b)))))
          (define-record-type frob
            (fields name (mutable expr) (mutable seen frob-seen? frob-seen!))
            (nongenerative)
            (sealed #t)
            (protocol
              (lambda (new)
                (case-lambda
                  [(name expr) (new name expr #f)]
                  [(name expr seen) (new name expr seen)]))))
          (define-record-type closure
            (nongenerative)
            (sealed #t)
            (fields wk? name label b*
              (mutable sibling*) (mutable free*) (mutable type)
              (mutable seen closure-seen? closure-seen!)
              (mutable borrowed-name))
            (protocol
              (lambda (new)
                (lambda (wk? b*)
                  ; must use name and label of first binding
                  (let ([b (car b*)])
                    (let ([c (new wk? (binding-x b) (binding-l b) b* '() '() #f #f #f)])
                      (for-each
                        (lambda (b) (uvar-location-set! (binding-x b) c))
                        b*)
                      c))))))
          (module (make-bank deposit retain borrow)
            ; NB: borrowing is probably cubic at present
            ; might should represent bank as a prefix tree
            (define sort-free
              (lambda (free*)
                (sort (lambda (x y) (fx< (var-index x) (var-index y))) free*)))
            (define make-bank (lambda () '()))
            (define deposit
              ; NB: if used when self-references are possible, remove (olosure-name c) from free*
              (lambda (free* c bank)
                (cons (cons (sort-free free*) c)
                  (cons (cons (sort-free (cons (closure-name c) free*)) c)
                    bank))))
            (define retain
              (lambda (name* bank)
                (filter (lambda (a) (memq (closure-name (cdr a)) name*)) bank)))
            (define borrow
              ; NB: if used when self-references are possible, remove (olosure-name c) from free*
              (lambda (free* bank)
                (let ([free* (sort-free free*)])
                  (cond
                    [(assoc free* bank) => cdr]
                    [else #f])))))
          (module (with-offsets)
            (define set-offsets!
              (lambda (x* index)
                (do ([x* x* (cdr x*)] [index index (fx+ index 1)])
                  ((null? x*) index)
                  (var-index-set! (car x*) index))))
            (define-syntax with-offsets
              (syntax-rules ()
                [(_ index ?x* ?e1 ?e2 ...)
                 (identifier? #'index)
                 (let ([x* ?x*])
                   (let ([index (set-offsets! x* index)])
                     (let ([v (begin ?e1 ?e2 ...)])
                       (for-each (lambda (x) (var-index-set! x #f)) x*)
                       v)))])))
          (with-output-language (L7 Expr)
            (module (create-bindings create-inits)
              (define (build-free-ref x)
                (let ([loc (uvar-location x)])
                  (when (eq? loc 'loop)
                    (sorry! who "found reference to loop variable outside call position" x))
                  (frob-expr loc)))
              (define create-bindings
                (lambda (c* body)
                  (fold-right
                    (lambda (c body)
                      (case (closure-type c)
                        ; NB: the pair and vector cases can be done this way only if well-known
                        ; NB: closures can be shared with each other and up to one non-well-known closure
                        [(pair)
                         `(let ([,(closure-name c) ,(%primcall #f #f cons ,(map build-free-ref (closure-free* c)) ...)])
                            ,body)]
                        [(vector)
                         `(let ([,(closure-name c) ,(%primcall #f #f vector ,(map build-free-ref (closure-free* c)) ...)])
                            ,body)]
                        [else
                          (safe-assert (eq? (closure-type c) 'closure))
                          `(let ([,(closure-name c) ,(%constant-alloc type-closure
                                                          (fx* (fx+ (length (closure-free* c)) 1) (constant ptr-bytes)))])
                             ,(%seq
                                (set! ,(%mref ,(closure-name c) ,(constant closure-code-disp))
                                  (label-ref ,(closure-label c) ,(constant code-data-disp)))
                                ,body))]))
                    (add-create-and-alloc-counters c* body)
                    c*)))
              (define create-inits
                (lambda (c* body)
                  (fold-right
                    (lambda (c body)
                      (case (closure-type c)
                        [(closure)
                         (let f ([x* (closure-free* c)] [i (constant closure-data-disp)])
                           (if (null? x*)
                               body
                               (%seq
                                 (set! ,(%mref ,(closure-name c) ,i) ,(build-free-ref (car x*)))
                                 ,(f (cdr x*) (fx+ i (constant ptr-bytes))))))]
                        [else body]))
                    body c*))))
            (define-syntax with-frob-location
              (syntax-rules ()
                [(_ ?x ?expr ?e)
                 (let ([frob (uvar-location ?x)])
                   (let ([loc (frob-expr frob)])
                     (frob-expr-set! frob ?expr)
                     (let ([v ?e])
                       (frob-expr-set! frob loc)
                       v)))]))
            (define with-locations
              (lambda (type free* mcp body index bank)
                (case type
                  [(singleton) (with-frob-location (car free*) mcp (Expr body index bank))]
                  [(pair)
                   (with-frob-location (car free*) (add-ref-counter (%mref ,mcp ,(constant pair-car-disp)))
                     (with-frob-location (cadr free*) (add-ref-counter (%mref ,mcp ,(constant pair-cdr-disp)))
                       (Expr body index bank)))]
                  [else
                    (safe-assert (memq type '(vector closure)))
                    (let f ([free* free*] [i (if (eq? type 'vector) (constant vector-data-disp) (constant closure-data-disp))])
                      (if (null? free*)
                          (Expr body index bank)
                          (with-frob-location (car free*) (add-ref-counter (%mref ,mcp ,i))
                            (f (cdr free*) (fx+ i (constant ptr-bytes))))))])))))
        (CaseLambdaExpr : CaseLambdaExpr (ir index c bank) -> CaseLambdaExpr ()
          [(case-lambda ,info ,cl* ...)
           (info-lambda-fv*-set! info (closure-free* c))
           (info-lambda-closure-rep-set! info (closure-type c))
           `(case-lambda ,info
              ,(let ([bank (retain (closure-free* c) bank)])
                 (map (lambda (cl) (CaseLambdaClause cl index c bank)) cl*))
              ...)])
        (CaseLambdaClause : CaseLambdaClause (ir index c bank) -> CaseLambdaClause ()
          [(clause (,x* ...) ,mcp ,interface ,body)
           (with-offsets index x*
             (let ([type (if (and c mcp) (closure-type c) 'constant)])
               (if (eq? type 'constant)
                   `(clause (,x* ...) #f ,interface ,(Expr body index bank))
                   `(clause (,x* ...) ,mcp ,interface
                      ,(with-frob-location (closure-name c) mcp
                         (if (eq? type 'borrowed)
                             (with-frob-location (closure-borrowed-name c) mcp
                               (let ([free* (closure-free* c)])
                                 (with-locations (if (fx= (length free*) 2) 'pair 'vector) free* mcp body index bank)))
                             (with-locations type (closure-free* c) mcp body index bank)))))))])
        (Expr : Expr (ir index bank) -> Expr ()
          [(closures ([,x* (,x** ...) ,le*] ...) ,body)
           (with-offsets index x*
             (safe-assert (andmap var-index x*)) ; should be bound now
             (safe-assert (andmap (lambda (x*) (andmap var-index x*)) x**)) ; should either have already been bound, or are bound now
             (add-original-closures! x**)
             (let* ([x**-loc (map (lambda (x*) (map uvar-location x*)) x**)]
                    [l* (map (lambda (x) (make-local-label (uvar-name x))) x*)]
                    ; create one closure for each not-well-known binding, and one for all well-known bindings
                    [c* (let-values ([(wk* !wk*) (partition binding-well-known? (map make-binding l* x* x**))])
                          (cond
                            [(null? wk*) (map (lambda (b) (make-closure #f (list b))) !wk*)]
                            [(null? !wk*) (list (make-closure #t wk*))]
                            [else
                              ; putting one !wk* in with wk*.  claim: if any of the closures is nonempty,
                              ; all will be nonempty, so might as well allow wk* to share a !wk's closure.
                              ; if all are empty, no harm done.
                              ; TODO: there might be a more suitable !wk to pick than (car !wk*)
                              (cons
                                (make-closure #f (cons (car !wk*) wk*))
                                (map (lambda (b) (make-closure #f (list b))) (cdr !wk*)))]))]
                    [xc* (map uvar-location x*)])

               ; set up sibling* and initial free*
               (for-each
                 (lambda (c)
                   (let fb ([b* (closure-b* c)] [free* '()] [sibling* '()])
                     (if (null? b*)
                         (begin
                           (closure-free*-set! c free*)
                           (closure-sibling*-set! c sibling*))
                         (let fx ([x* (binding-x* (car b*))] [free* free*] [sibling* sibling*])
                           (if (null? x*)
                               (fb (cdr b*) free* sibling*)
                               (let* ([x (car x*)] [loc (uvar-location x)])
                                 (cond
                                   [(not loc)
                                    (let ([frob (make-frob x x #t)])
                                      (uvar-location-set! x frob)
                                      (fx (cdr x*) (cons x free*) sibling*)
                                      (frob-seen! frob #f))]
                                   [(frob? loc)
                                    (if (or (frob-seen? loc) (not (frob-name loc)))
                                        (fx (cdr x*) free* sibling*)
                                        (begin
                                          (frob-seen! loc #t)
                                          (fx (cdr x*) (cons (frob-name loc) free*) sibling*)
                                          (frob-seen! loc #f)))]
                                   [(closure? loc)
                                    (if (or (eq? loc c) (closure-seen? loc)) ; no reflexive links
                                        (fx (cdr x*) free* sibling*)
                                        (begin
                                          (closure-seen! loc #t)
                                          (fx (cdr x*) free* (cons (closure-name loc) sibling*))
                                          (closure-seen! loc #f)))]
                                   [else (sorry! who "unexpected uvar location ~s" loc)])))))))
                 c*)

               ; find closures w/free variables (non-constant closures) and propagate
               (when (ormap (lambda (c) (not (null? (closure-free* c)))) c*)
                 (for-each
                   (lambda (c)
                     (closure-free*-set! c (append (closure-sibling* c) (closure-free* c))))
                   c*))

               ; determine each closure's representation & set uvar location frobs
               (for-each
                 (lambda (c)
                   (let ([free* (closure-free* c)])
                     (let ([frob (cond
                                   [(null? free*)
                                    (closure-type-set! c 'constant)
                                    (make-frob #f `(literal ,(make-info-literal #f 'closure (closure-label c) 0)))]
                                   [(closure-wk? c)
                                    (cond
                                      [(fx= (length free*) 1)
                                       (closure-type-set! c 'singleton)
                                       (uvar-location (car free*))]
                                      [(borrow free* bank) =>
                                       (lambda (mc)
                                         (closure-type-set! c 'borrowed)
                                         (closure-borrowed-name-set! c (closure-name mc))
                                         (closure-free*-set! c (closure-free* mc))
                                         (uvar-location (closure-name mc)))]
                                      [else
                                        ; NB: HACK
                                        (set! bank (deposit free* c bank))
                                        (closure-type-set! c (if (fx= (length free*) 2) 'pair 'vector))
                                        (make-frob (closure-name c) (closure-name c))])]
                                   [else
                                     (closure-type-set! c 'closure)
                                     (make-frob (closure-name c) (closure-name c))])])
                       (for-each
                         (lambda (b) (uvar-location-set! (binding-x b) frob))
                         (closure-b* c)))))
                 c*)

               ; NB: if we are not sharing, but we are borrowing, we need to ensure
               ; NB: all closure variables point to final frob, and not a closure record

               ; record static closure counts
               (add-final-closures! c*)

               ; process subforms and rebuild
               (fold-left (lambda (body le)
                            (nanopass-case (L6 CaseLambdaExpr) le
                              [(case-lambda ,info ,cl ...) body]))
                 (let ([le* (map (lambda (le xc x*) (with-was-closure-ref x* (CaseLambdaExpr le index xc bank)))
                              le* xc* x**)]
                       [body (Expr body index bank)])
                   (set! gl* (append l* gl*))
                   (set! gle* (append le* gle*))
                   (let ([c* (filter (lambda (c) (memq (closure-type c) '(pair closure vector))) c*)])
                     (let ([body (create-bindings c* (create-inits c* (add-raw-counters x** body)))])
                       ; leave location clean for later passes
                       (for-each (lambda (x) (uvar-location-set! x #f)) x*)
                       (for-each (lambda (x* x*-loc) (for-each uvar-location-set! x* x*-loc)) x** x**-loc)
                       body)))
                 le*)))]
          [,x (with-raw-closure-ref-counter x (cond [(uvar-location x) => frob-expr] [else x]))]
          [(loop ,x (,x* ...) ,body)
           (uvar-location-set! x 'loop)
           (let ([body (with-offsets index x* (Expr body index bank))])
             (uvar-location-set! x #f)
             `(loop ,x (,x* ...) ,body))]
          [(call ,info ,mdcl ,x ,[e*] ...)
           (guard (eq? (uvar-location x) 'loop))
           `(call ,info ,mdcl ,x ,e* ...)]
          [(call ,info ,mdcl ,x ,[e*] ...)
           (guard mdcl)
           (with-raw-closure-ref-counter x
             (cond
               [(uvar-location x) =>
                (lambda (frob)
                  (if (frob-name frob)
                      `(call ,info ,mdcl ,(frob-expr frob) ,e* ...)
                      `(call ,info ,mdcl #f ,e* ...)))]
               [else `(call ,info ,mdcl ,x ,e* ...)]))]
          [(fcallable ,info)
           (let ([label (make-local-label 'fcallable)])
             (set! gl* (cons label gl*))
             (set! gle* (cons (in-context CaseLambdaExpr `(fcallable ,info ,label)) gle*))
             `(label-ref ,label 0))]
          [(let ([,x* ,[e*]] ...) ,body)
           (with-offsets index x*
             `(let ([,x* ,e*] ...) ,(Expr body index bank)))]
          [(mvlet ,[e] ((,x** ...) ,interface* ,body*) ...)
           (let f ([var** x**] [body* body*] [rbody* '()])
             (if (null? var**)
                 `(mvlet ,e ((,x** ...) ,interface* ,(reverse rbody*)) ...)
                 (f (cdr var**) (cdr body*) (cons (with-offsets index (car var**) (Expr (car body*) index bank)) rbody*))))])
        (nanopass-case (L6 CaseLambdaExpr) ir
          [(case-lambda ,info ,[CaseLambdaClause : cl 0 #f (make-bank) -> cl] ...)
           (let ([l (make-local-label 'main)])
             (let-values ([(gl* gle*) (sort-bindings gl* gle*)])
               `(labels ([,gl* ,gle*] ... [,l (case-lambda ,info ,cl ...)]) ,l)))])))

    (define-pass np-simplify-if : L7 (ir) -> L7 ()
      (definitions
        (define-$type-check (L7 Expr))
        (with-output-language (L7 Expr)
          ; (and (fixnum? x1) ... (fixnum xn) e ...) => (and (fixnum? (logor x1 ... xn)) e ...)
          ; restricting fixnum? arguments to vars to avoid unnecessary computation
          (define process-fixnum?
            (lambda (info1 pr1 e x*)
              (define build-fixnum?
                (lambda (x*)
                  `(call ,info1 #f ,pr1
                     ,(if (fx= (length x*) 1)
                          (car x*)
                          (%primcall #f #f fxlogor ,x* ...)))))
              (let f ([e e] [x* x*])
                (nanopass-case (L7 Expr) e
                  [(if (call ,info1 ,mdcl ,pr1 ,x1) ,e2 (quote ,d))
                   (guard (eq? mdcl #f) (eq? (primref-name pr1) 'fixnum?) (eq? d #f))
                   (f e2 (cons x1 x*))]
                  [(call ,info1 ,mdcl ,pr1 ,x1)
                   (guard (eq? mdcl #f) (eq? (primref-name pr1) 'fixnum?))
                   (build-fixnum? (cons x1 x*))]
                  [else `(if ,(build-fixnum? x*) ,(Expr e) (quote #f))]))))
          (define process-paired-predicate
            (lambda (info1 pr1 pr2 x-arg)
              (let ([pr1 (primref-name pr1)] [pr2 (primref-name pr2)])
                (cond
                  [(and (eq? pr1 'integer?) (eq? pr2 'exact?))
                   `(if ,(%primcall #f #f fixnum? ,x-arg) (quote #t) ,(%primcall #f #f bignum? ,x-arg))]
                  [(and (eq? pr1 'port?) (eq? pr2 'binary-port?))
                   (%typed-object-check mask-binary-port type-binary-port ,x-arg)]
                  [(and (eq? pr1 'port?) (eq? pr2 'textual-port?))
                   (%typed-object-check mask-textual-port type-textual-port ,x-arg)]
                  [(and (eq? pr1 'input-port?) (eq? pr2 'binary-port?))
                   (%typed-object-check mask-binary-input-port type-binary-input-port ,x-arg)]
                  [(and (eq? pr1 'input-port?) (eq? pr2 'textual-port?))
                   (%typed-object-check mask-textual-input-port type-textual-input-port ,x-arg)]
                  [(and (eq? pr1 'output-port?) (eq? pr2 'binary-port?))
                   (%typed-object-check mask-binary-output-port type-binary-output-port ,x-arg)]
                  [(and (eq? pr1 'output-port?) (eq? pr2 'textual-port?))
                   (%typed-object-check mask-textual-output-port type-textual-output-port ,x-arg)]
                  [else #f]))))))
      (Expr : Expr (ir) -> Expr ()
        [(if (call ,info1 ,mdcl ,pr1 ,x1) ,e2 (quote ,d))
         (guard (eq? d #f) (eq? mdcl #f))
         (if (eq? (primref-name pr1) 'fixnum?)
             (process-fixnum? info1 pr1 e2 (list x1))
             (or (and (nanopass-case (L7 Expr) e2
                        [(if (call ,info5 ,mdcl5 ,pr2 ,x2) ,e2 (quote ,d))
                         (guard (eq? x2 x1) (eq? mdcl5 #f) (eq? d #f))
                         (let ([e-paired-pred (process-paired-predicate info1 pr1 pr2 x1)])
                           (and e-paired-pred `(if ,e-paired-pred ,(Expr e2) (quote #f))))]
                        [(call ,info4 ,mdcl4 ,pr2 ,x2)
                         (guard (eq? x2 x1) (eq? mdcl4 #f))
                         (process-paired-predicate info1 pr1 pr2 x1)]
                        [else #f]))
                 `(if (call ,info1 ,mdcl ,pr1 ,x1) ,(Expr e2) (quote ,d))))]))

    (module (np-profile-unroll-loops)
      (define-syntax mvmap
        (lambda (x)
          (syntax-case x ()
            [(_ ?n ?proc ?ls1 ?ls2 ...)
             (let ([n (datum ?n)])
               (unless (and (fixnum? n) (fx>= n 0)) (syntax-error #'?n "invalid return-value count"))
               (let ([foo* (make-list n)])
                 (with-syntax ([(ls2 ...) (generate-temporaries #'(?ls2 ...))]
                               [(out ...) (generate-temporaries foo*)]
                               [(out* ...) (generate-temporaries foo*)])
                   #'(let ([proc ?proc])
                       (let f ([ls1 ?ls1] [ls2 ?ls2] ...)
                         (if (null? ls1)
                             (let ([out '()] ...) (values out ...))
                             (let-values ([(out ...) (proc (car ls1) (car ls2) ...)]
                                          [(out* ...) (f (cdr ls1) (cdr ls2) ...)])
                               (values (cons out out*) ...))))))))])))
      (define-who loop-unroll-limit
        ($make-thread-parameter
          0 ; NB: disabling loop unrolling for now
          (lambda (x)
            (cond
              [(fixnum? x) x]
              [else ($oops who "~s is not a fixnum" x)]))))
      (define PATH-SIZE-LIMIT 100)
      ;; NB: this comment is no longer accurate
      ;; Code growth computation is a little restrictive since it's measured
      ;; per loop... but maybe since new-size is weighted when profiling is
      ;; enabled it's fine.
      #;(define CODE-GROWTH-FACTOR (fx1+ (loop-unroll-limit)))
      (define-syntax delay
        (syntax-rules ()
          [(_ x) (lambda () x)]))
      (define (force x) (if (procedure? x) (x) x))
      (define-who analyze-loops ;; -> (lambda () body) size new-weighted-size
        (lambda (body path-size unroll-count)
          (with-output-language (L7 Expr)
            ;; Not really a loop, just didn't want to pass around path-size and unroll-count when unnecessary
            (let loop ([body body])
              (if (not body)
                  (values #f 0 0)
                  (nanopass-case (L7 Expr) body
                    [(literal ,info) (values body 0 0)]
                    [(immediate ,imm) (values body 0 0)]
                    [(quote ,d) (values body 0 0)]
                    [(goto ,l) (values body 1 1)]
                    [(mref ,[loop : e1 -> e1-promise e1-size e1-new-size] ,[loop : e2 -> e2-promise e2-size e2-new-size] ,imm)
                     (values (delay `(mref ,(force e1-promise) ,(force e2-promise) ,imm))
                       (fx+ e1-size e2-size 1)
                       (fx+ e1-new-size e2-new-size 1))]
                    [,lvalue (values body 1 1)]
                    [(profile ,src) (values body 0 0)]
                    [(pariah) (values body 0 0)]
                    [(label-ref ,l ,offset) (values body 0 0)]
                    [,pr (values body 1 1)]
                    [(inline ,info ,prim ,[loop : e* -> e*-promise size* new-size*] ...)
                     (values (delay `(inline ,info ,prim ,(map force e*-promise) ...))
                       (apply fx+ size*)
                       (apply fx+ new-size*))]
                    [(values ,info ,[loop : e* -> e*-promise size* new-size*] ...)
                     (values (delay `(values ,info ,(map force e*-promise) ...))
                       (apply fx+ size*)
                       (apply fx+ new-size*))]
                    [(call ,info ,mdcl ,x ,[loop : e* -> e*-promise size* new-size*] ...)
                     (guard (uvar-location x))
                     ;; NB: Magic formulas, using number assuming query-count \in [0,1000]
                     (let* ([src (info-call-src info)]
                            [query-count (if src (profile-query-weight src) #f)]
                            ;; don't bother with unimportant loops (less than 1% count relative to max)
                            [query-count (if (or (not query-count) (< query-count .1)) 0 (exact (truncate (* query-count 1000))))]
                            ;; allow path-size to increase up to 300
                            [adjusted-path-size-limit (fx+ PATH-SIZE-LIMIT (fx/ (or query-count 0) 5))]
                            ;; allow unroll limit to increase up to 4
                            [adjusted-unroll-limit (fx+ (loop-unroll-limit) (fx/ (or query-count 0) 300))])
                       (if (or (fxzero? query-count)
                               (fxzero? (fx+ unroll-count adjusted-unroll-limit))
                               (fx> path-size adjusted-path-size-limit))
                           (begin
                             (values (delay `(call ,info ,mdcl ,x ,(map force e*-promise) ...))
                               (fx1+ (apply fx+ size*))
                               (fx1+ (apply fx+ new-size*))))
                           (let*-values ([(var*) (car (uvar-location x))]
                                         [(loop-body-promise body-size new-size) (analyze-loops (cdr (uvar-location x)) (fx1+ path-size) (fx1- unroll-count))]
                                         [(new-size) ((lambda (x) (if query-count (fx/ x query-count) x)) (fx+ (length e*-promise) new-size))]
                                         [(acceptable-new-size) (fx* (fx1+ adjusted-unroll-limit) body-size)])
                             ;; NB: trying code growth computation here, where it could be per call site.
                             (values
                               (if (<= new-size acceptable-new-size)
                                   (delay (fold-left
                                            (lambda (body var e-promise)
                                              `(seq (set! ,var ,(force e-promise)) ,body))
                                            (rename-loop-body (force loop-body-promise))
                                            var* e*-promise))
                                   body)
                               (fx1+ (apply fx+ size*))
                               ;; pretend the new size is smaller for important loops
                               new-size))))]
                    [(call ,info ,mdcl ,pr ,e* ...)
                     (let-values ([(e*-promise size* new-size*) (mvmap 3 (lambda (e) (analyze-loops e (fx1+ path-size) unroll-count)) e*)])
                       (values (delay `(call ,info ,mdcl ,pr ,(map force e*-promise) ...))
                         (fx+ 2 (apply fx+ size*))
                         (fx+ 2 (apply fx+ new-size*))))]
                    [(call ,info ,mdcl ,e ,e* ...)
                     (let-values ([(e-promise e-size e-new-size) (loop e)]
                                  [(e*-promise size* new-size*) (mvmap 3 (lambda (e) (analyze-loops e (fx1+ path-size) unroll-count)) e*)])
                       (values (delay `(call ,info ,mdcl ,(force e-promise) ,(map force e*-promise) ...))
                         (fx+ 5 e-size (apply fx+ size*))
                         (fx+ 5 e-new-size (apply fx+ new-size*))))]
                    [(foreign-call ,info ,[loop : e -> e-promise e-size e-new-size] ,[loop : e* -> e*-promise size* new-size*] ...)
                     (values (delay `(foreign-call ,info ,(force e-promise) ,(map force e*-promise) ...))
                       (fx+ 5 e-size (apply fx+ size*))
                       (fx+ 5 e-new-size (apply fx+ new-size*)))]
                    [(label ,l ,[loop : body -> e size new-size])
                     (values (delay `(label ,l ,(force e))) size new-size)]
                    [(mvlet ,[loop : e -> e-promise e-size e-new-size] ((,x** ...) ,interface* ,body*) ...)
                     (let-values ([(body*-promise body*-size body*-new-size) (mvmap 3 (lambda (e) (analyze-loops e (fx+ e-size path-size) unroll-count)) body*)])
                       (values (delay `(mvlet ,(force e-promise) ((,x** ...) ,interface* ,(map force body*-promise)) ...))
                         (fx+ e-size (apply fx+ body*-size))
                         (fx+ e-new-size (apply fx+ body*-new-size))))]
                    [(mvcall ,info ,e1 ,e2)
                     (let-values ([(e1-promise e1-size e1-new-size) (analyze-loops e1 (fx+ 5 e1) unroll-count)]
                                  [(e2-promise e2-size e2-new-size) (analyze-loops e2 (fx+ 5 e2) unroll-count)])
                       (values (delay `(mvcall ,info ,(force e1-promise) ,(force e2-promise)))
                         (fx+ 5 e1-size e2-size)
                         (fx+ 5 e1-new-size e2-new-size)))]
                    [(let ([,x* ,[loop : e* -> e*-promise size* new-size*]] ...) ,body)
                     (let-values ([(body-promise body-size body-new-size) (analyze-loops body (fx+ path-size (apply fx+ size*)) unroll-count)])
                       (values (delay `(let ([,x* ,(map force e*-promise)] ...) ,(force body-promise)))
                         (fx+ 1 body-size (apply fx+ size*))
                         (fx+ 1 body-new-size (apply fx+ new-size*))))]
                    [(if ,[loop : e0 -> e0-promise e0-size e0-new-size] ,e1 ,e2)
                     (let-values ([(e1-promise e1-size e1-new-size) (analyze-loops e1 (fx+ path-size e0-size) unroll-count)]
                                  [(e2-promise e2-size e2-new-size) (analyze-loops e2 (fx+ path-size e0-size) unroll-count)])
                       (values (delay `(if ,(force e0-promise) ,(force e1-promise) ,(force e2-promise)))
                         (fx+ e0-size e1-size e2-size)
                         (fx+ e0-new-size e1-new-size e2-new-size)))]
                    [(seq ,[loop : e0 -> e0-promise e0-size e0-new-size] ,e1)
                     (let-values ([(e1-promise e1-size e1-new-size) (analyze-loops e1 (fx+ path-size e0-size) unroll-count)])
                       (values (delay `(seq ,(force e0-promise) ,(force e1-promise)))
                         (fx+ e0-size e1-size)
                         (fx+ e0-new-size e1-new-size)))]
                    [(set! ,lvalue ,[loop : e -> e-promise e-size e-new-size])
                     (values (delay `(set! ,lvalue ,(force e-promise)))
                       (fx+ 1 e-size)
                       (fx+ 1 e-new-size))]
                    [(alloc ,info ,[loop : e -> e-promise e-size e-new-size])
                     (values (delay `(alloc ,info ,(force e-promise)))
                       (fx+ 1 e-size)
                       (fx+ 1 e-new-size))]
                    [(loop ,x (,x* ...) ,[loop : body -> body-promise body-size body-new-size])
                     ;; NB: Handling of inner loops?
                     (values (delay `(loop ,x (,x* ...) ,(force body-promise)))
                       body-size
                       body-new-size)]
                    [else ($oops who "forgot a case: ~a" body)]))))))

      (define-pass rename-loop-body : (L7 Expr) (ir) -> (L7 Expr) ()
        (definitions
          (define-syntax with-fresh
            (syntax-rules ()
              [(_ rename-ht x* body)
               (let* ([x* x*]
                      [rename-ht (hashtable-copy rename-ht #t)]
                      [x* (let ([t* (map (lambda (x) (make-tmp (uvar-name x))) x*)])
                            (for-each (lambda (x t) (eq-hashtable-set! rename-ht x t)) x* t*)
                            t*)])
                 body)])))
        (Lvalue : Lvalue (ir rename-ht) -> Lvalue ()
          [,x (eq-hashtable-ref rename-ht x x)]
          [(mref ,[e1] ,[e2] ,imm) `(mref ,e1 ,e2 ,imm)])
        (Expr : Expr (ir rename-ht) -> Expr ()
          [(loop ,x (,[Lvalue : x* rename-ht -> x*] ...) ,body)
           ;; NB: with-fresh is so well designed that it can't handle this case
           (let*-values ([(x) (list x)]
                         [(x body) (with-fresh rename-ht x (values (car x) (Expr body rename-ht)))])
             `(loop ,x (,x* ...) ,body))]
          [(let ([,x* ,[e*]] ...) ,body)
           (with-fresh rename-ht x*
             `(let ([,x* ,e*] ...) ,(Expr body rename-ht)))]
          [(mvlet ,[e] ((,x** ...) ,interface* ,body*) ...)
           (let* ([x**/body* (map (lambda (x* body)
                                    (with-fresh rename-ht x* (cons x* (Expr body rename-ht))))
                               x** body*)]
                  [x** (map car x**/body*)]
                  [body* (map cdr x**/body*)])
             `(mvlet ,e ((,x** ...) ,interface* ,body*) ...))])
        (Expr ir (make-eq-hashtable)))

      (define-pass np-profile-unroll-loops : L7 (ir) -> L7 ()
        (Expr : Expr (ir) -> Expr ()
          [(loop ,x (,x* ...) ,body)
           (uvar-location-set! x (cons x* body))
           (let-values ([(e-promise size new-size) (analyze-loops body 0 (loop-unroll-limit))])
             (uvar-location-set! x #f)
             ;; NB: Not fx
             `(loop ,x (,x* ...) ,(force e-promise))
             ;; trying out code-growth computation higher up
             #;(if (<= new-size (* size CODE-GROWTH-FACTOR))
                   (begin
                     #;(printf "Opt: ~a\n" x)
                     `(loop ,x (,x* ...) ,(force e-promise)))
                   (begin
                     #;(printf "New size: ~a, old size: ~a\n" new-size size)
                     ir)))]))
      (set! $loop-unroll-limit loop-unroll-limit))

    (define target-fixnum?
      (if (and (= (constant most-negative-fixnum) (most-negative-fixnum))
               (= (constant most-positive-fixnum) (most-positive-fixnum)))
          fixnum?
          (lambda (x)
            (and (or (fixnum? x) (bignum? x))
                 (<= (constant most-negative-fixnum) x (constant most-positive-fixnum))))))

    (define unfix
      (lambda (imm)
        (ash imm (fx- (constant fixnum-offset)))))

    (define fix
      (lambda (imm)
        (ash imm (constant fixnum-offset))))

    (define ptr->imm
      (lambda (x)
        (cond
          [(eq? x #f) (constant sfalse)]
          [(eq? x #t) (constant strue)]
          [(eq? x (void)) (constant svoid)]
          [(null? x) (constant snil)]
          [(eof-object? x) (constant seof)]
          [($unbound-object? x) (constant sunbound)]
          [(bwp-object? x) (constant sbwp)]
          [(target-fixnum? x) (fix x)]
          [(char? x) (+ (* (constant char-factor) (char->integer x)) (constant type-char))]
          [else #f])))

    (define-syntax ref-reg
      (lambda (x)
        (syntax-case x ()
          [(k reg)
           (identifier? #'reg)
           (if (real-register? (datum reg))
               #'reg
               (with-implicit (k %mref) #`(%mref ,%tc ,(tc-disp reg))))])))

    ; TODO: recognize a direct call when it is at the end of a sequence, closures, or let form
    ; TODO: push call into if? (would need to pull arguments into temporaries to ensure order of evaluation
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
      (define-pass np-expand-primitives : L7 (ir) -> L9 ()
        (Program : Program (ir) -> Program ()
          [(labels ([,l* ,le*] ...) ,l)
           (fluid-let ([new-l* '()] [new-le* '()])
             (let ([le* (map CaseLambdaExpr le*)])
               `(labels ([,l* ,le*] ... [,new-l* ,new-le*] ...) ,l)))])
        (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ())
        (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ())
        (Expr : Expr (ir) -> Expr ()
          [(quote ,d)
           (cond
             [(ptr->imm d) => (lambda (i) `(immediate ,i))]
             [else `(literal ,(make-info-literal #f 'object d 0))])]
          [,pr (Symref (primref-name pr))]
          [(call ,info0 ,mdcl0
             (call ,info1 ,mdcl1 ,pr (quote ,d))
             ,[e*] ...)
           (guard (and (eq? (primref-name pr) '$top-level-value) (symbol? d)))
           `(call ,info0 ,mdcl0 ,(Symref d) ,e* ...)]
          [(call ,info ,mdcl ,pr ,e* ...)
           (cond
             [(handle-prim (info-call-src info) (info-call-sexpr info) (primref-level pr) (primref-name pr) e*) => Expr]
             [else
              (let ([e* (map Expr e*)])
                ; NB: expand calls through symbol top-level values similarly
                (let ([info (if (any-set? (prim-mask abort-op) (primref-flags pr))
                                (make-info-call (info-call-src info) (info-call-sexpr info) (info-call-check? info) #t #t)
                                info)])
                  `(call ,info ,mdcl ,(Symref (primref-name pr)) ,e* ...)))])]))
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
            (if (no-need-to-bind? multiple-ref? e)
                (values e values)
                (let ([t (make-tmp 't type)])
                  (values t
                    (lambda (body)
                      `(let ([,t ,e]) ,body)))))))
        (define list-binder
          (lambda (multiple-ref? type e*)
            (if (null? e*)
                (values '() values)
                (let-values ([(e dobind) (binder multiple-ref? type (car e*))]
                             [(e* dobind*) (list-binder multiple-ref? type (cdr e*))])
                  (values (cons e e*)
                    (lambda (body)
                      (dobind (dobind* body))))))))
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
        (define build-simple-or
          (lambda (e1 e2)
            `(if ,e1 ,(%constant strue) ,e2)))
         (define build-fix
           (lambda (e)
             (%inline sll ,e ,(%constant fixnum-offset))))
         (define build-unfix
           (lambda (e)
             (nanopass-case (L7 Expr) e
               [(quote ,d) (guard (target-fixnum? d)) `(immediate ,d)]
               [else (%inline sra ,e ,(%constant fixnum-offset))])))
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
            (build-libcall #f src sexpr eqv? e1 e2)))
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
        (define build-dirty-store
          (case-lambda
            [(base offset e) (build-dirty-store base %zero offset e)]
            [(base index offset e) (build-dirty-store base index offset e
                                     (lambda (base index offset e) `(set! ,(%mref ,base ,index ,offset) ,e))
                                     (lambda (s r) `(seq ,s ,r)))]
            [(base index offset e build-assign build-seq)
             (if (nanopass-case (L7 Expr) e
                   [(quote ,d) (ptr->imm d)]
                   [else #f])
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
                         (bind #t ([a a])
                           (build-seq
                             (build-assign a %zero 0 e)
                             (%inline remember ,a))))
                       (bind #t ([e e])
                         ; eval a second so the address is not live across any calls
                         (bind #t ([a a])
                           (build-seq
                             (build-assign a %zero 0 e)
                             `(if ,(%type-check mask-fixnum type-fixnum ,e)
                                  ,(%constant svoid)
                                  ,(%inline remember ,a))))))))]))
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
            (if (constant? flonum? x)
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
                             [(1) (let ([imm (logand imm #xff)])
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
                `(seq
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
                         ,%ac0))))))
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
          (lambda (swapped? type base index offset build-int)
            (case type
              [(integer-8 unsigned-8)
               (build-int `(inline ,(make-info-load type #f) ,%load ,base ,index (immediate ,offset)))]
              [(integer-16 integer-32 unsigned-16 unsigned-32)
               (build-int `(inline ,(make-info-load type swapped?) ,%load ,base ,index (immediate ,offset)))]
              [(integer-64 unsigned-64)
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
               (constant-case unaligned-integers
                 [(#t)
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
               (constant-case unaligned-integers
                 [(#t)
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
               (constant-case unaligned-integers
                 [(#t)
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
               (constant-case unaligned-integers
                 [(#t)
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
                      (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                        (%seq
                          (inline ,(make-info-loadfl %flreg1) ,%load-double
                            ,base ,index (immediate ,offset))
                          (inline ,(make-info-loadfl %flreg1) ,%store-double
                            ,t ,%zero ,(%constant flonum-data-disp))
                          ,t))))]
               [(single-float)
                (if swapped?
                    (bind #f (base index)
                      (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                        (%seq
                          (set! ,(%mref ,t ,(constant flonum-data-disp))
                            (inline ,(make-info-load 'unsigned-32 #t) ,%load ,base ,index
                              (immediate ,offset)))
                          (inline ,(make-info-loadfl %flreg1) ,%load-single->double
                            ,t ,%zero ,(%constant flonum-data-disp))
                          (inline ,(make-info-loadfl %flreg1) ,%store-double
                            ,t ,%zero ,(%constant flonum-data-disp))
                          ,t)))
                    (bind #f (base index)
                      (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                        (%seq
                          (inline ,(make-info-loadfl %flreg1) ,%load-single->double
                            ,base ,index (immediate ,offset))
                          (inline ,(make-info-loadfl %flreg1) ,%store-double
                            ,t ,%zero ,(%constant flonum-data-disp))
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
          (lambda (swapped? type base index offset value)
            (case type
              [(integer-8 unsigned-8)
               `(inline ,(make-info-load type #f) ,%store ,base ,index (immediate ,offset) ,value)]
              [(integer-16 integer-32 integer-64 unsigned-16 unsigned-32 unsigned-64)
               `(inline ,(make-info-load type swapped?) ,%store ,base ,index (immediate ,offset) ,value)]
              [(integer-24 unsigned-24)
               (constant-case unaligned-integers
                 [(#t)
                  (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                            (values (+ offset 1) offset)
                                            (values offset (+ offset 2)))])
                    (bind #t (base index value)
                      (%seq
                        (inline ,(make-info-load 'unsigned-16 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                        (inline ,(make-info-load 'unsigned-8 #f) ,%store ,base ,index (immediate ,hi)
                          ,(%inline srl ,value (immediate 16))))))])]
              [(integer-40 unsigned-40)
               (constant-case unaligned-integers
                 [(#t)
                  (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                            (values (+ offset 1) offset)
                                            (values offset (+ offset 4)))])
                    (bind #t (base index value)
                      (%seq
                        (inline ,(make-info-load 'unsigned-32 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                        (inline ,(make-info-load 'unsigned-8 #f) ,%store ,base ,index (immediate ,hi)
                          ,(%inline srl ,value (immediate 32))))))])]
              [(integer-48 unsigned-48)
               (constant-case unaligned-integers
                 [(#t)
                  (let-values ([(lo hi) (if (constant-case native-endianness [(little) swapped?] [(big) (not swapped?)])
                                            (values (+ offset 2) offset)
                                            (values offset (+ offset 4)))])
                    (bind #t (base index value)
                      (%seq
                        (inline ,(make-info-load 'unsigned-32 swapped?) ,%store ,base ,index (immediate ,lo) ,value)
                        (inline ,(make-info-load 'unsigned-16 swapped?) ,%store ,base ,index (immediate ,hi)
                          ,(%inline srl ,value (immediate 32))))))])]
              [(integer-56 unsigned-56)
               (constant-case unaligned-integers
                 [(#t)
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
          (case-lambda
            [(type base offset-expr value)
             (let-values ([(index offset) (offset-expr->index+offset offset-expr)])
               (build-object-set! type base index offset value))]
            [(type base index offset value)
             (case type
               [(scheme-object) (build-dirty-store base index offset value)]
               [(double-float)
                (bind #f (base index)
                  (%seq
                    (inline ,(make-info-loadfl %flreg1) ,%load-double
                      ,value ,%zero ,(%constant flonum-data-disp))
                    (inline ,(make-info-loadfl %flreg1) ,%store-double
                      ,base ,index (immediate ,offset))))]
               [(single-float)
                (bind #f (base index)
                  (%seq
                    (inline ,(make-info-loadfl %flreg1) ,%load-double->single
                      ,value ,%zero ,(%constant flonum-data-disp))
                    (inline ,(make-info-loadfl %flreg1) ,%store-single
                      ,base ,index (immediate ,offset))))]
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
                   ,(%mref ,value ,(constant flonum-data-disp)))]
               ; 40-bit+ only on 64-bit machines
               [(integer-16 integer-24 integer-32 integer-40 integer-48 integer-56 integer-64
                 unsigned-16 unsigned-24 unsigned-32 unsigned-40 unsigned-48 unsigned-56 unsigned-64)
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
                      (%inline srl ,e (immediate ,delta)))))))
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
        (define inline-args-limit 10)
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
              (%inline eq?
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
          [(e) e]
          [e* `(values ,(make-info-call src sexpr #f #f #f) ,e* ...)])
        (define-inline 2 eq?
          [(e1 e2)
           (or (eqvop-null-fptr e1 e2)
               (relop-length RELOP= e1 e2)
               (%inline eq? ,e1 ,e2))])
        (define-inline 2 $keep-live
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
                   [(e) `(seq ,e ,(%constant strue))]
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
                 [(e) e]
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
          [(e) e]
          [(e1 e2) (%inline + ,e1 ,e2)]
          [(e1 . e*) (reduce src sexpr moi e1 e*)])
        (define-inline 3 r6rs:fx+ ; limited to two arguments
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
            [(e1 e2) (go src sexpr e1 e2)]))

        (define-inline 3 fx-
          [(e) (%inline - (immediate 0) ,e)]
          [(e1 e2) (%inline - ,e1 ,e2)]
          [(e1 . e*) (reduce src sexpr moi e1 e*)])
        (define-inline 3 r6rs:fx- ; limited to one or two arguments
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
            [(e1 e2) (go src sexpr e1 e2)]))
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
          (define fixnum-powers-of-two
            (let f ([m 2] [e 1])
              (if (<= m (constant most-positive-fixnum))
                  (cons (cons m e) (f (* m 2) (fx+ e 1)))
                  '())))
          (define-inline 3 fxdiv
            [(e1 e2)
             (nanopass-case (L7 Expr) e2
               [(quote ,d)
                (let ([a (assv d fixnum-powers-of-two)])
                  (and a
                       (%inline logand
                          ,(%inline sra ,e1 (immediate ,(cdr a)))
                          (immediate ,(- (constant fixnum-factor))))))]
               [else #f])])
          (define-inline 3 fxmod
            [(e1 e2)
             (nanopass-case (L7 Expr) e2
               [(quote ,d)
                (let ([a (assv d fixnum-powers-of-two)])
                  (and a (%inline logand ,e1 (immediate ,(fix (- d 1))))))]
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
                      [(assv n fixnum-powers-of-two) =>
                       (lambda (a) (%inline sll ,e (immediate ,(cdr a))))]
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
              [(e) e]
              [(e1 e2) (build-fx* e1 e2 #f)]
              [(e1 . e*) (reduce src sexpr moi e1 e*)])
            (define-inline 3 r6rs:fx* ; limited to two arguments
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
                [(e1 e2) (go src sexpr e1 e2)]))
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
                         (let ([a (assv d fixnum-powers-of-two)])
                           (and a (build-fx/p2 e1 (cdr a))))]
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
                         e2 #f)))]))))
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
          [(e) e]
          [(e1 e2) (bind #t (e1 e2)
                     `(if ,(%inline < ,e1 ,e2)
                          ,e1
                          ,e2))]
          [(e1 . e*) (reduce src sexpr moi e1 e*)])

        (define-inline 3 fxmax
          [(e) e]
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
          (typed-object-pred mutable-fxvector? mask-mutable-fxvector type-mutable-fxvector)
          (typed-object-pred immutable-fxvector? mask-mutable-fxvector type-immutable-fxvector)
          (typed-object-pred $inexactnum? mask-inexactnum type-inexactnum)
          (typed-object-pred $rtd-counts? mask-rtd-counts type-rtd-counts)
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
          [(e) (bind #t (e)
                 `(if ,(%type-check mask-fixnum type-fixnum ,e)
                      ,(%constant strue)
                      ,(%type-check mask-immediate type-immediate ,e)))])

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
        (define-inline 3 $closure-code
          [(e) (%inline -
                  ,(%mref ,e ,(constant closure-code-disp))
                  ,(%constant code-data-disp))])
        (define-inline 3 $code-free-count
          [(e) (build-fix (%mref ,e ,(constant code-closure-length-disp)))])
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
            [(e) e]
            [(e . e*) (go e e*)])
          (define-inline 2 cons*
            [(e) e]
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
          (inline-accessor csv7:record-type-descriptor record-type-disp)
          (inline-accessor $record-type-descriptor record-type-disp)
          (inline-accessor record-rtd record-type-disp)
          (inline-accessor $port-handler port-handler-disp)
          (inline-accessor $port-info port-info-disp)
          (inline-accessor port-name port-name-disp)
          (inline-accessor $thread-tc thread-tc-disp)
          )
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
          (def-len string-length string-type-disp string-length-offset)
          (def-len bytevector-length bytevector-type-disp bytevector-length-offset)
          (def-len $bignum-length bignum-type-disp bignum-length-offset))
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
          (def-len string-length mask-string type-string string-type-disp string-length-offset)
          (def-len bytevector-length mask-bytevector type-bytevector bytevector-type-disp bytevector-length-offset))
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
           (bind #t (e-box e-new)
             `(if ,(%typed-object-check mask-mutable-box type-mutable-box ,e-box)
                  ,(build-dirty-store e-box (constant box-ref-disp) e-new)
                  ,(build-libcall #t src sexpr set-box! e-box e-new)))])
        (define-inline 2 box-cas!
          [(e-box e-old e-new)
           (bind #t (e-box e-old e-new)
             `(if ,(%typed-object-check mask-mutable-box type-mutable-box ,e-box)
                  ,(build-dirty-store e-box %zero (constant box-ref-disp) e-new (make-build-cas e-old) build-cas-seq)
                  ,(build-libcall #t src sexpr box-cas! e-box e-old e-new)))])
        (define-inline 2 set-car!
          [(e-pair e-new)
           (bind #t (e-pair e-new)
             `(if ,(%type-check mask-pair type-pair ,e-pair)
                  ,(build-dirty-store e-pair (constant pair-car-disp) e-new)
                  ,(build-libcall #t src sexpr set-car! e-pair e-new)))])
        (define-inline 2 set-cdr!
          [(e-pair e-new)
           (bind #t (e-pair e-new)
             `(if ,(%type-check mask-pair type-pair ,e-pair)
                  ,(build-dirty-store e-pair (constant pair-cdr-disp) e-new)
                  ,(build-libcall #t src sexpr set-cdr! e-pair e-new)))])
        (define-inline 3 $set-symbol-hash!
          ; no need for dirty store---e2 should be a fixnum
          [(e1 e2) `(set! ,(%mref ,e1 ,(constant symbol-hash-disp)) ,e2)])
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
              (not (memq name '(nuate nonprocedure-code error-invoke invoke)))))
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
          (define-tc-parameter default-record-equal-procedure default-record-equal-procedure)
          (define-tc-parameter default-record-hash-procedure default-record-hash-procedure)
          )

        (define-inline 3 $install-guardian
          [(e-obj e-rep e-tconc)
           (bind #f (e-obj e-rep e-tconc)
             (bind #t ([t (%constant-alloc typemod (constant size-guardian-entry))])
               (%seq
                 (set! ,(%mref ,t ,(constant guardian-entry-obj-disp)) ,e-obj)
                 (set! ,(%mref ,t ,(constant guardian-entry-rep-disp)) ,e-rep)
                 (set! ,(%mref ,t ,(constant guardian-entry-tconc-disp)) ,e-tconc)
                 (set! ,(%mref ,t ,(constant guardian-entry-next-disp)) ,(%tc-ref guardian-entries))
                 (set! ,(%tc-ref guardian-entries) ,t))))])

        (define-inline 3 $install-ftype-guardian
          [(e-obj e-tconc)
           (bind #f (e-obj e-tconc)
             (bind #t ([t (%constant-alloc typemod (constant size-guardian-entry))])
               (%seq
                 (set! ,(%mref ,t ,(constant guardian-entry-obj-disp)) ,e-obj)
                 (set! ,(%mref ,t ,(constant guardian-entry-rep-disp)) (immediate ,(constant ftype-guardian-rep)))
                 (set! ,(%mref ,t ,(constant guardian-entry-tconc-disp)) ,e-tconc)
                 (set! ,(%mref ,t ,(constant guardian-entry-next-disp)) ,(%tc-ref guardian-entries))
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
          (define-inline 2 $raw-collect-cond
            [() `(literal ,(make-info-literal #f 'entry (lookup-c-entry raw-collect-cond) 0))]))
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
        (define-inline 2 native-endianness
          [() `(quote ,(constant native-endianness))])
        (define-inline 2 directory-separator
          [() `(quote ,(if-feature windows #\\ #\/))])
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
        (define-inline 3 eq-hashtable-ref
          [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-ref e1 e2 e3)])
        (define-inline 3 eq-hashtable-contains?
          [(e1 e2) (build-libcall #f src sexpr eq-hashtable-contains? e1 e2)])
        (define-inline 3 eq-hashtable-set!
          [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-set! e1 e2 e3)])
        (define-inline 3 eq-hashtable-update!
          [(e1 e2 e3 e4) (build-libcall #f src sexpr eq-hashtable-update! e1 e2 e3 e4)])
        (define-inline 3 eq-hashtable-cell
          [(e1 e2 e3) (build-libcall #f src sexpr eq-hashtable-cell e1 e2 e3)])
        (define-inline 3 eq-hashtable-delete!
          [(e1 e2) (build-libcall #f src sexpr eq-hashtable-delete! e1 e2)])
        (define-inline 3 symbol-hashtable-ref
          [(e1 e2 e3) (build-libcall #f src sexpr symbol-hashtable-ref e1 e2 e3)])
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
          (define eqok-help?
            (lambda (obj)
              (or (symbol? obj)
                  (char? obj)
                  (target-fixnum? obj)
                  (null? obj)
                  (boolean? obj)
                  (eqv? obj "")
                  (eqv? obj ($tc-field 'null-immutable-string ($tc)))
                  (eqv? obj '#())
                  (eqv? obj ($tc-field 'null-immutable-vector ($tc)))
                  (eqv? obj '#vu8())
                  (eqv? obj ($tc-field 'null-immutable-bytevector ($tc)))
                  (eqv? obj '#vfx())
                  (eqv? obj ($tc-field 'null-immutable-fxvector ($tc)))
                  (eq? obj (void))
                  (eof-object? obj)
                  (bwp-object? obj)
                  (eq? obj '#6=#6#)
                  ($unbound-object? obj))))
          (define eqvok-help? number?)
          (define e*ok?
            (lambda (e*ok-help?)
              (lambda (e)
                (nanopass-case (L7 Expr) e
                  [(quote ,d) (e*ok-help? d)]
                  [else #f]))))
          (define eqok? (e*ok? eqok-help?))
          (define eqvok? (e*ok? eqvok-help?))
          (define-inline 2 eqv?
            [(e1 e2) (or (eqvop-null-fptr e1 e2)
                         (relop-length RELOP= e1 e2)
                         (if (or (eqok? e1) (eqok? e2))
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
                 (%inline logand
                    ,(%inline srl
                      ,(constant-case ptr-bits
                         [(32) (%inline +
                                  ,(%mref ,e ,(constant flonum-data-disp))
                                  ,(%mref ,e ,(fx+ (constant flonum-data-disp) 4)))]
                         [(64) (%mref ,e ,(constant flonum-data-disp))])
                      (immediate 1))
                    (immediate ,(- (constant fixnum-factor)))))])
        (let ()
          (define build-flonum-extractor
            (lambda (pos size e1)
              (let ([cnt (- pos (constant fixnum-offset))]
                    [mask (* (- (expt 2 size) 1) (expt 2 (constant fixnum-offset)))])
                (%inline logand
                  ,(let ([body `(inline ,(make-info-load 'integer-32 #f) ,%load ,e1 ,%zero
                                  (immediate ,(constant-case native-endianness
                                                [(little) (fx+ (constant flonum-data-disp) 4)]
                                                [(big) (constant flonum-data-disp)])))])
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

        (define-inline 3 $fleqv?
          [(e1 e2)
           (constant-case ptr-bits
             [(32) (build-and
                     (%inline eq?
                       ,(%mref ,e1 ,(constant flonum-data-disp))
                       ,(%mref ,e2 ,(constant flonum-data-disp)))
                     (%inline eq?
                       ,(%mref ,e1 ,(fx+ (constant flonum-data-disp) 4))
                       ,(%mref ,e2 ,(fx+ (constant flonum-data-disp) 4))))]
             [(64) (%inline eq?
                     ,(%mref ,e1 ,(constant flonum-data-disp))
                     ,(%mref ,e2 ,(constant flonum-data-disp)))]
             [else ($oops 'compiler-internal
                     "$fleqv doesn't handle ptr-bits = ~s"
                     (constant ptr-bits))])])


        (let ()
          (define build-flop-1
            ; NB: e must be bound
            (lambda (op e)
              (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                `(seq (inline ,null-info ,op ,e ,t) ,t))))
          (define build-flop-2
            ; NB: e1 and e2 must be bound
            (lambda (op e1 e2)
              (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                `(seq (inline ,null-info ,op ,e1 ,e2 ,t) ,t))))
          (define build-flabs
            (lambda (e)
              (bind (constant-case ptr-bits [(32) #t] [(64) #f]) (e)
                (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                  (%seq
                    ,(constant-case ptr-bits
                       [(64)
                        `(set! ,(%mref ,t ,(constant flonum-data-disp))
                           ,(%inline logand
                              ,(%mref ,e ,(constant flonum-data-disp))
                              ,(%inline srl (immediate -1) (immediate 1))))]
                       [(32)
                        (let ()
                          (constant-case native-endianness
                            [(big)
                             (begin
                               (define disp-high (constant flonum-data-disp))
                               (define disp-low (fx+ (constant flonum-data-disp) 4)))]
                            [(little)
                             (begin
                               (define disp-low (constant flonum-data-disp))
                               (define disp-high (fx+ (constant flonum-data-disp) 4)))])
                          (%seq
                            (set! ,(%mref ,t ,disp-high)
                              ,(%inline logand
                                 ,(%mref ,e ,disp-high)
                                 ,(%inline srl (immediate -1) (immediate 1))))
                            (set! ,(%mref ,t ,disp-low)
                              ,(%mref ,e ,disp-low))))])
                    ,t)))))
          (define build-flneg
            (lambda (e)
              (bind (constant-case ptr-bits [(32) #t] [(64) #f]) (e)
                (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                  (%seq
                    ,(constant-case ptr-bits
                       [(64)
                        `(set! ,(%mref ,t ,(constant flonum-data-disp))
                           ,(%inline logxor
                              ,(%mref ,e ,(constant flonum-data-disp))
                              ,(%inline sll (immediate 1) (immediate 63))))]
                       [(32)
                        (let ()
                          (constant-case native-endianness
                            [(big)
                             (begin
                               (define disp-high (constant flonum-data-disp))
                               (define disp-low (fx+ (constant flonum-data-disp) 4)))]
                            [(little)
                             (begin
                               (define disp-low (constant flonum-data-disp))
                               (define disp-high (fx+ (constant flonum-data-disp) 4)))])
                          (%seq
                            (set! ,(%mref ,t ,disp-high)
                              ,(%inline logxor
                                 ,(%mref ,e ,disp-high)
                                 ,(%inline sll (immediate 1) (immediate 31))))
                            (set! ,(%mref ,t ,disp-low)
                              ,(%mref ,e ,disp-low))))])
                    ,t)))))

          ;; TODO: Rather then reducing here, (which will allocate a new flonum for each interim result)
          ;; we could allocate a single flonum and reuse it until the final result is calculated.
          ;; Better yet, we could do this across nested fl operations, so that only one flonum is
          ;; allocated across nested fl+, fl*, fl-, fl/ etc. operation
          (define-inline 3 fl+
            [() `(quote 0.0)]
            [(e) e]
            [(e1 e2) (bind #f (e1 e2) (build-flop-2 %fl+ e1 e2))]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 fl*
            [() `(quote 1.0)]
            [(e) e]
            [(e1 e2) (bind #f (e1 e2) (build-flop-2 %fl* e1 e2))]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 fl-
            [(e) (build-flneg e)]
            [(e1 e2) (bind #f (e1 e2) (build-flop-2 %fl- e1 e2))]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 fl/
            [(e) (bind #f (e) (build-flop-2 %fl/ `(quote 1.0) e))]
            [(e1 e2) (bind #f (e1 e2) (build-flop-2 %fl/ e1 e2))]
            [(e1 . e*) (reduce src sexpr moi e1 e*)])

          (define-inline 3 flsqrt
            [(e)
             (constant-case architecture
               [(x86 x86_64 arm32) (bind #f (e) (build-flop-1 %flsqrt e))]
               [(ppc32) #f])])

          (define-inline 3 flround
            ; NB: there is no support in SSE2 for flround, though this was added in SSE4.1
            [(e) (build-libcall #f src sexpr flround e)])

          (define-inline 3 flabs
            [(e) (build-flabs e)])

          (let ()
            (define build-fl-make-rectangular
              (lambda (e1 e2)
                (bind #f (e1 e2)
                  (bind #t ([t (%constant-alloc type-typed-object (constant size-inexactnum))])
                    `(seq
                       (set! ,(%mref ,t ,(constant inexactnum-type-disp))
                         ,(%constant type-inexactnum))
                       ,(%seq
                          (inline ,(make-info-loadfl %flreg1) ,%load-double
                            ,e1 ,%zero ,(%constant flonum-data-disp))
                          (inline ,(make-info-loadfl %flreg1) ,%store-double
                            ,t ,%zero ,(%constant inexactnum-real-disp))
                          (inline ,(make-info-loadfl %flreg1) ,%load-double
                            ,e2 ,%zero ,(%constant flonum-data-disp))
                          (inline ,(make-info-loadfl %flreg1) ,%store-double
                            ,t ,%zero ,(%constant inexactnum-imag-disp))
                          ,t))))))

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
              [(e) e]
              [(e1 e2) (build-libcall #f src sexpr cfl+ e1 e2)]
              ; TODO: add 3 argument version of cfl+ library function
              #;[(e1 e2 e3) (build-libcall #f src sexpr cfl+ e1 e2 e3)]
              [(e1 e2 . e*) #f])

            (define-inline 3 cfl*
              [() `(quote 1.0)]
              [(e) e]
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
            (define (build-fl< e1 e2) (%inline fl< ,e1 ,e2))
            (define (build-fl= e1 e2) (%inline fl= ,e1 ,e2))
            (define (build-fl<= e1 e2) (%inline fl<= ,e1 ,e2))

            (let ()
              (define-syntax define-fl-cmp-inline
                (lambda (x)
                  (syntax-case x ()
                    [(_ op r6rs:op builder inequality? swapped?)
                     (with-syntax ([(args ...) (if (datum swapped?) #'(e2 e1) #'(e1 e2))]
                                   [reducer (if (datum inequality?)
                                                #'reduce-inequality
                                                #'reduce-equality)])
                       #'(begin
                           (define-inline 3 op
                             [(e) (bind #t (e) (build-fl= e e))]
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
                   (bind #t (e1 e2)
                     `(if ,(build-and
                             (%type-check mask-flonum type-flonum ,e1)
                             (%type-check mask-flonum type-flonum ,e2))
                          ,body
                          ,(build-libcall #t src sexpr op e1 e2)))]))
              (define-syntax define-fl-cmp-inline
                (lambda (x)
                  (syntax-case x ()
                    [(_ op r6rs:op builder inequality? swapped?)
                     (with-syntax ([(args ...) (if (datum swapped?) #'(e2 e1) #'(e1 e2))])
                       #'(begin
                           (define-inline 2 op
                             [(e) #f]
                             [(e1 e2) (build-bind-and-check src sexpr op e1 e2 (builder args ...))]
                             [(e1 e2 . e*) #f])
                           (define-inline 2 r6rs:op
                             [(e1 e2) (build-bind-and-check src sexpr r6rs:op e1 e2 (builder args ...))]
                             [(e1 e2 . e*) #f])))])))

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
                [(e) (bind #f (e) (build-cfl= e e))] ; this is weird, why not just true?
                [(e1 e2) (bind #f (e1 e2) (build-cfl= e1 e2))]
                ; TODO: should we avoid building for more then the 3 item case?
                [(e1 e2 . e*) (reduce-equality src sexpr moi e1 e2 e*)])))

          (let ()
            (define build-flop-3
              ; NB: e1, e2, and e3 must be bound
              (lambda (op e1 e2 e3)
                (build-flop-2 op e1
                  (build-flop-2 op e2 e3))))
            (define build-checked-flop
              (case-lambda
                [(e k)
                 (bind #t (e)
                   `(if ,(build-flonums? (list e))
                        ,e
                        ,(k e)))]
                [(e1 e2 op k)
                 (bind #t (e1 e2)
                   `(if ,(build-flonums? (list e1 e2))
                        ,(build-flop-2 op e1 e2)
                        ,(k e1 e2)))]
                [(e1 e2 e3 op k)
                 (bind #f (e1 e2 e3)
                   `(if ,(build-flonums? (list e1 e2 e3))
                        ,(build-flop-3 op e1 e2 e3)
                        ,(k e1 e2 e3)))]))

            (define-inline 2 fl+
              [() `(quote 0.0)]
              [(e) (build-checked-flop e
                     (lambda (e)
                       (build-libcall #t src sexpr fl+ e `(quote 0.0))))]
              [(e1 e2) (build-checked-flop e1 e2 %fl+
                         (lambda (e1 e2)
                           (build-libcall #t src sexpr fl+ e1 e2)))]
              ; TODO: add 3 argument fl+ library function
              #;[(e1 e2 e3) (build-checked flop e1 e2 e3 %fl+
                            (lambda (e1 e2 e3)
                              (build-libcall #t src sexpr fl+ e1 e2 e3)))]
              [(e1 . e*) #f])

            (define-inline 2 fl*
              [() `(quote 1.0)]
              [(e) (build-checked-flop e
                     (lambda (e)
                       (build-libcall #t src sexpr fl* e `(quote 1.0))))]
              [(e1 e2) (build-checked-flop e1 e2 %fl*
                         (lambda (e1 e2)
                           (build-libcall #t src sexpr fl* e1 e2)))]
              ; TODO: add 3 argument fl* library function
              #;[(e1 e2 e3) (build-checked flop e1 e2 e3 %fl*
                            (lambda (e1 e2 e3)
                              (build-libcall #t src sexpr fl* e1 e2 e3)))]
              [(e1 . e*) #f])

            (define-inline 2 fl-
              [(e)
               (bind #t (e)
                 `(if ,(build-flonums? (list e))
                      ,(build-flneg e)
                      ,(build-libcall #t src sexpr flnegate e)))]
              [(e1 e2) (build-checked-flop e1 e2 %fl-
                         (lambda (e1 e2)
                           (build-libcall #t src sexpr fl- e1 e2)))]
              ; TODO: add 3 argument fl- library function
              #;[(e1 e2 e3) (build-checked flop e1 e2 e3 %fl-
                            (lambda (e1 e2 e3)
                              (build-libcall #t src sexpr fl- e1 e2 e3)))]
              [(e1 . e*) #f])

            (define-inline 2 fl/
              [(e) (build-checked-flop `(quote 1.0) e %fl/
                     (lambda (e1 e2)
                       (build-libcall #t src sexpr fl/ e1 e2)))]
              [(e1 e2) (build-checked-flop e1 e2 %fl/
                         (lambda (e1 e2)
                           (build-libcall #t src sexpr fl/ e1 e2)))]
              ; TODO: add 3 argument fl/ library function
              #;[(e1 e2 e3) (build-checked flop e1 e2 e3 %fl/
                            (lambda (e1 e2 e3)
                              (build-libcall #t src sexpr fl/ e1 e2 e3)))]
              [(e1 . e*) #f])))

        ; NB: assuming that we have a trunc instruction for now, will need to change to support Sparc
        (define-inline 3 flonum->fixnum
          [(e-x) (bind #f (e-x)
                   (build-fix
                     (%inline trunc ,e-x)))])
        (let ()
          (define build-fixnum->flonum
           ; NB: x must already be bound in order to ensure it is done before the flonum is allocated
            (lambda (e-x)
              (bind #t ([t (%constant-alloc type-flonum (constant size-flonum))])
                (%seq
                  ,(%inline flt ,(build-unfix e-x) ,t)
                  ,t))))
          (define-inline 3 fixnum->flonum
            [(e-x) (bind #f (e-x) (build-fixnum->flonum e-x))])
          (define-inline 2 real->flonum
            [(e-x)
             (if (constant? flonum? e-x)
                 e-x
                 (bind #t (e-x)
                   `(if ,(%type-check mask-fixnum type-fixnum ,e-x)
                        ,(build-fixnum->flonum e-x)
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
        (define-inline 2 $object-ref
          [(type base offset)
           (nanopass-case (L7 Expr) type
             [(quote ,d)
              (let ([type (filter-foreign-type d)])
                (and (memq type (record-datatype list))
                     (not (memq type '(char wchar boolean)))
                     (build-object-ref #f type base offset)))]
             [else #f])])
        (define-inline 2 $swap-object-ref
          [(type base offset)
           (nanopass-case (L7 Expr) type
             [(quote ,d)
              (let ([type (filter-foreign-type d)])
                (and (memq type (record-datatype list))
                     (not (memq type '(char wchar boolean)))
                     (build-object-ref #t type base offset)))]
             [else #f])])
        (define-inline 3 foreign-ref
          [(e-type e-addr e-offset)
           (nanopass-case (L7 Expr) e-type
             [(quote ,d)
              (let ([type (filter-foreign-type d)])
                (and (memq type (record-datatype list))
                     (not (memq type '(char wchar boolean)))
                     (bind #f (e-offset)
                       (build-object-ref #f type
                         (ptr->integer e-addr (constant ptr-bits))
                         e-offset))))]
             [else #f])])
        (define-inline 2 $object-set!
          [(type base offset value)
           (nanopass-case (L7 Expr) type
             [(quote ,d)
              (let ([type (filter-foreign-type d)])
                (and (memq type (record-datatype list))
                     (not (memq type '(char wchar boolean)))
                     (or (>= (constant ptr-bits) (type->width type)) (eq? type 'double-float))
                     (build-object-set! type base offset value)))]
             [else #f])])
        (define-inline 3 foreign-set!
          [(e-type e-addr e-offset e-value)
           (nanopass-case (L7 Expr) e-type
             [(quote ,d)
              (let ([type (filter-foreign-type d)])
                (and (memq type (record-datatype list))
                     (not (memq type '(char wchar boolean)))
                     (or (>= (constant ptr-bits) (type->width type)) (eq? type 'double-float))
                     (bind #f (e-offset e-value)
                       (build-object-set! type
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
                  raw-arg
                  (%inline + ,raw-arg ,(build-unfix fx-arg)))))
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
                   (bind #f (e-index e3)
                     `(inline ,(make-info-load ptr-type #f) ,%load
                        ,($extract-fptr-address e1)
                        ,e-index (immediate ,imm-offset))))]
                ; skip allocation and dereference of ftype-pointer for $fptr-&ref
                [(call ,info ,mdcl ,pr ,e1 ,e2 ,e3) ; e1, e2, e3 = fptr, offset, ftd
                 (guard
                   (eq? (primref-name pr) '$fptr-&ref)
                   (all-set? (prim-mask unsafe) (primref-flags pr)))
                 (build-fx+raw e2 ($extract-fptr-address e1))]
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
                      (%mref ,e3 ,(constant record-data-disp)))]
                   [else
                    (bind #f (e1)
                      (ptr->integer e2 (constant ptr-bits)))])]
                [else
                 `(inline ,(make-info-load ptr-type #f) ,%load ,(suppress-unsafe-cast e-fptr) ,%zero
                    ,(%constant record-data-disp))])))
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

            (define-fptr-ref-inline $fptr-ref-integer-24 'integer-24 #f)
            (define-fptr-ref-inline $fptr-ref-unsigned-24 'unsigned-24 #f)
            (define-fptr-ref-inline $fptr-ref-swap-integer-24 'integer-24 #t)
            (define-fptr-ref-inline $fptr-ref-swap-unsigned-24 'unsigned-24 #t)

            (define-fptr-ref-inline $fptr-ref-integer-32 'integer-32 #f)
            (define-fptr-ref-inline $fptr-ref-unsigned-32 'unsigned-32 #f)
            (define-fptr-ref-inline $fptr-ref-swap-integer-32 'integer-32 #t)
            (define-fptr-ref-inline $fptr-ref-swap-unsigned-32 'unsigned-32 #t)

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
            (define-fptr-ref-inline $fptr-ref-swap-unsigned-56 'unsigned-56 #t)

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

            (define-fptr-set!-inline #f $fptr-set-integer-24! 'integer-24 build-object-set!)
            (define-fptr-set!-inline #f $fptr-set-unsigned-24! 'unsigned-24 build-object-set!)
            (define-fptr-set!-inline #f $fptr-set-swap-integer-24! 'integer-24 build-swap-object-set!)
            (define-fptr-set!-inline #f $fptr-set-swap-unsigned-24! 'unsigned-24 build-swap-object-set!)

            (define-fptr-set!-inline #f $fptr-set-integer-32! 'integer-32 build-object-set!)
            (define-fptr-set!-inline #f $fptr-set-unsigned-32! 'unsigned-32 build-object-set!)
            (define-fptr-set!-inline #f $fptr-set-swap-integer-32! 'integer-32 build-swap-object-set!)
            (define-fptr-set!-inline #f $fptr-set-swap-unsigned-32! 'unsigned-32 build-swap-object-set!)

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
            (define-fptr-set!-inline #t $fptr-set-swap-unsigned-56! 'unsigned-56 build-swap-object-set!)

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

            (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-24 #t unsigned-24 #f)
            (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-24 #f unsigned-24 #f)
            (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-24 #t unsigned-24 #t)
            (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-24 #f unsigned-24 #t)

            (define-fptr-bits-ref-inline $fptr-ref-ibits-unsigned-32 #t unsigned-32 #f)
            (define-fptr-bits-ref-inline $fptr-ref-ubits-unsigned-32 #f unsigned-32 #f)
            (define-fptr-bits-ref-inline $fptr-ref-ibits-swap-unsigned-32 #t unsigned-32 #t)
            (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-32 #f unsigned-32 #t)

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
            (define-fptr-bits-ref-inline $fptr-ref-ubits-swap-unsigned-56 #f unsigned-56 #t)

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

            (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-24! unsigned-24 #f)
            (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-24! unsigned-24 #t)

            (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-32! unsigned-32 #f)
            (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-32! unsigned-32 #t)

            (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-40! unsigned-40 #f)
            (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-40! unsigned-40 #t)

            (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-48! unsigned-48 #f)
            (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-48! unsigned-48 #t)

            (define-fptr-bits-set-inline #f $fptr-set-bits-unsigned-56! unsigned-56 #f)
            (define-fptr-bits-set-inline #f $fptr-set-bits-swap-unsigned-56! unsigned-56 #t)

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
                                  (if ,(%inline eq? (mref ,e-base ,e-index ,imm-offset) (immediate 0))
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
          (define build-fxvector-ref-check (build-ref-check fxvector-type-disp maximum-fxvector-length fxvector-length-offset type-fxvector mask-fxvector fxvector-immutable-flag))
          (define build-fxvector-set!-check (build-ref-check fxvector-type-disp maximum-fxvector-length fxvector-length-offset type-mutable-fxvector mask-mutable-fxvector fxvector-immutable-flag))
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
                     ,(build-libcall #t src sexpr fxvector-set! e-fv e-i e-new)))])
           (define-inline 3 $fxvector-set-immutable!
             [(e-fv) ((build-set-immutable! fxvector-type-disp fxvector-immutable-flag) e-fv)])))
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
               (bind #t (e-v e-i e-new)
                 `(if ,(build-vector-set!-check e-v e-i #f)
                      ,(go e-v e-i e-new)
                      ,(build-libcall #t src sexpr vector-set! e-v e-i e-new)))])
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
               (bind #t (e-v e-i e-old e-new)
                 `(if ,(build-vector-set!-check e-v e-i #f)
                      ,(go e-v e-i e-old e-new)
                      ,(build-libcall #t src sexpr vector-cas! e-v e-i e-old e-new)))]))
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
                  ,(%constant svoid)))]))

        (let ()
          (define build-bytevector
            (lambda (e*)
              (define (find-k n)
                (let loop ([bytes (constant-case ptr-bits [(32) 4] [(64) 8])]
                           [type* (constant-case ptr-bits
                                    [(32) '(unsigned-32 unsigned-16 unsigned-8)]
                                    [(64) '(unsigned-64 unsigned-32 unsigned-16 unsigned-8)])])
                  (let ([bytes/2 (fxsrl bytes 1)])
                    (if (fx<= n bytes/2)
                        (loop bytes/2 (cdr type*))
                        (values bytes (car type*))))))
              (define (build-chunk k n e*)
                (define (build-shift e shift)
                  (if (fx= shift 0) e (%inline sll ,e (immediate ,shift))))
                (let loop ([k (constant-case native-endianness
                                [(little) (fxmin k n)]
                                [(big) k])]
                           [e* (constant-case native-endianness
                                 [(little) (reverse (if (fx<= n k) e* (list-head e* k)))]
                                 [(big) e*])]
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
            (define-bv-native-ref-inline bytevector-ieee-double-native-ref double-float))

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
            (define-bv-native-ieee-set!-inline bytevector-ieee-double-native-set! double-float))

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
                             (let-values ([(e-index imm-offset) (bv-index-offset e-offset)])
                               (build-object-ref (not (eq? (constant-value e-eness) (constant native-endianness)))
                                 'type e-bv e-index imm-offset)))])])))

            (define-bv-int-ref-inline bytevector-s16-ref integer-16 1)
            (define-bv-int-ref-inline bytevector-u16-ref unsigned-16 1)

            (define-bv-int-ref-inline bytevector-s24-ref integer-24 1)
            (define-bv-int-ref-inline bytevector-u24-ref unsigned-24 1)

            (define-bv-int-ref-inline bytevector-s32-ref integer-32 3)
            (define-bv-int-ref-inline bytevector-u32-ref unsigned-32 3)

            (define-bv-int-ref-inline bytevector-s40-ref integer-40 3)
            (define-bv-int-ref-inline bytevector-u40-ref unsigned-40 3)

            (define-bv-int-ref-inline bytevector-s48-ref integer-48 3)
            (define-bv-int-ref-inline bytevector-u48-ref unsigned-48 3)

            (define-bv-int-ref-inline bytevector-s56-ref integer-56 7)
            (define-bv-int-ref-inline bytevector-u56-ref unsigned-56 7)

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
              (lambda (e-length e-fill)
                ; NB: caller must bind e-fill
                (safe-assert (no-need-to-bind? #f e-fill))
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
                                 ,(build-string-fill t `(immediate ,bytes) e-fill))))))
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
                                   ,(build-string-fill t-str t-bytes e-fill))))))))))
            (define default-fill `(immediate ,(ptr->imm #\nul)))
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
                ,(%constant svoid))]))

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
          (define build-vector-fill
            (make-build-fill (constant ptr-bytes) (constant vector-data-disp)))
          (meta-assert (= (constant log2-ptr-bytes) (constant fixnum-offset)))
          (let ()
            (define do-make-vector
              (lambda (e-length e-fill)
                ; NB: caller must bind e-fill
                (safe-assert (no-need-to-bind? #f e-fill))
                (if (constant? (lambda (x) (and (fixnum? x) (fx<= 0 x 10000))) e-length)
                    (let ([n (constant-value e-length)])
                      (if (fx= n 0)
                          `(quote ,(vector))
                          (let ([bytes (fx* n (constant ptr-bytes))])
                            (bind #t ([t (%constant-alloc type-typed-object
                                           (fx+ (constant header-size-vector) bytes))])
                              `(seq
                                 (set! ,(%mref ,t ,(constant vector-type-disp))
                                   (immediate ,(+ (fx* n (constant vector-length-factor))
                                                  (constant type-vector))))
                                 ,(build-vector-fill t `(immediate ,bytes) e-fill))))))
                    (bind #t (e-length) ; fixnum length doubles as byte count
                      (let ([t-vec (make-tmp 'tvec)])
                        `(if ,(%inline eq? ,e-length (immediate 0))
                             (quote ,(vector))
                             (let ([,t-vec (alloc ,(make-info-alloc (constant type-typed-object) #f #f)
                                             ,(%inline logand
                                                ,(%inline + ,e-length
                                                   (immediate ,(fx+ (constant header-size-vector)
                                                                    (fx- (constant byte-alignment) 1))))
                                                (immediate ,(- (constant byte-alignment)))))])
                               (seq
                                 (set! ,(%mref ,t-vec ,(constant vector-type-disp))
                                   ,(build-type/length e-length
                                      (constant type-vector)
                                      (constant fixnum-offset)
                                      (constant vector-length-offset)))
                                 ,(build-vector-fill t-vec e-length e-fill)))))))))
            (define default-fill `(immediate ,(fix 0)))
            (define-inline 3 make-vector
              [(e-length) (do-make-vector e-length default-fill)]
              [(e-length e-fill) (bind #t (e-fill) (do-make-vector e-length e-fill))])
            (let ()
              (define (valid-length? e-length)
                (constant?
                  (lambda (x) (and (target-fixnum? x) (>= x 0)))
                  e-length))
              (define-inline 2 make-vector
                [(e-length)
                 (and (valid-length? e-length)
                      (do-make-vector e-length default-fill))]
                [(e-length e-fill)
                 (and (valid-length? e-length)
                      (constant? fixnum? e-fill)
                      (do-make-vector e-length e-fill))]))))

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

        (define-inline 2 $continuation?
          [(e)
           (bind #t (e)
             (build-and
               (%type-check mask-closure type-closure ,e)
               (%type-check mask-continuation-code type-continuation-code
                 ,(%mref
                    ,(%inline -
                      ,(%mref ,e ,(constant closure-code-disp))
                      ,(%constant code-data-disp))
                    ,(constant code-type-disp)))))])
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
        (define-inline 3 $continuation-return-code
          [(e)
           (bind #t ([t (%inline +
                           ,(%mref ,e ,(constant continuation-return-address-disp))
                           ,(%constant return-address-toplink-disp))])
             (%inline - ,t ,(%mref ,t 0)))])
        (define-inline 3 $continuation-return-offset
          [(e)
           (build-fix
             (%inline -
                ,(%mref
                  ,(%mref ,e ,(constant continuation-return-address-disp))
                  ,(constant return-address-toplink-disp))
                ,(%constant return-address-toplink-disp)))])
        (define-inline 3 $continuation-return-livemask
          [(e)
           (%mref
              ,(%mref ,e ,(constant continuation-return-address-disp))
              ,(constant return-address-livemask-disp))])
        (define-inline 3 $continuation-stack-ref
          [(e-k e-i)
           (%mref
              ,(%mref ,e-k ,(constant continuation-stack-disp))
              ,(translate e-i (constant fixnum-offset) (constant log2-ptr-bytes))
              0)])
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
            (lambda (e e-rtd)
              (bind #t (e)
                (bind #f (e-rtd)
                  (build-and
                    (%type-check mask-typed-object type-typed-object ,e)
                    (%inline eq?
                      ,(%mref ,e ,(constant typed-object-type-disp))
                      ,e-rtd))))))
          (define build-unsealed-isa?
            (lambda (e e-rtd)
              (let ([t (make-assigned-tmp 't)] [Ltop (make-local-label 'Ltop)])
                (bind #t (e e-rtd)
                  (build-and
                    (%type-check mask-typed-object type-typed-object ,e)
                    `(let ([,t ,(%mref ,e ,(constant typed-object-type-disp))])
                       ,(build-simple-or
                          (%inline eq? ,t ,e-rtd)
                          (build-and
                            (%type-check mask-record type-record ,t)
                            `(label ,Ltop
                               (seq
                                 (set! ,t ,(%mref ,t ,(constant record-type-parent-disp)))
                                 ,(build-simple-or
                                    (%inline eq? ,t ,e-rtd)
                                    `(if ,(%inline eq? ,t ,(%constant sfalse))
                                         ,(%constant sfalse)
                                         (goto ,Ltop)))))))))))))
          (define-inline 3 record?
            [(e) (build-record? e)]
            [(e e-rtd)
             (if (constant? (lambda (x)
                              (and (record-type-descriptor? x)
                                   (record-type-sealed? x)))
                   e-rtd)
                 (build-sealed-isa? e e-rtd)
                 (build-unsealed-isa? e e-rtd))])
          (define-inline 2 r6rs:record?
            [(e) (build-record? e)])
          (define-inline 2 record?
            [(e) (build-record? e)]
            [(e e-rtd)
             (nanopass-case (L7 Expr) e-rtd
               [(quote ,d)
                (and (record-type-descriptor? d)
                     (if (record-type-sealed? d)
                         (build-sealed-isa? e e-rtd)
                         (build-unsealed-isa? e e-rtd)))]
               [else #f])])
          (define-inline 2 $sealed-record?
            [(e e-rtd) (build-sealed-isa? e e-rtd)])
          (define-inline 2 eq-hashtable?
            [(e) (let ([rtd (let () (include "hashtable-types.ss") (record-type-descriptor eq-ht))])
                   (let ([e-rtd `(quote ,rtd)])
                     (if (record-type-sealed? rtd)
                         (build-sealed-isa? e e-rtd)
                         (build-unsealed-isa? e e-rtd))))]))
        (define-inline 2 gensym?
          [(e)
           (bind #t (e)
             (build-and
               (%type-check mask-symbol type-symbol ,e)
               (bind #t ([t (%mref ,e ,(constant symbol-name-disp))])
                 `(if ,t
                      ,(%type-check mask-pair type-pair ,t)
                      ,(%constant strue)))))])
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
          (define-inline 3 gensym
            [() (build-make-symbol (%constant sfalse))]
            [(e-pname) (bind #f (e-pname) (go e-pname))]
            [(e-pname e-uname) #f])
          (define-inline 2 gensym
            [() (build-make-symbol (%constant sfalse))]
            [(e-pname) (and (constant? string? e-pname) (go e-pname))]
            [(e-pname e-uname) #f]))
        (define-inline 3 symbol->string
          [(e-sym)
           (bind #t (e-sym)
             (bind #t ([e-name (%mref ,e-sym ,(constant symbol-name-disp))])
               `(if ,e-name
                    (if ,(%type-check mask-pair type-pair ,e-name)
                        ,(%mref ,e-name ,(constant pair-cdr-disp))
                        ,e-name)
                    ,(%primcall #f sexpr $gensym->pretty-name ,e-sym))))])
        (define-inline 3 $fxaddress
          [(e) (%inline logand
                  ,(let ([n (- (log2 (constant typemod)) (constant fixnum-offset))])
                     (if (> n 0) (%inline sra ,e (immediate ,n)) e))
                  (immediate ,(- (constant fixnum-factor))))])
        (define-inline 3 $set-timer
          [(e) (bind #f (e)
                 (bind #t ([t (build-fix (ref-reg %trap))])
                   `(seq
                      (set! ,(ref-reg %trap) ,(build-unfix e))
                      ,t)))])
        (define-inline 3 directory-separator?
          [(e) (if-feature windows
                 (bind #t (e)
                   (build-simple-or
                     (%inline eq? ,e (immediate ,(ptr->imm #\/)))
                     (%inline eq? ,e (immediate ,(ptr->imm #\\)))))
                 (%inline eq? ,e (immediate ,(ptr->imm #\/))))])
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
             [(arm32) (unsigned->ptr (%inline read-time-stamp-counter) 32)]
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
             [(arm32 ppc32) (unsigned->ptr (%inline read-performance-monitoring-counter ,(build-unfix e)) 32)])])

    )) ; expand-primitives module

    (define-pass np-place-overflow-and-trap : L9 (ir) -> L9.5 ()
      (definitions
        (define repeat? #f)
        (define update-label!
          (lambda (l oc tc)
            (let ([orig-oc (local-label-overflow-check l)]
                  [orig-tc (local-label-trap-check l)])
              (unless (and (eq? oc orig-oc) (eq? tc orig-tc))
                (set! repeat? #t)
                (local-label-overflow-check-set! l oc)
                (local-label-trap-check-set! l tc)))))
        (define combine-seq
          (lambda (x y)
            (case x
              [(no) y]
              [(yes) 'yes]
              [else (if (eq? y 'no) 'maybe 'yes)])))
        (define-pass strip-redundant-overflow-and-trap : (L9.5 Expr) (ir) -> (L9.5 Expr) ()
          (definitions
            (define-record-type goto (nongenerative) (fields label oc? tc?))
            (define goto* '())
            (define well-behaved-goto?
              (lambda (goto)
                (and (or (goto-oc? goto) (not (local-label-overflow-check (goto-label goto))))
                     (or (goto-tc? goto) (not (local-label-trap-check (goto-label goto))))))))
          (Lvalue : Lvalue (ir oc? tc?) -> Lvalue ()
            [(mref ,[e0] ,[e1] ,imm) `(mref ,e0 ,e1 ,imm)])
          (Expr : Expr (ir oc? tc?) -> Expr ()
            [(overflow-check ,[e #t tc? -> e]) (if oc? e `(overflow-check ,e))]
            [(trap-check ,ioc ,[e oc? #t -> e]) (if tc? e `(trap-check ,(if oc? #f ioc) ,e))]
            [(call ,info ,mdcl (literal ,info0) ,[e*] ...)
             (guard oc? (eq? (info-literal-type info0) 'library)
               (libspec-does-not-expect-headroom? (info-literal-addr info0)))
             `(call ,info ,mdcl
                (literal ,(make-info-literal #f 'library
                            (libspec->headroom-libspec (info-literal-addr info0))
                            0))
                ,e* ...)]
            [(loop ,x (,x* ...) ,[body oc? #f -> body]) `(loop ,x (,x* ...) ,body)]
            [(label ,l ,[body])
             (local-label-overflow-check-set! l (and (not (eq? (local-label-overflow-check l) 'no)) oc?))
             (local-label-trap-check-set! l (and (not (eq? (local-label-trap-check l) 'no)) tc?))
             `(label ,l ,body)]
            [(goto ,l) (set! goto* (cons (make-goto l oc? tc?) goto*)) ir])
          (let ([ir (Expr ir #f #f)])
            (and (andmap well-behaved-goto? goto*) ir)))
        (define-pass insert-loop-traps : (L9 Expr) (ir) -> (L9.5 Expr) ()
          (Expr : Expr (ir) -> Expr ()
            [(loop ,x (,x* ...) ,[body]) `(loop ,x (,x* ...) (trap-check #f ,body))]))
        (define has-no-headroom-libcall?
          (lambda (e?)
            (and e?
                 (nanopass-case (L9.5 Expr) e?
                   [(literal ,info)
                    (and (eq? (info-literal-type info) 'library)
                         (libspec-has-does-not-expect-headroom-version? (info-literal-addr info))
                         info)]
                   [else #f]))))
        (with-output-language (L9.5 Expr)
          (define request-trap-check (if (generate-interrupt-trap) 'yes 'no))
          (define add-trap-check
            (lambda (overflow? e)
              (if (eq? request-trap-check 'yes)
                  `(trap-check ,overflow? ,e)
                  e)))))
      (Lvalue : Lvalue (ir) -> Lvalue ('no 'no)
        [(mref ,[e0 #f -> e0 oc0 tc0] ,[e1 #f -> e1 oc1 tc1] ,imm)
         (values `(mref ,e0 ,e1 ,imm) (combine-seq oc0 oc1) (combine-seq tc0 tc1))])
      (Expr : Expr (ir tail?) -> Expr ('no 'no)
        [(goto ,l)
         (if (local-label? l)
             (values `(goto ,l) (local-label-overflow-check l) (local-label-trap-check l))
             (values `(goto ,l) 'no 'no))]
        [(values ,info ,[e* #f -> e* oc* tc*] ...)
         (values `(values ,info ,e* ...) (fold-left combine-seq 'no oc*) (fold-left combine-seq 'no tc*))]
        [(call ,info ,mdcl ,x ,[e* #f -> e* oc* tc*] ...)
         (guard (uvar? x) (eq? (uvar-location x) 'loop))
         (values `(call ,info ,mdcl ,x ,e* ...) (fold-left combine-seq 'no oc*) request-trap-check)]
        [(call ,info ,mdcl ,e? ,[e* #f -> e* oc* tc*] ...)
         (let-values ([(e? oc tc) (if e? (Expr e? #f) (values e? 'no 'no))])
           ; to save code space, we skip trap check for error calls under assumption trap checks will
           ; be made by the error handler.  if not, could get a uninterruptible hard loop...c'est la vie
           (define wrap-tc
             (lambda (overflow? call)
               (if (and (info-call-error? info)
                        (eq? (fold-left combine-seq tc tc*) 'no))
                   call
                   (add-trap-check overflow? call))))
           (let ([noc? (eq? (fold-left combine-seq oc oc*) 'no)])
             (cond
               [(and (or tail? (and (info-call-error? info) (fx< (debug-level) 2))) noc?)
                (let ([call `(call ,info ,mdcl ,e? ,e* ...)])
                  (if (info-call-pariah? info)
                      (values (wrap-tc #t call) 'no 'no)
                      (values call 'no request-trap-check)))]
               [(and noc? (has-no-headroom-libcall? e?)) =>
                (lambda (info0)
                  (safe-assert (not (libspec-does-not-expect-headroom? (info-literal-addr info0))))
                  (let ([call `(call ,info ,mdcl
                                 (literal ,(make-info-literal #f 'library
                                             (libspec->does-not-expect-headroom-libspec (info-literal-addr info0))
                                             0))
                                 ,e* ...)])
                    (if (info-call-pariah? info)
                        (values (wrap-tc #t call) 'no 'no)
                        (values call 'no request-trap-check))))]
               [else (let ([call `(call ,info ,mdcl ,e? ,e* ...)])
                       (if (info-call-pariah? info)
                           (values `(overflow-check ,(wrap-tc #f call)) 'no 'no)
                           (values call 'yes request-trap-check)))])))]
        [(inline ,info ,prim ,[e* #f -> e* oc* tc*] ...)
         (values `(inline ,info ,prim ,e* ...) (fold-left combine-seq 'no oc*) (fold-left combine-seq 'no tc*))]
        [(alloc ,info ,[e #f -> e oc tc]) (values `(alloc ,info ,e) oc tc)]
        [(loop ,x (,x* ...) ,body)
         (uvar-location-set! x 'loop)
         (let-values ([(body oc tc) (Expr body tail?)])
           (uvar-location-set! x #f)
           (values
             (if (eq? tc 'yes)
                 `(loop ,x (,x* ...) ,(add-trap-check #t body))
                 `(loop ,x (,x* ...) ,body))
             (if (eq? oc 'no) 'no 'yes)
             'no))]
        [(foreign-call ,info ,[e #f -> e oc tc] ,[e* #f -> e* oc* tc*] ...)
         (values `(foreign-call ,info ,e ,e* ...) (fold-left combine-seq oc oc*) (fold-left combine-seq tc tc*))]
        [(label ,l ,[body oc tc]) (update-label! l oc tc) (values `(label ,l ,body) oc tc)]
        [(set! ,[lvalue -> lvalue oc0 tc0] ,[e #f -> e oc1 tc1])
         (values `(set! ,lvalue ,e) (combine-seq oc0 oc1) (combine-seq tc0 tc1))]
        [(mvlet ,[e #f -> e oc tc] ((,x** ...) ,interface* ,[body* oc* tc*]) ...)
        ; claiming mvlet always makes a nontail call
         (values `(mvlet ,e ((,x** ...) ,interface* ,body*) ...) 'yes request-trap-check)]
        [(mvcall ,info ,[e1 #f -> e1 oc1 tc1] ,[e2 #f -> e2 oc2 tc2])
        ; claiming mvcall always makes a nontail call
         (values `(mvcall ,info ,e1 ,e2) 'yes request-trap-check)]
        [(let ([,x* ,[e* #f -> e* oc* tc*]] ...) ,[body oc tc])
         (values `(let ([,x* ,e*] ...) ,body) (fold-left combine-seq oc oc*) (fold-left combine-seq tc tc*))]
        [(if ,[e0 #f -> e0 oc0 tc0] ,[e1 oc1 tc1] ,[e2 oc2 tc2])
         (define combine-branch
           (lambda (l r)
             (case l
               [(yes) (if (eq? r 'yes) 'yes 'maybe)]
               [(no) (if (eq? r 'no) 'no 'maybe)]
               [else l])))
         (let ([oc (combine-seq oc0 (combine-branch oc1 oc2))]
               [tc (combine-seq tc0 (combine-branch tc1 tc2))])
           (define wrap-oc
             (lambda (ocx e)
               (if (and (eq? ocx 'yes) (not (eq? oc 'yes)))
                   `(overflow-check ,e)
                   e)))
           (define wrap-tc
             (lambda (tcx e)
               (if (and (eq? tcx 'yes) (not (eq? tc 'yes)))
                   (add-trap-check #t e)
                   e)))
           (values
             `(if ,e0 ,(wrap-oc oc1 (wrap-tc tc1 e1)) ,(wrap-oc oc2 (wrap-tc tc2 e2)))
             oc tc))]
        [(seq ,[e0 #f -> e0 oc0 tc0] ,[e1 oc1 tc1])
         (values `(seq ,e0 ,e1) (combine-seq oc0 oc1) (combine-seq tc0 tc1))])
      (CaseLambdaClause : CaseLambdaClause (ir force-overflow?) -> CaseLambdaClause ()
        [(clause (,x* ...) ,mcp ,interface ,body)
         (safe-assert (not repeat?)) ; should always be initialized and/or reset to #f
         `(clause (,x* ...) ,mcp ,interface
            ,(or (let f ()
                   (let-values ([(body oc tc) (Expr body #t)])
                     (if repeat?
                         (begin (set! repeat? #f) (f))
                         (strip-redundant-overflow-and-trap
                           (let ([body (if (eq? tc 'yes) (add-trap-check #t body) body)])
                             (if (or force-overflow? (eq? oc 'yes))
                                 `(overflow-check ,body)
                                 body))))))
                 ; punting badly here under assumption that we currently can't even generate
                 ; misbehaved gotos, i.e., paths ending in a goto that don't do an overflow
                 ; or trap check where the target label expects it to have been done.  if we
                 ; ever violate this assumption on a regular basis, might want to revisit and
                 ; do something better.
                 ; ... test punt case by commenting out above for all but library.ss
                 `(overflow-check (trap-check #f ,(insert-loop-traps body)))))])
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(case-lambda ,info ,[cl* (let ([libspec (info-lambda-libspec info)])
                                    (and libspec (libspec-does-not-expect-headroom? libspec))) -> cl*] ...)
         `(case-lambda ,info ,cl* ...)]))

    (define-pass np-rebind-on-ruined-path : L9.5 (ir) -> L9.5 ()
      (definitions
        (define prefix*)
        (define add-prefix!
          (lambda (x)
            (when (uvar? x)
              (unless (uvar-in-prefix? x)
                (uvar-in-prefix! x #t)
                (set! prefix* (cons x prefix*))))))
        (define add-prefix*! (lambda (x*) (for-each add-prefix! x*)))
        (define reset-prefix*!
          (lambda (orig-prefix*)
            (let loop ([ls prefix*] [diff* '()])
              (if (eq? ls orig-prefix*)
                  (begin (set! prefix* ls) diff*)
                  (let ([x (car ls)])
                    (uvar-in-prefix! x #f)
                    (loop (cdr ls) (cons x diff*)))))))
        (define-pass gather-refs : (L9.5 Expr) (e) -> (L9.5 Expr) (x*)
          (definitions (define x*))
          (Expr : Expr (ir) -> Expr ()
            [,x (guard (uvar? x))
             (cond
               [(uvar-in-prefix? x)
                (let ([t (make-tmp 't)])
                  (uvar-location-set! x t)
                  (uvar-in-prefix! x #f)
                  (set! x* (cons x x*))
                  t)]
               [(uvar-location x)]
               [else x])])
          (fluid-let ([x* '()])
            (let ([e (Expr e)])
              (values e x*)))))
      (Expr : Expr (ir) -> Expr ()
        [(overflow-check (call ,info ,mdcl ,e? ,e* ...))
         (guard (info-call-error? info))
         `(overflow-check (call ,info ,mdcl ,e? ,e* ...))]
        [(overflow-check ,e)
         (if (null? prefix*)
             `(overflow-check ,e)
             (let-values ([(e x*) (gather-refs e)])
               (let ([t* (map (lambda (x)
                                (let ([t (uvar-location x)])
                                  (uvar-location-set! x #f)
                                  t))
                           x*)])
                 `(let ([,t* ,x*] ...) (overflow-check ,e)))))]
        [(set! ,x ,[e])
         (guard (and (uvar? x) (not (uvar-assigned? x))))
         (add-prefix! x)
         `(set! ,x ,e)]
        [(let ([,x* ,[e*]] ...) ,body)
         (add-prefix*! x*)
         `(let ([,x* ,e*] ...) ,(Expr body))]
        [(if ,[e0] ,e1 ,e2)
         (let ([orig-prefix* prefix*])
           (let ([e1 (Expr e1)])
             (let ([e1-diff-prefix* (reset-prefix*! orig-prefix*)])
               (let ([e2 (Expr e2)])
                 (add-prefix*! e1-diff-prefix*)
                 `(if ,e0 ,e1 ,e2)))))]
        [(seq ,[e0] ,e1) `(seq ,e0 ,(Expr e1))])
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) ,mcp ,interface ,body)
         (fluid-let ([prefix* x*])
           `(clause (,x* ...) ,mcp ,interface ,(Expr body)))]))


    (define-pass np-finalize-loops : L9.5 (ir) -> L9.75 ()
      (Expr : Expr (ir) -> Expr ()
        [(loop ,x (,x* ...) ,body)
         (let ([Ltop (make-local-label (uvar-name x))])
           (uvar-location-set! x (cons Ltop x*))
           (let ([body (Expr body)])
             (uvar-location-set! x #f)
             `(label ,Ltop ,body)))]
        [(call ,info ,mdcl ,x ,[e*] ...)
         (guard (uvar-location x))
         (let ([Ltop.x* (uvar-location x)])
           (fold-left (lambda (body x e) `(seq (set! ,x ,e) ,body))
             `(goto ,(car Ltop.x*)) (cdr Ltop.x*) e*))]))

    (define-pass np-optimize-pred-in-value : L9.75 (ir) -> L9.75 ()
      (definitions
        (define bar
          (lambda (e bool?)
            (if (eq? bool? 'wrapper)
                (with-output-language (L9.75 Expr)
                  `(if ,e ,(%constant strue) ,(%constant sfalse)))
                e)))
        (define dont
          (lambda (e)
            (with-values (Expr e #f) (lambda (e bool?) e)))))
      (Value : Expr (ir) -> Expr ()
        [else (with-values (Expr ir 'value) bar)])
      (Lvalue : Lvalue (ir) -> Expr (#f))
      (Expr : Expr (ir [value? #f]) -> Expr (#f)
        [(immediate ,imm) (values ir (or (eq? imm (constant strue)) (eq? imm (constant sfalse))))]
        [(set! ,lvalue ,[e]) (values `(set! ,lvalue ,e) #f)]
        [(seq ,[dont : e0] ,[e1 bool?]) (values `(seq ,e0 ,e1) bool?)]
        [(let ([,x* ,[e*]] ...) ,[e bool?]) (values `(let ([,x* ,e*] ...) ,e) bool?)]
        [(inline ,info ,prim ,[e*] ...)
         (guard (pred-primitive? prim))
         (values `(inline ,info ,prim ,e* ...) #t)]
        [(if ,[dont : e0] ,[e1 bool1?] ,[e2 bool2?])
         (guard value?)
         (if (and bool1? bool2?)
             (values `(if ,e0 ,e1 ,e2) 'wrapper)
             (values `(if ,e0 ,(bar e1 bool1?) ,(bar e2 bool2?)) #f))]))

    (define-pass np-remove-complex-opera* : L9.75 (ir) -> L10 ()
     ; remove-complex-opera* cannot assume that assigned uvars and
     ; (mrefs at this point) are immutable.  it must take this into
     ; account and avoid possible interleaved subexpression evaluation
     ; for calls and inline forms.  it can do so by removing all lvalues
     ; as call/inline subexpressions, or it can be more selective and
     ; allow them to remain when doing so can't cause any problems.
     ; for example, (<lvalue1> <lvalue2>) can be left alone, and both
     ;
     ; ((begin e <lvalue1>) <lvalue2>) => (begin e (<lvalue1> <lvalue2>))
     ;
     ; and
     ;
     ; (<lvalue1> (begin e <lvalue2>)) => (begin e (<lvalue1> <lvalue2>))
     ;
     ; are safe transformations, but
     ;
     ; ((begin e1 <lvalue1>) (begin e2 <lvalue2>))
     ;
     ; cannot be turned into
     ;
     ; (begin e1 e2 (<lvalue1> <lvalue2>)).
     ;
     ; NB: remove-complex-opera* produces set! forms rather than let bindings
     ; since the former (but not the latter) can be pushed into both branches
     ; of an if without causing potentially exponential code growth
      (definitions
        (define local*)
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-tmp))
            (let ([x (make-tmp x)])
              (set! local* (cons x local*))
              x)))
        (define Ref
          (lambda (ir setup*)
            (if (var? ir)
                (values ir setup*)
                (let ([tmp (make-tmp 't)])
                  (values tmp (cons (Rhs ir tmp) setup*))))))
        (define Lvalue?
          (lambda (x)
            (nanopass-case (L10 Triv) x
              [,lvalue #t]
              [else #f])))
        (define Triv*
          (lambda (e* k)
            (let f ([e* e*] [lvalue-setup* '()] [rt* '()] [setup* '()])
              (if (null? e*)
                  (build-seq* setup*
                    (build-seq* lvalue-setup*
                      (k (reverse rt*))))
                  (let-values ([(t t-setup*) (Triv (car e*) (null? lvalue-setup*))])
                    (if (and (null? lvalue-setup*)
                             (not (null? t-setup*))
                             (Lvalue? t)
                             ; uvar's are singly assigned
                             (or (not (uvar? t)) (uvar-assigned? t)))
                        (f (cdr e*) t-setup* (cons t rt*) setup*)
                        (f (cdr e*) lvalue-setup* (cons t rt*) (append t-setup* setup*))))))))
        (define build-seq* (lambda (x* y) (fold-right build-seq y x*)))
        (with-output-language (L10 Expr)
          (define build-seq (lambda (x y) `(seq ,x ,y)))
          (define Rhs
            (lambda (ir lvalue)
              (Expr ir
                (lambda (e)
                  (nanopass-case (L10 Expr) e
                    [,rhs `(set! ,lvalue ,rhs)]
                    [(values ,info ,t) `(set! ,lvalue ,t)]
                    [(values ,info ,t* ...)
                    ; sets lvalue to void. otherwise, the lvalue we entered with (which
                    ; might be referenced downstream) is never set and hence fails in the live
                    ; analysis where it is live all the way out of the function.
                     `(seq
                        (call ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #t #t) #f
                          (literal ,(make-info-literal #t 'object '$oops (constant symbol-value-disp)))
                          ,(%constant sfalse)
                          (literal ,(make-info-literal #f 'object
                                      (format "returned ~r values to single value return context"
                                        (length t*)) 0)))
                        (set! ,lvalue ,(%constant svoid)))]
                    [else (sorry! who "unexpected Rhs expression ~s" e)])))))))
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) ,mcp ,interface ,body)
         (fluid-let ([local* '()])
           (let ([body (Expr body values)])
             (safe-assert (nodups x* local*))
             `(clause (,x* ...) (,local* ...) ,mcp ,interface
                ,body)))])
      (Triv : Expr (ir lvalue-okay?) -> Triv (setup*)
        [,x
         (guard (or lvalue-okay? (and (uvar? x) (not (uvar-assigned? x))) (eq? x %zero)))
         (values x '())]
        [(mref ,e1 ,e2 ,imm)
         (guard lvalue-okay?)
         (let*-values ([(x1 setup*) (Ref e1 '())] [(x2 setup*) (Ref e2 setup*)])
           (values (%mref ,x1 ,x2 ,imm) setup*))]
        [(literal ,info) (values `(literal ,info) '())]
        [(immediate ,imm) (values `(immediate ,imm) '())]
        [(label-ref ,l ,offset) (values `(label-ref ,l ,offset) '())]
        [(let ([,x* ,e*] ...) ,[t setup*])
         (set! local* (append x* local*))
         (safe-assert (nodups local*))
         (values t
           (fold-right
             (lambda (ir lvalue setup*) (cons (Rhs ir lvalue) setup*))
             setup* e* x*))]
        [(seq ,[Expr : e0 values -> e0] ,[t setup*])
         (values t (cons e0 setup*))]
        [(pariah) (values (%constant svoid) (list (with-output-language (L10 Expr) `(pariah))))]
        [else
         (let ([tmp (make-tmp 't)])
           (values tmp (list (Rhs ir tmp))))])
      (Expr : Expr (ir k) -> Expr ()
        [(inline ,info ,prim ,e1* ...)
         (Triv* e1*
           (lambda (t1*)
             (k `(inline ,info ,prim ,t1* ...))))]
        [(alloc ,info ,e)
         (let-values ([(t setup*) (Triv e #t)])
           (build-seq* setup* (k `(alloc ,info ,t))))]
        [(call ,info ,mdcl ,e0? ,e1* ...)
         (if e0?
             (Triv* (cons e0? e1*) (lambda (t*) (k `(call ,info ,mdcl ,(car t*) ,(cdr t*) ...))))
             (Triv* e1* (lambda (t*) (k `(call ,info ,mdcl #f ,t* ...)))))]
        [(foreign-call ,info ,e0 ,e1* ...)
         (Triv* (cons e0 e1*)
           (lambda (t*)
             (k `(foreign-call ,info ,(car t*) ,(cdr t*) ...))))]
        [(values ,info ,e* ...)
         (Triv* e*
           (lambda (t*)
             (k `(values ,info ,t* ...))))]
        [(if ,[Expr : e0 values -> e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
        [(seq ,[Expr : e0 values -> e0] ,[e1]) `(seq ,e0 ,e1)]
        [(set! ,lvalue ,e)
         (let-values ([(lvalue setup*) (Triv lvalue #t)])
           ; must put lvalue setup* first to avoid potentially interleaved argument
           ; evaluation in, e.g.:
           ;
           ;  (let ([p1 (cons 0 1)] [p2 (cons 0 2)])
           ;    (let ([x (cons 0 3)])
           ;      (set-car!
           ;        (begin (set-car! x p1) (car x))
           ;        (begin (set-car! x p2) (car x)))
           ;      (eq? (car p1) p2)))
           ;  ; after expand-primitives (essentially):
           ;   => (let ([p1 (cons 0 1)] [p2 (cons 0 2)])
           ;        (let ([x (cons 0 3)])
           ;          (set!
           ;            ,(%mref (begin (set! ,(%mref x 0) p1) ,(%mref x 0)) 0)
           ;            (begin (set! ,(%mref x 0) p2) ,(%mref x 0)))
           ;          (eq? ,(%mref p1 0) p2)))
           ;  ; okay:
           ;   => (let ([p1 (cons 0 1)] [p2 (cons 0 2)])
           ;        (let ([x (cons 0 3)])
           ;         ; setup* for lvalue:
           ;          (set! ,(%mref x 0) p1)
           ;          (set! t ,(%mref x 0))
           ;         ; setup* for e
           ;          (set! ,(%mref x 0) p2)
           ;          (set!  ,(%mref t 0) ,(%mref x 0))
           ;          (eq? ,(%mref p1 0) p2)))
           ;  ; not okay:
           ;   => (let ([p1 (cons 0 1)] [p2 (cons 0 2)])
           ;        (let ([x (cons 0 3)])
           ;         ; setup* for e
           ;          (set! ,(%mref x 0) p2)
           ;         ; setup* for lvalue:
           ;          (set! ,(%mref x 0) p1)
           ;          (set! t ,(%mref x 0))
           ;          (set!
           ;            ,(%mref t 0)
           ;           ; wrong x[0]
           ;            ,(%mref x 0))
           ;          (eq? ,(%mref p1 0) p2)))
           (build-seq* setup*
             `(seq
                ,(Rhs e lvalue)
                ,(k (%constant svoid)))))]
        [(let ([,x* ,e*] ...) ,[body])
         (set! local* (append x* local*))
         (safe-assert (nodups local*))
         (fold-left (lambda (t x e) (build-seq (Rhs e x) t)) body x* e*)]
        [(mvlet ,[Expr : e values -> e] ((,x** ...) ,interface* ,[body*]) ...)
         (set! local* (append (apply append x**) local*))
         (safe-assert (nodups local*))
         `(mvlet ,e ((,x** ...) ,interface* ,body*) ...)]
        [(mvcall ,info ,[Expr : e1 values -> e1] ,e2)
         (let-values ([(t2 setup*) (Triv e2 #t)])
           (build-seq* setup* (k `(mvcall ,info ,e1 ,t2))))]
        [(goto ,l) `(goto ,l)]
        [(label ,l ,[body]) `(label ,l ,body)]
        [(trap-check ,ioc ,[body]) `(trap-check ,ioc ,body)]
        [(overflow-check ,[body]) `(overflow-check ,body)]
        [(pariah) `(pariah)]
        [(profile ,src) `(profile ,src)]
        [else
         (let-values ([(t setup*) (Triv ir #t)])
           (build-seq* setup* (k t)))]))

    (define-pass np-push-mrvs : L10 (ir) -> L10.5 ()
      (definitions
        (define local*)
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-tmp))
            (let ([x (make-tmp x)])
              (set! local* (cons x local*))
              x)))
        (define Mvcall
          (lambda (info e consumer k)
            (with-output-language (L10.5 Expr)
              (nanopass-case (L10.5 Expr) e
                [,t (k `(mvcall ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #f #f) #f ,consumer ,t ()))]
                [(values ,info2 ,t* ...)
                 (k `(mvcall ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #f #f) #f ,consumer ,t* ... ()))]
                [(mvcall ,info ,mdcl ,t0 ,t1* ... (,t* ...))
                 (k `(mvcall ,info ,mdcl ,t0 ,t1* ... (,t* ... ,consumer)))]
                [(if ,e0 ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
                [(seq ,e0 ,[e1]) `(seq ,e0 ,e1)]
                [(mlabel ,[e] (,l* ,[e*]) ...) `(mlabel ,e (,l* ,e*) ...)]
                [(label ,l ,[body]) `(label ,l ,body)]
                [(trap-check ,ioc ,[body]) `(trap-check ,ioc ,body)]
                [(overflow-check ,[body]) `(overflow-check ,body)]
                [(pariah) `(pariah)]
                [(profile ,src) `(profile ,src)]
                [(goto ,l) `(goto ,l)]
                [,rhs ; alloc, inline, foreign-call
                 (let ([tmp (make-tmp 't)])
                   `(seq
                      (set! ,tmp ,rhs)
                      ,(k `(mvcall ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #f #f) #f ,consumer ,tmp ()))))]
                [else ; set! & mvset
                 `(seq ,e ,(k `(mvcall ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #f #f) #f ,consumer ,(%constant svoid) ())))])))))
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) (,local0* ...) ,mcp ,interface ,body)
         (fluid-let ([local* local0*])
           (let ([body (Expr body)])
             (safe-assert (nodups x* local*))
             `(clause (,x* ...) (,local* ...) ,mcp ,interface
                ,body)))])
      (Rhs : Rhs (ir) -> Rhs ()
        [(call ,info ,mdcl ,[t0?] ,[t1*] ...) `(mvcall ,info ,mdcl ,t0? ,t1* ... ())])
      (Expr : Expr (ir) -> Expr ()
        [(mvcall ,info ,[e] ,[t]) (Mvcall info e t values)]
        [(set! ,[lvalue] (mvcall ,info ,[e] ,[t]))
         (Mvcall info e t (lambda (rhs) `(set! ,lvalue ,rhs)))]
        [(mvlet ,[e] ((,x** ...) ,interface* ,body*) ...)
         (let ([label* (map (lambda (x) (make-local-label 'mv)) body*)])
           (define Pvalues
             (lambda (info t*)
               (define build-assignments
                 (lambda (x* t* body)
                   (fold-left
                     (lambda (body x t)
                       ; okay to drop t since it's a triv
                       (if (uvar-referenced? x)
                           `(seq (set! ,x ,t) ,body)
                           body))
                     body x* t*)))
               (find-matching-clause (length t*) x** interface* label*
                 (lambda (x* label)
                   ; mark label referenced so it won't be discarded
                   (local-label-iteration-set! label #t)
                   (build-assignments x* t* `(goto ,label)))
                 (lambda (nfixed x* label)
                   ; mark label referenced so it won't be discarded
                   (local-label-iteration-set! label #t)
                   (let ([xfixed* (list-head x* nfixed)]
                         [tfixed* (list-head t* nfixed)]
                         [xvar (list-ref x* nfixed)]
                         [tvar* (list-tail t* nfixed)])
                     ; the args are all trivs, otherwise this code would not properly build the rest
                     ; list after all of the arguments have been evaluated (and it couldn't suppress
                     ; the list creation when xvar is unreferenced)
                     (build-assignments xfixed* tfixed*
                       (if (uvar-referenced? xvar)
                           `(seq
                              ,(if (null? tvar*)
                                   `(set! ,xvar ,(%constant snil))
                                   (let ([t (make-tmp 't)])
                                     `(seq
                                        (set! ,t ,(%constant-alloc type-pair
                                                    (fx* (constant size-pair) (length tvar*))))
                                        ,(let f ([tvar* tvar*] [offset 0])
                                           (let ([tvar (car tvar*)] [tvar* (cdr tvar*)])
                                             `(seq
                                                (set! ,(%mref ,t
                                                         ,(fx+ (constant pair-car-disp) offset))
                                                  ,tvar)
                                                ,(if (null? tvar*)
                                                     `(seq
                                                        (set! ,(%mref ,t
                                                                 ,(fx+ (constant pair-cdr-disp) offset))
                                                          ,(%constant snil))
                                                        (set! ,xvar ,t))
                                                     (let ([next-offset (fx+ offset (constant size-pair))])
                                                       `(seq
                                                          (set! ,(%mref ,t
                                                                   ,(fx+ (constant pair-cdr-disp) offset))
                                                            ,(%lea ,t next-offset))
                                                          ,(f tvar* next-offset))))))))))
                              (goto ,label))
                           `(goto ,label)))))
                 (lambda ()
                   (let ([src (and info (info-call-src info))] [sexpr (and info (info-call-sexpr info))])
                     `(seq
                        (pariah)
                        (mvcall ,(make-info-call src sexpr #f #t #t) #f
                          (literal ,(make-info-literal #t 'object '$oops (constant symbol-value-disp)))
                          ,(%constant sfalse)
                          (literal ,(make-info-literal #f 'object "incorrect number of values received in multiple value context" 0))
                          ())))))))
           (let ([e (nanopass-case (L10.5 Expr) e
                      [,t (Pvalues #f (list t))]
                      [(values ,info ,t* ...) (Pvalues info t*)]
                      [(mvcall ,info ,mdcl ,t0? ,t1* ... (,t* ...))
                       (for-each (lambda (l) (local-label-iteration-set! l #t)) label*)
                       `(mvset ,info (,mdcl ,t0? ,t1* ...) (,t* ...) ((,x** ...) ,interface* ,label*) ...)]
                      [(if ,e0 ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
                      [(seq ,e0 ,[e1]) `(seq ,e0 ,e1)]
                      [(label ,l ,[body]) `(label ,l ,body)]
                      [(profile ,src) `(profile ,src)]
                      [(trap-check ,ioc ,[body]) `(trap-check ,ioc ,body)]
                      [(overflow-check ,[body]) `(overflow-check ,body)]
                      [(pariah) `(pariah)]
                      [(mlabel ,[e] (,l* ,[e*]) ...) `(mlabel ,e (,l* ,e*) ...)]
                      [(goto ,l) `(goto ,l)]
                      [,rhs ; alloc, inline, foreign-call
                        (let ([tmp (make-tmp 't)])
                          `(seq
                             (set! ,tmp ,rhs)
                             ,(Pvalues #f (list tmp))))]
                      [else ; set! & mvset
                        `(seq ,e ,(Pvalues #f (list (%constant svoid))))])])
             (let-values ([(label* body*)
                           (let loop ([label* label*] [body* body*] [rlabel* '()] [rbody* '()])
                             (if (null? label*)
                                 (values rlabel* rbody*)
                                 (let* ([label (car label*)])
                                   (if (local-label-iteration label)
                                       (begin
                                         (local-label-iteration-set! label #f)
                                         (loop (cdr label*) (cdr body*)
                                           (cons label rlabel*)
                                           (cons (Expr (car body*)) rbody*)))
                                       (loop (cdr label*) (cdr body*) rlabel* rbody*)))))])
               `(mlabel ,e (,label* ,body*) ...))))]))

    (define-pass np-normalize-context : L10.5 (ir) -> L11 ()
      (definitions
        (define local*)
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-tmp))
            (let ([x (make-tmp x)])
              (set! local* (cons x local*))
              x)))
        (define rhs-inline
          (lambda (lvalue info prim t*)
            (with-output-language (L11 Effect)
              (cond
                [(pred-primitive? prim)
                 `(if (inline ,info ,prim ,t* ...)
                      (set! ,lvalue ,(%constant strue))
                      (set! ,lvalue ,(%constant sfalse)))]
                [(effect-primitive? prim)
                 `(seq
                    (inline ,info ,prim ,t* ...)
                    (set! ,lvalue ,(%constant svoid)))]
                [(not (value-primitive? prim)) ($oops who "unrecognized prim ~s" prim)]
                [else `(set! ,lvalue (inline ,info ,prim ,t* ...))])))))
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) (,local0* ...) ,mcp ,interface ,body)
         (fluid-let ([local* local0*])
           (let ([tlbody (Tail body)])
             (safe-assert (nodups x* local*))
             `(clause (,x* ...) (,local* ...) ,mcp ,interface ,tlbody)))])
      (Pred : Expr (ir) -> Pred ()
        (definitions
          (define-syntax predicafy-triv
            (syntax-rules ()
              [(_ ?t)
               `(if ,(%inline eq? ?t (immediate ,(constant sfalse)))
                    (false)
                    (true))]))
          (define-syntax predicafy-rhs
            (syntax-rules ()
              [(_ ?rhs)
               (let ([t (make-tmp 't)])
                 `(seq
                    (set! ,t ?rhs)
                    ,(predicafy-triv ,t)))])))
        [,x (predicafy-triv ,x)]
        [(mref ,x1 ,x2 ,imm) (predicafy-triv ,(%mref ,x1 ,x2 ,imm))]
        [(literal ,info)
         (if (info-literal-indirect? info)
             (predicafy-triv (literal ,info))
             (if (and (eq? (info-literal-type info) 'object)
                      (eq? (info-literal-addr info) #f)
                      (eqv? (info-literal-offset info) 0))
                 `(false)
                 `(true)))]
        [(immediate ,imm) (if (eqv? imm (constant sfalse)) `(false) `(true))]
        [(label-ref ,l ,offset) `(true)]
        [(mvcall ,info ,mdcl ,[t0?] ,[t1] ... (,[t*] ...))
         (if (and (info-call-error? info) (fx< (debug-level) 2))
             `(seq (tail (mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...))) (true))
             (predicafy-rhs (mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...))))]
        [(foreign-call ,info ,[t0] ,[t1] ...) (predicafy-rhs (foreign-call ,info ,t0 ,t1 ...))]
        [(label ,l ,[pbody]) `(seq (label ,l) ,pbody)]
        [(trap-check ,ioc ,[pbody]) `(seq (trap-check ,ioc) ,pbody)]
        [(overflow-check ,[pbody]) `(seq (overflow-check) ,pbody)]
        [(profile ,src) `(seq (profile ,src) (true))]
        [(pariah) `(seq (pariah) (true))]
        [(alloc ,info ,t) `(true)]
        [(inline ,info ,prim ,[t*] ...)
         (guard (value-primitive? prim))
         (predicafy-rhs (inline ,info ,prim ,t* ...))]
        [(inline ,info ,prim ,[t*] ...)
         (guard (effect-primitive? prim))
         `(seq (inline ,info ,prim ,t* ...) (true))]
        [(inline ,info ,prim ,t* ...)
         (guard (not (pred-primitive? prim)))
         ($oops who "unrecognized prim ~s" prim)]
        [(set! ,[lvalue] (inline ,info ,prim ,[t*] ...))
         `(seq ,(rhs-inline lvalue info prim t*) (true))]
        [(set! ,[lvalue] (mvcall ,info ,mdcl ,[t0?] ,[t1] ... (,[t*] ...)))
         (guard (info-call-error? info) (fx< (debug-level) 2))
         (%seq
           (tail (mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...)))
           (true))]
        [(set! ,[lvalue] ,[rhs]) `(seq (set! ,lvalue ,rhs) (true))]
        [(mvset ,info (,mdcl ,[t0?] ,[t1] ...) (,[t*] ...) ((,x** ...) ,interface* ,l*) ...)
         `(seq
            (mvset ,info (,mdcl ,t0? ,t1 ...) (,t* ...) ((,x** ...) ,interface* ,l*) ...)
            (true))]
        [(values ,info ,t) (Pred t)]
        [(values ,info ,t* ...)
         `(seq (mvcall ,(make-info-call (info-call-src info) (info-call-sexpr info) #f #t #t) #f
                 (literal ,(make-info-literal #t 'object '$oops (constant symbol-value-disp)))
                 ,(%constant sfalse)
                 (literal ,(make-info-literal #f 'object
                             (format "returned ~r values to single value return context"
                               (length t*)) 0))
                 ())
            (true))])
      (Effect : Expr (ir) -> Effect ()
        [,x `(nop)]
        [(mref ,x1 ,x2 ,imm) `(nop)]
        [(literal ,info) `(nop)]
        [(immediate ,imm) `(nop)]
        [(label-ref ,l ,offset) `(nop)]
        [(alloc ,info ,t) `(nop)]
        [(inline ,info ,prim ,[t*] ...)
         (cond
           [(primitive-pure? prim) `(nop)] ; TODO: do we get any of these when cp0 is run?
           [(value-primitive? prim)
            `(set! ,(make-tmp 'waste) (inline ,info ,prim ,t* ...))]
           [(pred-primitive? prim)
            `(if (inline ,info ,prim ,t* ...) (nop) (nop))]
           [else `(inline ,info ,prim ,t* ...)])]
        [(set! ,[lvalue] (inline ,info ,prim ,[t*] ...))
         (rhs-inline lvalue info prim t*)]
        [(set! ,[lvalue] (mvcall ,info ,mdcl ,[t0?] ,[t1] ... (,[t*] ...)))
         (guard (info-call-error? info) (fx< (debug-level) 2))
         `(tail (mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...)))]
        [(label ,l ,[ebody]) `(seq (label ,l) ,ebody)]
        [(trap-check ,ioc ,[ebody]) `(seq (trap-check ,ioc) ,ebody)]
        [(overflow-check ,[ebody]) `(seq (overflow-check) ,ebody)]
        [(profile ,src) `(profile ,src)]
        [(pariah) `(pariah)]
        [(mvcall ,info ,mdcl ,[t0?] ,[t1] ... (,[t*] ...))
         (guard (info-call-error? info) (fx< (debug-level) 2))
         `(tail (mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...)))]
        [(mlabel ,[e] (,l* ,[e*]) ...)
         (let ([join (make-local-label 'mjoin)])
           `(seq
              ,(let f ([e e] [l* l*] [e* e*])
                 (if (null? l*)
                     e
                     (%seq ,e (goto ,join)
                       ,(f `(seq (label ,(car l*)) ,(car e*)) (cdr l*) (cdr e*)))))
              (label ,join)))]
        [(values ,info ,t* ...) `(nop)])
      (Tail : Expr (ir) -> Tail ()
        [(inline ,info ,prim ,[t*] ...)
         (guard (pred-primitive? prim))
         `(if (inline ,info ,prim ,t* ...)
              ,(%constant strue)
              ,(%constant sfalse))]
        [(inline ,info ,prim ,[t*] ...)
         (guard (effect-primitive? prim))
         `(seq (inline ,info ,prim ,t* ...) ,(%constant svoid))]
        [(inline ,info ,prim ,t* ...)
         (guard (not (value-primitive? prim)))
         ($oops who "unrecognized prim ~s" prim)]
        [(set! ,[lvalue] (inline ,info ,prim ,[t*] ...))
         `(seq ,(rhs-inline lvalue info prim t*) ,(%constant svoid))]
        [(set! ,[lvalue] (mvcall ,info ,mdcl ,[t0?] ,[t1] ... (,[t*] ...)))
         (guard (info-call-error? info) (fx< (debug-level) 2))
         `(mvcall ,info ,mdcl ,t0? ,t1 ... (,t* ...))]
        [(set! ,[lvalue] ,[rhs]) `(seq (set! ,lvalue ,rhs) ,(%constant svoid))]
        [(mvset ,info (,mdcl ,[t0?] ,[t1] ...) (,[t*] ...) ((,x** ...) ,interface* ,l*) ...)
         `(seq
            (mvset ,info (,mdcl ,t0? ,t1 ...) (,t* ...) ((,x** ...) ,interface* ,l*) ...)
            ,(%constant svoid))]
        [(label ,l ,[tlbody]) `(seq (label ,l) ,tlbody)]
        [(trap-check ,ioc ,[tlbody]) `(seq (trap-check ,ioc) ,tlbody)]
        [(overflow-check ,[tlbody]) `(seq (overflow-check) ,tlbody)]
        [(profile ,src) `(seq (profile ,src) ,(%constant svoid))]
        [(pariah) `(seq (pariah) ,(%constant svoid))]
        [(mlabel ,[tl] (,l* ,[tl*]) ...)
         (let f ([tl tl] [l* l*] [tl* tl*])
           (if (null? l*)
               tl
               `(seq
                  (tail ,tl)
                  ,(f `(seq (label ,(car l*)) ,(car tl*)) (cdr l*) (cdr tl*)))))]))

    (define-pass np-insert-trap-check : L11 (ir) -> L11.5 ()
      (Effect : Effect (ir) -> Effect ()
        [(trap-check ,ioc)
         `(seq
            (set! ,(ref-reg %trap) ,(%inline -/eq ,(ref-reg %trap) (immediate 1)))
            (if (inline ,(make-info-condition-code 'eq? #f #t) ,%condition-code)
                ,(%seq
                   (pariah)
                   (mvcall ,(make-info-call #f #f #f #t #f) #f
                     (literal ,(make-info-literal #f 'library
                                 (if ioc
                                     (lookup-does-not-expect-headroom-libspec event)
                                     (lookup-libspec event))
                                 0))
                     ()))
                (nop)))]))

    (define-pass np-flatten-case-lambda : L11.5 (ir) -> L12 ()
      (definitions
        (define Ldoargerr (make-Ldoargerr))
        (define Ldomvleterr (make-Ldomvleterr))
        (define flatten-clauses
          (lambda (info cl* dcl*)
            (let ([libspec (info-lambda-libspec info)])
              (with-output-language (L12 Tail)
                (when libspec
                  (safe-assert (equal? (info-lambda-interface* info) (list (libspec-interface libspec))))
                  (if (null? (info-lambda-fv* info))
                      (when (libspec-closure? libspec)
                        ($oops who "libspec claims closure needed, but no free variables for ~s" (libspec-name libspec)))
                      (unless (libspec-closure? libspec)
                        ($oops who "libspec claims no closure needed, but has free variables ~s for ~s" (info-lambda-fv* info) (libspec-name libspec)))))
                (if (or (info-lambda-well-known? info) libspec)
                    (let loop ([cl* cl*] [dcl* dcl*] [local* '()] [tlbody #f])
                      (if (null? cl*)
                          (values local* (or tlbody (%constant svoid)))
                          (if (or libspec (direct-call-label-referenced (car dcl*)))
                              (nanopass-case (L11.5 CaseLambdaClause) (car cl*)
                                [(clause (,x* ...) (,local1* ...) ,mcp ,interface ,tlbody1)
                                 (loop (cdr cl*) (cdr dcl*) (maybe-cons mcp (append x* local1* local*))
                                   (let ([tlbody1 `(entry-point (,x* ...) ,(car dcl*) ,mcp ,(Tail tlbody1))])
                                     (if tlbody
                                         `(seq (tail ,tlbody) ,tlbody1)
                                         tlbody1)))])
                              (loop (cdr cl*) (cdr dcl*) local* tlbody))))
                    (let f ([cl* cl*] [dcl* dcl*])
                      (if (null? cl*)
                          (values '() `(seq (pariah) (goto ,Ldoargerr)))
                          (nanopass-case (L11.5 CaseLambdaClause) (car cl*)
                            [(clause (,x* ...) (,local* ...) ,mcp ,interface ,tlbody)
                             (let ([tlbody `(entry-point (,x* ...) ,(car dcl*) ,mcp ,(Tail tlbody))])
                               (if (fx< interface 0)
                                   (let ([fixed-args (lognot interface)])
                                     (let ([tlbody (if (uvar-referenced? (list-ref x* fixed-args))
                                                       `(seq (do-rest ,fixed-args) ,tlbody)
                                                       tlbody)])
                                       (if (fx= fixed-args 0)
                                           (values (maybe-cons mcp (append x* local*)) tlbody)
                                           (let-values ([(next-local* next-tlbody) (f (cdr cl*) (cdr dcl*))])
                                             (values
                                               (maybe-cons mcp (append x* local* next-local*))
                                               `(if ,(%inline u< ,%ac0
                                                         (immediate ,fixed-args))
                                                    ,next-tlbody
                                                    ,tlbody))))))
                                   (let-values ([(next-local* next-tlbody) (f (cdr cl*) (cdr dcl*))])
                                     (values
                                       (maybe-cons mcp (append x* local* next-local*))
                                       `(if ,(%inline eq? ,%ac0
                                                 (immediate ,interface))
                                            ,tlbody
                                            ,next-tlbody)))))]))))))))
        (define flatten-mvclauses
          (lambda (x** interface* l*)
            (with-output-language (L12 Effect)
              (if (null? x**)
                  (%seq
                    (pariah)
                    ;; mverror point ensures that the call's return address
                    ;; is in sfp[0], so the caller's frame is still
                    ;; on the stack for error reporting and debugging
                    (mverror-point)
                    (goto ,Ldomvleterr))
                  (let ([x* (car x**)] [interface (car interface*)] [l (car l*)])
                    (let ([ebody `(mventry-point (,x* ...) ,l)])
                      (if (fx< interface 0)
                          (let ([fixed-args (lognot interface)])
                            (let ([ebody (if (uvar-referenced? (list-ref x* fixed-args))
                                             `(seq (do-rest ,fixed-args) ,ebody)
                                             ebody)])
                              (if (fx= fixed-args 0)
                                  ebody
                                  (let ([next-ebody (flatten-mvclauses (cdr x**) (cdr interface*) (cdr l*))])
                                    `(if ,(%inline u< ,%ac0
                                              (immediate ,fixed-args))
                                         ,next-ebody
                                         ,ebody)))))
                          (let ([next-ebody (flatten-mvclauses (cdr x**) (cdr interface*) (cdr l*))])
                            `(if ,(%inline eq? ,%ac0
                                      (immediate ,interface))
                                 ,ebody
                                 ,next-ebody))))))))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(case-lambda ,info ,cl* ...)
         (let-values ([(local* tlbody) (flatten-clauses info cl* (info-lambda-dcl* info))])
           (safe-assert (nodups local*))
           (info-lambda-dcl*-set! info (filter direct-call-label-referenced (info-lambda-dcl* info)))
           `(lambda ,info (,local* ...) ,tlbody))])
      (Tail : Tail (ir) -> Tail ())
      (Effect : Effect (ir) -> Effect ()
        [(mvset ,info (,mdcl ,[t0?] ,[t1] ...) (,[t*] ...) ((,x** ...) ,interface* ,l*) ...)
         `(mvset ,info (,mdcl ,t0? ,t1 ...) (,t* ...) ((,x** ...) ...)
            ,(flatten-mvclauses x** interface* l*))]))

    (define-pass np-impose-calling-conventions : L12 (ir) -> L13 ()
      (definitions
        (import (only asm-module asm-foreign-call asm-foreign-callable asm-enter))
        (define newframe-info-for-mventry-point)
        (define label-for-mverror-point)
        (define Lcall-error (make-Lcall-error))
        (define dcl*)
        (define local*)
        (define max-fv)
        (define le-label)
        (define-$type-check (L13 Pred))
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-tmp))
            (let ([x (make-tmp x)])
              (set! local* (cons x local*))
              x)))
        (define set-formal-registers!
          (lambda (x*)
            (let do-reg ([x* x*] [reg* arg-registers])
              (if (or (null? x*) (null? reg*))
                  x*
                  (begin
                    (uvar-location-set! (car x*) (car reg*))
                    (do-reg (cdr x*) (cdr reg*)))))))
        (define get-arg-regs
          (lambda (t*)
            (let f ([t* t*] [reg* arg-registers])
              (if (or (null? t*) (null? reg*))
                  (values '() '() t*)
                  (let ([reg (car reg*)])
                    (let-values ([(reg* reg-t* frame-t*) (f (cdr t*) (cdr reg*))])
                      (values (cons reg reg*) (cons (car t*) reg-t*) frame-t*)))))))
        (module (build-tail-call build-nontail-call build-mv-return)
          (define symref?
            (lambda (info)
              (and (info-literal-indirect? info)
                   (eq? (info-literal-type info) 'object)
                   (let ([x (info-literal-addr info)])
                     (and (symbol? x)
                          (eqv? (info-literal-offset info) (constant symbol-value-disp))
                          x)))))
          (define libref?
            (lambda (info)
              (and (not (info-literal-indirect? info))
                   (eq? (info-literal-type info) 'library)
                   (let ([x (info-literal-addr info)])
                     (and (libspec? x)
                          (eqv? (info-literal-offset info) 0)
                          x)))))
          (define build-call
            (with-output-language (L13 Tail)
              (case-lambda
                [(t rpl reg* fv* maybe-info mdcl)
                 (build-call t #f rpl reg* fv* maybe-info mdcl #f)]
                [(t cploc rpl reg* fv* maybe-info mdcl consumer?)
                 (let ()
                   (define set-return-address
                     (lambda (tl)
                       (if rpl
                           (%seq (set! ,%ref-ret (label-ref ,rpl ,(constant size-rp-header))) ,tl)
                           (meta-cond
                             [(real-register? '%ret) (%seq (set! ,%ret ,(get-fv 0)) ,tl)]
                             [else tl]))))
                   (define finish-call
                     (lambda (argcnt? cp? t)
                       (safe-assert (not (eq? t (get-fv 0))))
                       (let ([live-reg* (reg-cons* %ret (if cp? (reg-cons* %cp reg*) reg*))]
                             [live-fv* (meta-cond
                                         [(real-register? '%ret) fv*]
                                         [else (cons (get-fv 0) fv*)])])
                         (if consumer?
                             `(jump ,t (,%ac0 ,live-reg* ... ,live-fv* ...))
                             (if argcnt?
                                 `(seq
                                    (set! ,%ac0 (immediate ,(fx+ (length reg*) (length fv*))))
                                    (jump ,t (,%ac0 ,live-reg* ... ,live-fv* ...)))
                                 `(jump ,t (,live-reg* ... ,live-fv* ...)))))))
                   (define direct-call
                     (lambda ()
                       (if rpl
                           `(joto ,mdcl (,fv* ...))
                           `(goto ,mdcl))))
                   (define normal-call
                     (lambda ()
                       (define cploc-is-cp?
                         (lambda ()
                           ; cploc must be #f, an nfv, %cp or an mref tc[cp]
                           (meta-cond
                             [(real-register? '%cp) (eq? cploc %cp)]
                             [else (and cploc (not (var? cploc)))])))
                       (define-syntax set-cp
                         (syntax-rules ()
                           [(_ lhs rhs ?tl)
                            (let ([tl `?tl])
                              (if (cploc-is-cp?)
                                  tl
                                  `(seq (set! lhs rhs) ,tl)))]))
                       (define insert-procedure-check
                         (lambda (reg tlbody)
                           (if (and maybe-info (info-call-check? maybe-info))
                               `(if ,(%type-check mask-closure type-closure ,reg)
                                    ,tlbody
                                    (seq (pariah) (goto ,Lcall-error)))
                               tlbody)))
                       (if mdcl
                           (set-cp ,(ref-reg %cp) ,(or cploc (Triv t))
                             ,(set-return-address
                                (if (memq mdcl dcl*)
                                    (direct-call)
                                    (finish-call #f ; don't set the argcount, since it doesn't need to be checked
                                       #t (in-context Triv `(label-ref ,mdcl 0))))))
                           (meta-cond
                             [(real-register? '%cp)
                              (set-cp ,%cp ,(or cploc (Triv t))
                                ,(set-return-address ; must be set before potential jump to call-error
                                   (insert-procedure-check %cp
                                     (finish-call #t #t
                                       (in-context Triv
                                         (%mref ,%cp ,(constant closure-code-disp)))))))]
                             [else
                              `(seq
                                 (set! ,%xp ,(or cploc (Triv t)))
                                 ,(set-cp ,(ref-reg %cp) ,%xp
                                    ,(set-return-address ; must be set before potential jump to call-error
                                       (insert-procedure-check %xp
                                         (finish-call #t #t
                                           (in-context Triv
                                             (%mref ,%xp ,(constant closure-code-disp))))))))]))))
                   (if (not t)
                       (set-return-address
                         (if (memq mdcl dcl*)
                             (direct-call)
                             (finish-call #f #f (in-context Triv `(label-ref ,mdcl 0)))))
                       (nanopass-case (L12 Triv) t
                         ; if the expression in the cp position #f, and we have an mdcl, this is
                         ; a hackish workaround for not having a good way to express maybe-Expr
                         [(literal ,info)
                          (cond
                            [(symref? info) =>
                             ; okay to do pvalue call even if this is a consumer call since only primrefs
                             ; come through as consumer symrefs
                             (lambda (sym)
                               (%seq
                                 (set! ,%xp (literal ,(make-info-literal #f 'object sym 0)))
                                 (set! ,(ref-reg %cp) ,(%mref ,%xp ,(constant symbol-value-disp)))
                                 ,(set-return-address
                                    (finish-call #t #t
                                      (in-context Triv
                                        (%mref ,%xp ,(constant symbol-pvalue-disp)))))))]
                            [(libref? info) =>
                             (lambda (libspec)
                               (define set-cp
                                 (lambda (tlbody)
                                   (if (libspec-closure? libspec)
                                       `(seq
                                          (set! ,(ref-reg %cp) (literal ,info))
                                          ,tlbody)
                                       tlbody)))
                               (set-cp
                                 (set-return-address
                                   (finish-call #f (libspec-closure? libspec)
                                     (in-context Triv `(literal ,(make-info-literal #f 'library-code libspec (constant code-data-disp))))))))]
                            [else (normal-call)])]
                         [else (normal-call)])))])))
          (define build-consumer-call
            (lambda (tc cnfv rpl)
              ; haven't a clue which argument registers are live, so list 'em all.
              ; also haven't a clue which frame variables are live.  really need a
              ; way to list all of them as well, but we count on there being enough
              ; other registers (e.g., ac0, xp) to get us from the producer return
              ; point to the consumer jump point.
              (build-call tc cnfv rpl arg-registers '() #f #f #t)))
          (define prepare-for-consumer-call
            (lambda (mrvl)
              (with-output-language (L13 Effect)
                (let ([loc0 (if (null? arg-registers)
                                (in-context Lvalue (%mref ,%sfp 0))
                                (car arg-registers))])
                  (%seq
                    (set! ,loc0 ,%ac0)
                    (set! ,%ac0 (immediate 1))
                    (label ,mrvl))))))
          (define store-cp?
            (lambda (t)
              (nanopass-case (L12 Triv) t
                [(literal ,info) #f]
                [else #t])))
          (define build-nontail-call
            (lambda (info mdcl t0 t1* tc* nfv** mrvl prepare-for-consumer? build-postlude)
              (let-values ([(reg* reg-t* frame-t*) (get-arg-regs t1*)])
                (let ([nfv* (fold-left (lambda (ls x) (cons (make-tmp 'nfv) ls)) '() frame-t*)]
                      [cnfv* (fold-right (lambda (x ls) (cons (and (store-cp? x) (make-tmp 'cnfv)) ls)) '() tc*)]
                      [rpl* (map (lambda (tc) (make-local-label 'rpl)) tc*)]
                      [rpl (make-local-label 'rpl)])
                  (let ([newframe-info (make-info-newframe (info-call-src info) (info-call-sexpr info) (reverse (remq #f cnfv*)) nfv* nfv**)])
                    (with-output-language (L13 Effect)
                      (define build-return-point
                        (lambda (rpl mrvl cnfv* call)
                          (%seq (tail ,call) (label ,rpl) (return-point ,newframe-info ,rpl ,mrvl (,(remq #f cnfv*) ...)))))
                      (define set-locs
                        (lambda (loc* t* ebody)
                          (fold-right
                            (lambda (loc t ebody)
                              (if loc
                                  `(seq (set! ,loc ,(Triv t)) ,ebody)
                                  ebody))
                            ebody loc* t*)))
                      ((lambda (e) (if (info-call-pariah? info) (%seq (pariah) ,e) e))
                       (set-locs cnfv* tc*
                         (set-locs nfv* frame-t*
                           (set-locs reg* reg-t*
                             (%seq
                               (new-frame ,newframe-info ,rpl* ... ,rpl)
                               ,((lambda (e)
                                   (if prepare-for-consumer?
                                       `(seq ,e ,(prepare-for-consumer-call mrvl))
                                       e))
                                 (if (null? tc*)
                                     (build-return-point rpl mrvl cnfv*
                                       (build-call t0 rpl reg* nfv* info mdcl))
                                     (let ([this-mrvl (make-local-label 'mrvl)])
                                       `(seq
                                          ,(let ([rpl (car rpl*)])
                                             (build-return-point rpl this-mrvl cnfv*
                                               (build-call t0 rpl reg* nfv* info mdcl)))
                                          ,(let f ([tc* tc*] [cnfv* cnfv*] [rpl* rpl*] [this-mrvl this-mrvl])
                                             `(seq
                                                ,(prepare-for-consumer-call this-mrvl)
                                                ,(let ([tc (car tc*)] [tc* (cdr tc*)] [rpl* (cdr rpl*)] [cnfv (car cnfv*)] [cnfv* (cdr cnfv*)])
                                                   (if (null? tc*)
                                                       (build-return-point rpl mrvl cnfv*
                                                         (build-consumer-call tc cnfv rpl))
                                                       (let ([this-mrvl (make-local-label 'mrvl)])
                                                         `(seq
                                                            ,(let ([rpl (car rpl*)])
                                                               (build-return-point rpl this-mrvl cnfv*
                                                                 (build-consumer-call tc cnfv rpl)))
                                                            ,(f tc* cnfv* rpl* this-mrvl)))))))))))
                               ,(build-postlude newframe-info rpl))))))))))))
          ; NB: combine
          (define build-nontail-call-for-tail-call-with-consumers
            (lambda (info mdcl t0 t1* tc* nfv** mrvl prepare-for-consumer? build-postlude)
              (let-values ([(reg* reg-t* frame-t*) (get-arg-regs t1*)])
                (let ([nfv* (fold-left (lambda (ls x) (cons (make-tmp 'nfv) ls)) '() frame-t*)]
                      [cnfv* (fold-right (lambda (x ls) (cons (and (store-cp? x) (make-tmp 'cnfv)) ls)) '() tc*)]
                      [rpl* (map (lambda (tc) (make-local-label 'rpl)) (cdr tc*))]
                      [rpl (make-local-label 'rpl)])
                  (let ([newframe-info (make-info-newframe (info-call-src info) (info-call-sexpr info) (reverse (remq #f cnfv*)) nfv* nfv**)])
                    (with-output-language (L13 Effect)
                      (define build-return-point
                        (lambda (rpl mrvl cnfv* call)
                          (%seq (tail ,call) (label ,rpl) (return-point ,newframe-info ,rpl ,mrvl (,(remq #f cnfv*) ...)))))
                      (define set-locs
                        (lambda (loc* t* ebody)
                          (fold-right
                            (lambda (loc t ebody)
                              (if loc
                                  `(seq (set! ,loc ,(Triv t)) ,ebody)
                                  ebody))
                            ebody loc* t*)))
                      ((lambda (e) (if (info-call-pariah? info) (%seq (pariah) ,e) e))
                       (set-locs cnfv* tc*
                         (set-locs nfv* frame-t*
                           (set-locs reg* reg-t*
                             (%seq
                               (new-frame ,newframe-info ,rpl* ... ,rpl)
                               ,((lambda (e)
                                   (if prepare-for-consumer?
                                       `(seq ,e ,(prepare-for-consumer-call mrvl))
                                       e))
                                 (if (null? (cdr tc*))
                                     (build-return-point rpl mrvl cnfv*
                                       (build-call t0 rpl reg* nfv* info mdcl))
                                     (let ([this-mrvl (make-local-label 'mrvl)])
                                       `(seq
                                          ,(let ([rpl (car rpl*)])
                                             (build-return-point rpl this-mrvl cnfv*
                                               (build-call t0 rpl reg* nfv* info mdcl)))
                                          ,(let f ([tc* tc*] [cnfv* cnfv*] [rpl* rpl*] [this-mrvl this-mrvl])
                                             `(seq
                                                ,(prepare-for-consumer-call this-mrvl)
                                                ,(let ([tc (car tc*)] [tc* (cdr tc*)] [rpl* (cdr rpl*)] [cnfv (car cnfv*)] [cnfv* (cdr cnfv*)])
                                                   (if (null? (cdr tc*))
                                                       (build-return-point rpl mrvl cnfv*
                                                         (build-consumer-call tc cnfv rpl))
                                                       (let ([this-mrvl (make-local-label 'mrvl)])
                                                         `(seq
                                                            ,(let ([rpl (car rpl*)])
                                                               (build-return-point rpl this-mrvl cnfv*
                                                                 (build-consumer-call tc cnfv rpl)))
                                                            ,(f tc* cnfv* rpl* this-mrvl)))))))))))
                               ,(build-postlude newframe-info (car (last-pair cnfv*))))))))))))))
          (module (build-tail-call build-mv-return)
            (with-output-language (L13 Tail)
              (define set-locs
                (lambda (loc* t* tlbody)
                  (fold-right
                    (lambda (loc t tlbody)
                      ; omit set! for tail-frame optimization
                      (if (and (fv? loc) (uvar? t) (eq? (uvar-location t) loc))
                          tlbody
                          `(seq (set! ,loc ,(Triv t)) ,tlbody)))
                    tlbody loc* t*)))
              (define build-shift-args
                (lambda (info)
                  (with-output-language (L13 Effect)
                    (let ([Ltop (make-local-label 'Ltop)])
                      `(seq
                         (set! ,%ts ,(%inline - ,%ac0 (immediate ,(length arg-registers))))
                         (if ,(%inline <= ,%ts (immediate 0))
                             (nop)
                             ,(%seq
                                (set! ,%xp ,(%inline + ,%sfp ,(%constant ptr-bytes)))
                                (set! ,%ts ,(%inline sll ,%ts ,(%constant log2-ptr-bytes)))
                                (set! ,%ts ,(%inline + ,%ts ,%xp))
                                (label ,Ltop)
                                (shift-arg ,%xp 0 ,info)
                                (set! ,%xp ,(%inline + ,%xp ,(%constant ptr-bytes)))
                                (if ,(%inline eq? ,%xp ,%ts)
                                    (nop)
                                    (goto ,Ltop)))))))))
              (define build-tail-call
                (lambda (info mdcl t0 t1* tc*)
                  (if (null? tc*)
                      (let-values ([(reg* reg-t* frame-t*) (get-arg-regs t1*)])
                        (let ([fv* (let f ([frame-t* frame-t*] [i 0])
                                     (if (null? frame-t*)
                                         (begin (set! max-fv (fxmax max-fv i)) '())
                                         (let ([i (fx+ i 1)])
                                           (cons (get-fv i) (f (cdr frame-t*) i)))))])
                          (set-locs fv* frame-t*
                            (set-locs reg* reg-t*
                              (build-call t0 #f reg* fv* info mdcl)))))
                      (let ([tc (car (last-pair tc*))]
                            [mrvl (make-local-label 'mrvl)])
                        (if (store-cp? tc)
                            (%seq
                              ,(build-nontail-call-for-tail-call-with-consumers info mdcl t0 t1* tc* '() mrvl #t
                                 (lambda (newframe-info cnfv)
                                   (safe-assert cnfv)
                                   (%seq
                                     (remove-frame ,newframe-info)
                                     (restore-local-saves ,newframe-info)
                                     (set! ,(ref-reg %cp) ,cnfv)
                                     ,(build-shift-args newframe-info))))
                              ,(build-consumer-call tc (in-context Triv (ref-reg %cp)) #f))
                            (let ([tc* (list-head tc* (fx- (length tc*) 1))])
                              `(seq
                                 ,(build-nontail-call info mdcl t0 t1* tc* '() mrvl #t
                                    (lambda (newframe-info rpl)
                                      (%seq
                                        (remove-frame ,newframe-info)
                                        (restore-local-saves ,newframe-info)
                                        ,(build-shift-args newframe-info))))
                                 ,(build-consumer-call tc #f #f))))))))
              (define build-mv-return
                (lambda (t*)
                  (let-values ([(reg* reg-t* frame-t*) (get-arg-regs t*)])
                    (let ([fv* (let f ([frame-t* frame-t*] [i 0])
                                 (if (null? frame-t*)
                                     (begin (set! max-fv (fxmax max-fv i)) '())
                                     (let ([i (fx+ i 1)])
                                       (cons (get-fv i) (f (cdr frame-t*) i)))))])
                      (set-locs fv* frame-t*
                        (set-locs reg* reg-t*
                          `(seq
                             (set! ,%ac0 (immediate ,(length t*)))
                             ,(meta-cond
                                [(real-register? '%ret)
                                 (%seq
                                   ; must leave RA in %ret for values-error
                                   (set! ,%ret ,(get-fv 0))
                                   (jump ,(%mref ,%ret ,(constant return-address-mv-return-address-disp))
                                     (,%ac0 ,%ret ,reg* ... ,fv* ...)))]
                                [else
                                 (%seq
                                   (set! ,%xp ,(get-fv 0))
                                   (jump ,(%mref ,%xp ,(constant return-address-mv-return-address-disp))
                                     (,%ac0 ,reg* ... ,(get-fv 0) ,fv* ...)))])))))))))))
        (define-syntax do-return
          (lambda (x)
            (syntax-case x ()
              [(k retval)
               (with-implicit (k quasiquote)
                 #'`(seq
                      (set! ,%ac0 retval)
                      (jump ,(get-fv 0) (,%ac0))))])))
        (define Ref
          (lambda (x)
            (when (uvar? x) (uvar-referenced! x #t))
            x))
        (module (build-foreign-call build-fcallable)
          (with-output-language (L13 Effect)
            (define build-unfix
              (lambda (t)
                (in-context Rhs
                  (%inline sra ,t ,(%constant fixnum-offset)))))
            (define build-fix
              (lambda (t)
                (in-context Rhs
                  (%inline sll ,t ,(%constant fixnum-offset)))))
            (define Scheme->C
              ; ASSUMPTIONS: ac0, ac1, and xp are not C argument registers
              (lambda (type toC t)
                (define ptr->integer
                  (lambda (width t k)
                    (if (fx>= (constant fixnum-bits) width)
                        (k (build-unfix t))
                        `(seq
                           (set! ,%ac0 ,t)
                           (if ,(%type-check mask-fixnum type-fixnum ,%ac0)
                               ,(if (fx> width (constant ptr-bits))
                                    (%seq
                                      (set! ,%ac0 ,(build-unfix %ac0))
                                      (if ,(%inline < ,%ac0 (immediate 0))
                                          ,(k %ac0 (in-context Rhs `(immediate -1)))
                                          ,(k %ac0 (in-context Rhs `(immediate 0)))))
                                    (k (build-unfix %ac0)))
                               (seq
                                 (set! ,%ac0
                                   (inline
                                     ,(case width
                                        [(32) (intrinsic-info-asmlib dofargint32 #f)]
                                        [(64) (intrinsic-info-asmlib dofargint64 #f)]
                                        [else ($oops who "can't handle width ~s" width)])
                                     ,%asmlibcall))
                                 ,(if (fx> width (constant ptr-bits))
                                      (k %ac0 (in-context Rhs (ref-reg %ac1)))
                                      (k %ac0))))))))
                (define build-u*
                  (lambda ()
                    (let ([x (make-tmp 't)])
                      `(seq
                         (set! ,x ,t)
                         (if ,(%inline eq? ,x ,(%constant sfalse))
                             ,(toC (in-context Rhs `(immediate 0)))
                             ,(toC (in-context Rhs (%lea ,x (constant bytevector-data-disp)))))))))
                (define build-float
                  (lambda ()
                    (let ([x (make-tmp 't)])
                      `(seq
                         (set! ,x ,t)
                         ,(toC x)))))
                (nanopass-case (Ltype Type) type
                  [(fp-scheme-object) (toC t)]
                  [(fp-fixnum) (toC (build-unfix t))]
                  [(fp-u8*) (build-u*)]
                  [(fp-u16*) (build-u*)]
                  [(fp-u32*) (build-u*)]
                  [(fp-integer ,bits) (ptr->integer bits t toC)]
                  [(fp-unsigned ,bits) (ptr->integer bits t toC)]
                  [(fp-double-float) (build-float)]
                  [(fp-single-float) (build-float)]
                  [(fp-ftd ,ftd)
                   (let ([x (make-tmp 't)])
                     `(seq
                        (set! ,x ,t)
                        ,(toC (in-context Rhs
                                (%mref ,x ,(constant record-data-disp))))))]
                  [(fp-ftd& ,ftd)
                   (let ([x (make-tmp 't)])
                     (%seq
                      (set! ,x ,t)
                      (set! ,x ,(%mref ,x ,(constant record-data-disp)))
                      ,(toC x)))]
                  [else ($oops who "invalid parameter type specifier ~s" type)])))
            (define Scheme->C-for-result
              (lambda (type toC t)
                (nanopass-case (Ltype Type) type
                  [(fp-void) (toC)]
                  [(fp-ftd& ,ftd)
                   ;; pointer isn't received as a result, but instead passed
                   ;; to the function as its first argument (or simulated as such)
                   (toC)]
                  [else
                   (Scheme->C type toC t)])))
            (define C->Scheme
              ; ASSUMPTIONS: ac0, ac1, and xp are not C argument registers
              (lambda (type fromC lvalue for-return?)
                (define integer->ptr
                 ; ac0 holds low 32-bits, ac1 holds high 32 bits, if needed
                  (lambda (width lvalue)
                    (if (fx>= (constant fixnum-bits) width)
                        `(set! ,lvalue ,(build-fix %ac0))
                        (let ([e1 (lambda (big)
                                    (let ([x (make-tmp 't)])
                                      (%seq
                                        (set! ,x ,(build-fix %ac0))
                                        (set! ,x ,(build-unfix x))
                                        (if ,(%inline eq? ,x ,%ac0)
                                            (set! ,lvalue ,(build-fix %ac0))
                                            ,big))))]
                              [e2 `(seq
                                     (set! ,%ac0
                                       (inline
                                         ,(case width
                                            [(32) (intrinsic-info-asmlib dofretint32 #f)]
                                            [(64) (intrinsic-info-asmlib dofretint64 #f)]
                                              [else ($oops who "can't handle width ~s" width)])
                                         ,%asmlibcall))
                                     (set! ,lvalue ,%ac0))])
                          (if (fx> width (constant ptr-bits))
                              (let ([Lbig (make-local-label 'Lbig)] [t-ac1 (make-tmp 't-ac1)])
                                (let ([t-ac1 (make-tmp 't-ac1)])
                                  `(seq
                                     ; TODO: unnecessary if ac1 is not a pseudo register
                                     (set! ,t-ac1 ,(ref-reg %ac1))
                                     (if (if ,(%inline < ,%ac0 (immediate 0))
                                             ,(%inline eq? ,t-ac1 (immediate -1))
                                             ,(%inline eq? ,t-ac1 (immediate 0)))
                                         ,(e1 `(goto ,Lbig))
                                         (seq (label ,Lbig) ,e2)))))
                              (e1 e2))))))
                (define unsigned->ptr
                 ; ac0 holds low 32-bits, ac1 holds high 32 bits, if needed
                  (lambda (width lvalue)
                    (if (fx>= (constant fixnum-bits) width)
                        `(set! ,lvalue ,(build-fix %ac0))
                        (let ([e1 (lambda (big)
                                    `(if ,(%inline u<
                                              ,(%constant most-positive-fixnum)
                                              ,%ac0)
                                         ,big
                                         (set! ,lvalue ,(build-fix %ac0))))]
                              [e2 `(seq
                                     (set! ,%ac0
                                       (inline
                                         ,(case width
                                            [(32) (intrinsic-info-asmlib dofretuns32 #f)]
                                            [(64) (intrinsic-info-asmlib dofretuns64 #f)]
                                              [else ($oops who "can't handle width ~s" width)])
                                         ,%asmlibcall))
                                     (set! ,lvalue ,%ac0))])
                          (if (fx> width (constant ptr-bits))
                              (let ([Lbig (make-local-label 'Lbig)] [t-ac1 (make-tmp 't-ac1)])
                                (let ([t-ac1 (make-tmp 't-ac1)])
                                  `(seq
                                     ; TODO: unnecessary if ac1 is not a pseudo register
                                     (set! ,t-ac1 ,(ref-reg %ac1))
                                     (if ,(%inline eq? ,t-ac1 (immediate 0))
                                         ,(e1 `(goto ,Lbig))
                                         (seq (label ,Lbig) ,e2)))))
                              (e1 e2))))))
                (define (alloc-fptr ftd)
                  (%seq
                   (set! ,%xp
                         ,(%constant-alloc type-typed-object (fx* (constant ptr-bytes) 2) #f))
                   (set!
                    ,(%mref ,%xp ,(constant record-type-disp))
                    (literal ,(make-info-literal #f 'object ftd 0)))
                   (set! ,(%mref ,%xp ,(constant record-data-disp)) ,%ac0)
                   (set! ,lvalue ,%xp)))
                (nanopass-case (Ltype Type) type
                  [(fp-void) `(set! ,lvalue ,(%constant svoid))]
                  [(fp-scheme-object) (fromC lvalue)]
                  [(fp-fixnum)
                   (%seq
                     ,(fromC %ac0)
                     (set! ,%ac0 ,(build-fix %ac0))
                     (set! ,lvalue ,%ac0))]
                  [(fp-u8*)
                   (%seq
                     ,(fromC %ac0)
                     (set! ,%xp (inline ,(intrinsic-info-asmlib dofretu8* #f) ,%asmlibcall))
                     (set! ,lvalue ,%xp))]
                  [(fp-u16*)
                   (%seq
                     ,(fromC %ac0)
                     (set! ,%xp (inline ,(intrinsic-info-asmlib dofretu16* #f) ,%asmlibcall))
                     (set! ,lvalue ,%xp))]
                  [(fp-u32*)
                   (%seq
                     ,(fromC %ac0)
                     (set! ,%xp (inline ,(intrinsic-info-asmlib dofretu32* #f) ,%asmlibcall))
                     (set! ,lvalue ,%xp))]
                  [(fp-integer ,bits)
                   `(seq
                      ,(if (fx> bits (constant ptr-bits))
                           (fromC %ac0 (in-context Lvalue (ref-reg %ac1)))
                           (fromC %ac0))
                      ,(integer->ptr bits lvalue))]
                  [(fp-unsigned ,bits)
                   `(seq
                      ,(if (fx> bits (constant ptr-bits))
                           (fromC %ac0 (in-context Lvalue (ref-reg %ac1)))
                           (fromC %ac0))
                      ,(unsigned->ptr bits lvalue))]
                  [(fp-double-float)
                   (%seq
                     (set! ,%xp ,(%constant-alloc type-flonum (constant size-flonum) for-return?))
                     ,(fromC %xp)
                     (set! ,lvalue ,%xp))]
                  [(fp-single-float)
                   (%seq
                     (set! ,%xp ,(%constant-alloc type-flonum (constant size-flonum) for-return?))
                     ,(fromC %xp)
                     (set! ,lvalue ,%xp))]
                  [(fp-ftd ,ftd)
                   (%seq
                    ,(fromC %ac0) ; C integer return might be wiped out by alloc
                    ,(alloc-fptr ftd))]
                  [(fp-ftd& ,ftd)
                   (%seq
                    ,(fromC %ac0)
                    ,(alloc-fptr ftd))]
                  [else ($oops who "invalid result type specifier ~s" type)]))))
          (define (pick-Scall result-type)
            (nanopass-case (Ltype Type) result-type
              [(fp-void) (lookup-c-entry Scall-any-results)]
              [else (lookup-c-entry Scall-one-result)]))
          (define build-foreign-call
            (with-output-language (L13 Effect)
              (lambda (info t0 t1* maybe-lvalue new-frame?)
                (let ([arg-type* (info-foreign-arg-type* info)]
                      [result-type (info-foreign-result-type info)])
                  (let ([e (let-values ([(allocate c-args ccall c-res deallocate) (asm-foreign-call info)])
                             ; NB. allocate must save tc if not callee-save, and ccall
                             ;     (not deallocate) must restore tc if not callee-save
                             (%seq
                               ,(allocate)
                               ; cp must hold our closure or our code object.  we choose code object
                               (set! ,(%tc-ref cp) (label-ref ,le-label 0))
                               ,(with-saved-scheme-state
                                  (in) ; save just the required registers, e.g., %sfp
                                  (out %ac0 %ac1 %cp %xp %yp %ts %td scheme-args extra-regs)
                                  (fold-left (lambda (e t1 arg-type c-arg) `(seq ,(Scheme->C arg-type c-arg t1) ,e))
                                    (ccall t0) t1* arg-type* c-args))
                               ,(let ([e (deallocate)])
                                  (if maybe-lvalue
                                      (nanopass-case (Ltype Type) result-type
                                        [(fp-ftd& ,ftd)
                                         ;; Don't actually return a value, because the result
                                         ;; was instead installed in the first argument.
                                         `(seq (set! ,maybe-lvalue ,(%constant svoid)) ,e)]
                                        [else
                                         `(seq ,(C->Scheme result-type c-res maybe-lvalue #t) ,e)])
                                      e))))])
                    (if new-frame?
                        (sorry! who "can't handle nontail foreign calls")
                        e))))))
          (define build-fcallable
            (with-output-language (L13 Tail)
              (lambda (info self-label)
                (define set-locs
                  (lambda (loc* t* ebody)
                    (fold-right
                      (lambda (loc t ebody)
                        (if loc (in-context Effect `(seq (set! ,loc ,t) ,ebody)) ebody))
                      ebody loc* t*)))
                (let ([arg-type* (info-foreign-arg-type* info)]
                      [result-type (info-foreign-result-type info)])
                  (let ([x* (map (lambda (x) (make-tmp 't)) arg-type*)])
                    (let-values ([(reg* reg-x* frame-x*) (get-arg-regs x*)])
                      (let ([fv* (let f ([frame-x* frame-x*] [i 0])
                                   (if (null? frame-x*)
                                       (begin (set! max-fv (fxmax max-fv i)) '())
                                       (let ([i (fx+ i 1)])
                                         (cons (get-fv i) (f (cdr frame-x*) i)))))]
                            [cp-save (meta-cond
                                      [(real-register? '%cp) (make-tmp 'cp)]
                                      [else #f])])
                        ; add 2 for the old RA and cchain
                        (set! max-fv (fx+ max-fv 2))
                        (let-values ([(c-init c-args c-result c-return) (asm-foreign-callable info)])
                          ; c-init saves C callee-save registers and restores tc
                          ; each of c-args sets a variable to one of the C arguments
                          ; c-result converts C results to Scheme values
                          ; c-return restores callee-save registers and returns to C
                          (%seq
                            ,(c-init)
                            ,(restore-scheme-state
                               (in %cp) ; to save and then restore just before S_call_help
                               (out %ac0 %ac1 %xp %yp %ts %td scheme-args extra-regs))
                            ; need overflow check since we're effectively retroactively turning
                            ; what was a foreign call into a Scheme non-tail call
                            (fcallable-overflow-check)
                            ; leave room for the RA & c-chain
                            (set! ,%sfp ,(%inline + ,%sfp (immediate ,(fx* (constant ptr-bytes) 2))))
                            ; stash %cp and restore later to make sure it's intact by the time
                            ; that we get to S_call_help
                            ,(meta-cond
                              [(real-register? '%cp) `(set! ,cp-save ,%cp)]
                              [else `(nop)])
                            ; convert arguments
                            ,(fold-left (lambda (e x arg-type c-arg) `(seq ,(C->Scheme arg-type c-arg x #f) ,e))
                               (set-locs fv* frame-x*
                                 (set-locs (map (lambda (reg) (in-context Lvalue (%mref ,%tc ,(reg-tc-disp reg)))) reg*) reg-x*
                                   `(set! ,%ac0 (immediate ,(length arg-type*)))))
                               x* arg-type* c-args)
                            ; cookie (0) will be replaced by the procedure, so this
                            ; needs to be a quote, not an immediate
                            (set! ,(ref-reg %ac1) (literal ,(make-info-literal #f 'object 0 0)))
                            (set! ,(ref-reg %ts) (label-ref ,self-label 0)) ; for locking
                            ,(meta-cond
                              [(real-register? '%cp) `(set! ,%cp ,cp-save)]
                              [else `(nop)])
                            ,(save-scheme-state
                               (in %ac0 %ac1 %ts %cp)
                               (out %xp %yp %td scheme-args extra-regs))
                            ; Scall-{any,one}-results calls the Scheme implementation of the
                            ; callable, locking this callable wrapper (as communicated in %ts)
                            ; until just before returning
                            (inline ,(make-info-c-simple-call fv* #f (pick-Scall result-type)) ,%c-simple-call)
                            ,(restore-scheme-state
                               (in %ac0)
                               (out %ac1 %cp %xp %yp %ts %td scheme-args extra-regs))
                            ; assuming no use of %cp from here on that could get saved into `(%tc-ref cp)`:
                            ,(Scheme->C-for-result result-type c-result %ac0)
                            ,(c-return)))))))))))
        (define handle-do-rest
          (lambda (fixed-args offset save-asm-ra?)
            (with-output-language (L13 Effect)
              (let-values ([(arg reg* fv-start)
                            ; not using interface
                            (let f ([arg-number fixed-args] [rl arg-registers])
                              (cond
                                [(null? rl)
                                 (let ([fv-offset (fx+ (fx* arg-number (constant ptr-bytes)) offset)])
                                   (values
                                     (in-context Lvalue (%mref ,%sfp ,fv-offset))
                                     '()
                                     (fx+ fv-offset (constant ptr-bytes))))]
                                [(= arg-number 0) (values (car rl) (cdr rl) offset)]
                                [else (f (fx- arg-number 1) (cdr rl))]))])
                ; TODO: try to avoid using ts by starting at the end and coming back until ac0
                ; reaches k(sfp), so we can use ts and/or td as an argument register.  (need one
                ; available for the memory-memory moves)
                (let* ([Lstart (make-local-label 'Lstart)]
                       [Ldone (make-local-label 'Ldone)]
                       [bump-xp-and-store-cdr
                         `(seq
                            (set! ,%xp ,(%inline + ,%xp ,(%constant size-pair)))
                            (if ,(%inline eq? ,%xp ,%ac0)
                                (goto ,Ldone)
                                (set! ,(%mref ,%xp
                                         ,(fx- (constant pair-cdr-disp) (constant size-pair)))
                                  ,%xp)))])
                  (%seq
                    ; set ac0 to number of rest elements
                    (set! ,%ac0 ,(%inline - ,%ac0 (immediate ,fixed-args)))
                    (if ,(%inline eq? ,%ac0 (immediate 0))
                        (set! ,arg ,(%constant snil))
                        ,(%seq
                           ; adjust & scale ac0 to size of rest list in bytes
                           (set! ,%ac0 ,(%inline sll ,%ac0 ,(%constant pair-shift)))
                           ; allocate the space
                           (set! ,%xp (alloc ,(make-info-alloc (constant type-pair) #f save-asm-ra?) ,%ac0))
                           ; point ac0 past end of list
                           (set! ,%ac0 ,(%inline + ,%ac0 ,%xp))
                           ; store the first element
                           (set! ,(%mref ,%xp ,(constant pair-car-disp)) ,arg)
                           ; store the list in the first-element's old home
                           (set! ,arg ,%xp)
                           ; store remaining reg elements, then loop through frame elements
                           ,(let f ([reg* reg*])
                              (%seq
                                ,bump-xp-and-store-cdr
                                ,(if (null? reg*)
                                     (%seq
                                       ; set ts to start of the fram arguments
                                       (set! ,%ts ,(%inline + ,%sfp (immediate ,fv-start)))
                                       (label ,Lstart)
                                       ; copy next element from stack to list
                                       (set! ,(%mref ,%xp ,(constant pair-car-disp))
                                         ,(%mref ,%ts 0))
                                       ,bump-xp-and-store-cdr
                                       (set! ,%ts ,(%inline + ,%ts ,(%constant ptr-bytes)))
                                       (goto ,Lstart))
                                     (%seq
                                       (set! ,(%mref ,%xp ,(constant pair-car-disp))
                                         ,(car reg*))
                                       ,(f (cdr reg*))))))
                           (label ,Ldone)
                           ; store nil in the last cdr
                           (set! ,(%mref ,%xp
                                    ,(fx- (constant pair-cdr-disp) (constant size-pair)))
                             ,(%constant snil))))))))))
        (define make-named-info-lambda
          (lambda (name interface)
            (make-info-lambda #f #f #f interface name)))
        (define make-do-rest
          (lambda (fixed-args offset)
            (with-output-language (L13 CaseLambdaExpr)
              `(lambda ,(make-named-info-lambda 'dorest '()) 0 ()
                 ,(asm-enter
                    (%seq
                      (check-live ,(intrinsic-entry-live* (vector-ref dorest-intrinsics fixed-args)) ...)
                      ,(handle-do-rest fixed-args offset #t)
                      (asm-return ,(intrinsic-return-live* (vector-ref dorest-intrinsics fixed-args)) ...)))))))
        (define frame-args-offset (constant ptr-bytes))
        ; TODO: commonize these procedures (as macros) outside of
        ; np-expand-hand-coded/np-impose-calling-conventions?
        (define make-arg-opnd
          (lambda (n)
            (let ([regnum (length arg-registers)])
              (if (fx<= n regnum)
                  (list-ref arg-registers (fx- n 1))
                  (with-output-language (L13 Lvalue)
                    (%mref ,%sfp
                       ,(fx* (constant ptr-bytes) (fx- n regnum))))))))
        (define do-call
          (lambda (interface)
            (with-output-language (L13 Tail)
              (%seq
                (set! ,%ac0 (immediate ,interface))
                ,(meta-cond
                   [(real-register? '%cp)
                    `(jump ,(%mref ,%cp ,(constant closure-code-disp))
                       (,%ac0 ,%cp ,(reg-cons* %ret arg-registers) ...))]
                   [else
                     (%seq
                       (set! ,%td ,(ref-reg %cp))
                       (jump ,(%mref ,%td ,(constant closure-code-disp))
                         (,%ac0 ,(reg-cons* %ret arg-registers) ...)))])))))
        (with-output-language (L13 Effect)
          (meta-cond
            [(real-register? '%cp)
             (define xp/cp %cp)
             (define load-xp/cp `(nop))]
            [else
             (define xp/cp %xp)
             (define load-xp/cp `(set! ,%xp ,(ref-reg %cp)))]))
        (define-syntax %set-esp
          (lambda (x)
            (syntax-case x ()
              [(k e)
               (with-implicit (k quasiquote %mref ref-reg)
                 (if (real-register? '%esp)
                     ; write-through to tc so %esp need not be saved when going to C
                     #'`(seq
                          (set! ,(ref-reg %esp) e)
                          (set! ,(%mref ,%tc ,(tc-disp %esp)) ,(ref-reg %esp)))
                     #'`(set! ,(ref-reg %esp) e)))])))
        (define nuate-help
          (lambda ()
            ; Since cp is not always a real register, and the mref form requires us to put a var of some sort
            ; in for its base, we need to move cp to to a real register.  Unfortunately, there do not seem to be
            ; enough real registers available, since ac0 is in use through out, xp and td serve as temopraries, and
            ; we'd like to keep ts free to serve for memory to memory moves.
            ; Since this is the case, we need a temporary to put cp into when we are working with it and
            ; xp is the natural choice (or td or ts if we switched amongst their roles)
            (with-output-language (L13 Tail)
              ; cont. in cp and xp/cp, arg count in ac0, stack base in sfp, old frame base in yp
              (let ([Lmultishot (make-local-label 'Lmultishot)]
                    [Lcopy-values (make-local-label 'Lcopy-values)]
                    [Lcopyup-values (make-local-label 'Lcopyup-values)]
                    [Lcopydown-values (make-local-label 'Lcopydown-values)]
                    [Lcopy-stack (make-local-label 'Lcopy-stack)]
                    [Lreturn (make-local-label 'Lreturn)])
                (%seq
                  (set! ,%td ,(%mref ,xp/cp ,(constant continuation-stack-clength-disp)))
                  (if ,(%inline eq?
                         ,(%mref ,xp/cp ,(constant continuation-stack-length-disp))
                         ,%td)
                      ; length and clength match, so it is either mutlishot or shot1shot
                      (if ,(%inline eq? ,%td ,(%constant scaled-shot-1-shot-flag))
                          ; shot 1-shot
                          ,(%seq
                             (set! ,(ref-reg %cp) (literal ,(make-info-literal #t 'object '$oops
                                                              (constant symbol-value-disp))))
                             (set! ,(make-arg-opnd 1) ,(%constant sfalse))
                             (set! ,(make-arg-opnd 2)
                               (literal ,(make-info-literal #f 'object
                                           "attempt to invoke shot one-shot continuation" 0)))
                             ,(do-call 2))
                          ; multishot
                          ,(%seq
                             (label ,Lmultishot)
                             ; split if clength > underflow-limit
                             (if (if ,(%inline > ,%td ,(%constant underflow-limit))
                                     (true)
                                     ; resize unless stack-base + clength + size(values) <= esp
                                     ; this is conservative to save a few instructions: really need
                                     ; stack-base + clength <= esp and clength + size(values) < stack-size;
                                     ; also, size may include argument register values
                                     ; Carefully using ts again
                                     ,(%seq
                                        (set! ,%ts ,(%inline sll ,%ac0 ,(%constant log2-ptr-bytes)))
                                        (set! ,%ts ,(%inline + ,%ts ,%sfp))
                                        (set! ,%ts ,(%inline + ,%ts ,%td))
                                        ,(%inline < ,(ref-reg %esp) ,%ts)))
                                 ,(%seq
                                    ,(with-saved-scheme-state
                                       (in %ac0 %cp %xp %yp scheme-args)
                                       (out %ac1 %ts %td extra-regs)
                                       `(inline ,(make-info-c-simple-call #f (lookup-c-entry split-and-resize)) ,%c-simple-call))
                                    (set! ,%td ,(%mref ,xp/cp ,(constant continuation-stack-clength-disp))))
                                 (nop))
                             ; (new) stack base in sfp, clength in ac1, old frame base in yp
                             ; set up return address and stack link
                             (set! ,(%tc-ref stack-link) ,(%mref ,xp/cp ,(constant continuation-link-disp)))
                             ; set %td to end of the destination area / base of stack values dest
                             (set! ,%td ,(%inline + ,%td ,%sfp))
                             ; don't shift if no stack values
                             (if ,(%inline <= ,%ac0 (immediate ,(length arg-registers)))
                                 (nop)
                                 ,(%seq
                                    ; set xp to old frame base
                                    (set! ,%xp ,(ref-reg %yp))
                                    ; set sfp to stack values bytes
                                    (set! ,%sfp ,(%inline - ,%ac0 (immediate ,(length arg-registers))))
                                    (set! ,%sfp ,(%inline sll ,%sfp ,(%constant log2-ptr-bytes)))
                                    ; shift stack return values up or down
                                    (if ,(%inline < ,%xp ,%td)
                                        ,(%seq
                                           (label ,Lcopyup-values)
                                           (set! ,%sfp ,(%inline - ,%sfp ,(%constant ptr-bytes)))
                                           (set! ,(%mref ,%td ,%sfp ,frame-args-offset) ,(%mref ,%xp ,%sfp ,frame-args-offset))
                                           (if ,(%inline eq? ,%sfp (immediate 0))
                                               ,(%seq
                                                  ; restore for invariants below; td is already okay
                                                  ,load-xp/cp
                                                  (set! ,%sfp ,(%tc-ref scheme-stack)))
                                               (goto ,Lcopyup-values)))
                                        ,(%seq
                                           (set! ,%sfp ,(%inline + ,%sfp ,%td))
                                           (label ,Lcopydown-values)
                                           (set! ,(%mref ,%td ,frame-args-offset) ,(%mref ,%xp ,frame-args-offset))
                                           (set! ,%td ,(%inline + ,%td ,(%constant ptr-bytes)))
                                           (set! ,%xp ,(%inline + ,%xp ,(%constant ptr-bytes)))
                                           (if ,(%inline eq? ,%td ,%sfp)
                                               ,(%seq
                                                  ; restore for invariants below
                                                  ,load-xp/cp
                                                  (set! ,%sfp ,(%tc-ref scheme-stack))
                                                  (set! ,%td ,(%inline + ,%sfp ,(%mref ,xp/cp ,(constant continuation-stack-clength-disp)))))
                                               (goto ,Lcopydown-values))))))
                             ; invariants: xp/cp = continuation, sfp = stack base, td = end of destination area
                             ; set %xp to saved stack base
                             (set! ,%xp ,(%mref ,xp/cp ,(constant continuation-stack-disp)))
                             (label ,Lcopy-stack)
                             (if ,(%inline eq? ,%sfp ,%td)
                                 (nop)
                                 ,(%seq
                                    (set! ,(%mref ,%sfp 0) ,(%mref ,%xp 0))
                                    (set! ,%sfp ,(%inline + ,%sfp ,(%constant ptr-bytes)))
                                    (set! ,%xp ,(%inline + ,%xp ,(%constant ptr-bytes)))
                                    (goto ,Lcopy-stack)))
                             ,load-xp/cp
                             (goto ,Lreturn)))
                      ; 1 shot
                      ,(%seq
                         ; treat as multishot if clength + size(values) > length
                         ; conservative: some values may be in argument registers
                         ; AWK - very carefully using ts here as we are out of other registers
                         (set! ,%ts ,(%inline sll ,%ac0 ,(%constant log2-ptr-bytes)))
                         (set! ,%ts ,(%inline + ,%ts ,%td))
                         (if ,(%inline < ,(%mref ,xp/cp ,(constant continuation-stack-length-disp)) ,%ts)
                             (goto ,Lmultishot)
                             ,(%seq
                                ; set up stack link
                                (set! ,(%tc-ref stack-link) ,(%mref ,xp/cp ,(constant continuation-link-disp)))
                                ; place old stack in ac1 for now to cache him later (after we've removed
                                ; the values, so that we have a place to store the length and link)
                                (set! ,(ref-reg %ac1) ,%sfp)
                                ; grab saved stack
                                (set! ,%sfp ,(%mref ,xp/cp ,(constant continuation-stack-disp)))
                                ; set up tc's scheme-stack variable
                                (set! ,(%tc-ref scheme-stack) ,%sfp)
                                ; set up esp as stack-base + length - slop
                                (set! ,%ts ,(%inline - ,%sfp ,(%constant stack-slop)))
                                ,(%set-esp ,(%inline + ,%ts ,(%mref ,xp/cp ,(constant continuation-stack-length-disp))))
                                ; set up frame pointer to stack-base + current length
                                (set! ,%sfp ,(%inline + ,%sfp ,%td))
                                ; bypass copy loop if no stack values
                                (if ,(%inline <= ,%ac0 (immediate ,(length arg-registers)))
                                    (nop)
                                    ,(%seq
                                       ; set td to stack values bytes
                                       (set! ,%td ,(%inline - ,%ac0 (immediate ,(length arg-registers))))
                                       (set! ,%td ,(%inline sll ,%td ,(%constant log2-ptr-bytes)))
                                       ; set xp, td to top of stack values src, dest
                                       (set! ,%xp ,(ref-reg %yp))
                                       ; move stack return values to top of saved stack segment
                                       (label ,Lcopy-values)
                                       (set! ,%td ,(%inline - ,%td ,(%constant ptr-bytes)))
                                       (set! ,(%mref ,%sfp ,%td ,frame-args-offset) ,(%mref ,%xp ,%td ,frame-args-offset))
                                       (if ,(%inline eq? ,%td (immediate 0))
                                           ,load-xp/cp ; need to load cp-reg, since xp is wiped out
                                           (goto ,Lcopy-values))))
                                ; place old stack in stack cache
                                (set! ,%td ,(ref-reg %ac1))
                                (set! ,(%mref ,%td 0) ,(%tc-ref scheme-stack-size))
                                (set! ,(%mref ,%td ,(constant ptr-bytes)) ,(%tc-ref stack-cache))
                                (set! ,(%tc-ref stack-cache) ,%td)
                                ; set up tc's stack-size variable
                                (set! ,(%tc-ref scheme-stack-size) ,(%mref ,xp/cp ,(constant continuation-stack-length-disp)))
                                ; mark continuation shot
                                (set! ,(%mref ,xp/cp ,(constant continuation-stack-length-disp)) ,(%constant scaled-shot-1-shot-flag))
                                (set! ,(%mref ,xp/cp ,(constant continuation-stack-clength-disp)) ,(%constant scaled-shot-1-shot-flag))
                                ; return with 1 or multiple values
                                (label ,Lreturn)
                                (if ,(%inline eq? ,%ac0 (immediate 1))
                                    ,(%seq
                                       (set! ,%ac0 ,(make-arg-opnd 1))
                                       (jump ,(%mref ,xp/cp ,(constant continuation-return-address-disp)) (,%ac0)))
                                    ,(meta-cond
                                       [(real-register? '%ret)
                                        (%seq
                                          (set! ,%ret ,(%mref ,xp/cp ,(constant continuation-return-address-disp)))
                                          (jump ,(%mref ,%ret ,(constant return-address-mv-return-address-disp))
                                            (,%ac0 ,%ret ,arg-registers ...)))]
                                       [else
                                         (let ([fv0 (get-fv 0)])
                                           (%seq
                                             (set! ,%xp ,(%mref ,xp/cp ,(constant continuation-return-address-disp)))
                                             (set! ,fv0 ,%xp)
                                             (jump ,(%mref ,%xp ,(constant return-address-mv-return-address-disp))
                                               (,%ac0 ,arg-registers ... ,fv0))))]))))))))))))
      (Program : Program (ir) -> Program ()
        [(labels ([,l* ,le*] ...) ,l)
         `(labels ([,l* ,(map CaseLambdaExpr le* l*)] ...) ,l)])
      (CaseLambdaExpr : CaseLambdaExpr (ir l) -> CaseLambdaExpr ()
        [(lambda ,info (,local0* ...) ,tlbody)
         (fluid-let ([dcl* (info-lambda-dcl* info)] [max-fv 0] [local* local0*] [le-label l])
           (let ([tlbody (Tail tlbody)])
             (let ([local* (filter uvar-referenced? local*)])
               (safe-assert (nodups local*))
               (for-each (lambda (local) (uvar-location-set! local #f)) local*)
               `(lambda ,info ,max-fv (,local* ...) ,tlbody))))]
        [(fcallable ,info ,l)
         (let ([lambda-info (make-info-lambda #f #f #f (list (length (info-foreign-arg-type* info)))
                              (info-foreign-name info) (constant code-flag-template))])
           (fluid-let ([max-fv 0] [local* '()])
             (let ([tlbody (build-fcallable info l)])
               `(lambda ,lambda-info ,max-fv (,local* ...) ,tlbody))))]
        [(hand-coded ,sym)
         (case sym
           [(dorest0) (make-do-rest 0 frame-args-offset)]
           [(dorest1) (make-do-rest 1 frame-args-offset)]
           [(dorest2) (make-do-rest 2 frame-args-offset)]
           [(dorest3) (make-do-rest 3 frame-args-offset)]
           [(dorest4) (make-do-rest 4 frame-args-offset)]
           [(dorest5) (make-do-rest 5 frame-args-offset)]
           [(callcc)
            (let ([Ltop (make-local-label 'Ltop)])
              `(lambda ,(make-named-info-lambda 'callcc '(1)) 0 ()
                 ,(%seq
                    (set! ,(ref-reg %cp) ,(make-arg-opnd 1))
                    (set! ,%td ,(%tc-ref stack-link))
                    (set! ,%xp ,%td)
                    (label ,Ltop)
                    (set! ,%ac0 ,(%mref ,%xp ,(constant continuation-stack-clength-disp)))
                    (if ,(%inline eq?
                           ,(%mref ,%xp ,(constant continuation-stack-length-disp))
                           ,%ac0)
                        ,(%seq
                           (set! ,%ac0
                             (literal ,(make-info-literal #f 'library-code
                                         (lookup-libspec dounderflow)
                                         (fx+ (constant code-data-disp) (constant size-rp-header)))))
                           (if (if ,(%inline eq? ,%ref-ret ,%ac0)
                                   ,(%inline eq?
                                      ,(%mref ,%td ,(constant continuation-winders-disp))
                                      ,(%tc-ref winders))
                                   (false))
                               ,(%seq
                                  (set! ,(make-arg-opnd 1) ,%td)
                                  ,(do-call 1))
                               ,(%seq
                                  (set! ,%xp ,(%constant-alloc type-closure (constant size-continuation)))
                                  ; TODO: remove next line once get-room preserves %td
                                  (set! ,%td ,(%tc-ref stack-link))
                                  (set! ,(%mref ,%xp ,(constant continuation-code-disp))
                                    (literal ,(make-info-literal #f 'library (lookup-libspec nuate) (constant code-data-disp))))
                                  (set! ,(%mref ,%xp ,(constant continuation-return-address-disp)) ,%ref-ret)
                                  (set! ,(%mref ,%xp ,(constant continuation-winders-disp)) ,(%tc-ref winders))
                                  (set! ,%ref-ret ,%ac0)
                                  (set! ,(%mref ,%xp ,(constant continuation-link-disp)) ,%td)
                                  (set! ,(%tc-ref stack-link) ,%xp)
                                  (set! ,%ac0 ,(%tc-ref scheme-stack))
                                  (set! ,(%tc-ref scheme-stack) ,%sfp)
                                  (set! ,(%mref ,%xp ,(constant continuation-stack-disp)) ,%ac0)
                                  (set! ,%ac0 ,(%inline - ,%sfp ,%ac0))
                                  (set! ,(%mref ,%xp ,(constant continuation-stack-length-disp)) ,%ac0)
                                  (set! ,(%mref ,%xp ,(constant continuation-stack-clength-disp)) ,%ac0)
                                  (set! ,(%tc-ref scheme-stack-size) ,(%inline - ,(%tc-ref scheme-stack-size) ,%ac0))
                                  (set! ,(make-arg-opnd 1) ,%xp)
                                  ,(do-call 1))))
                        ,(%seq
                           (set! ,(%mref ,%xp ,(constant continuation-stack-length-disp)) ,%ac0)
                           (set! ,%xp ,(%mref ,%xp ,(constant continuation-link-disp)))
                           (goto ,Ltop))))))]
           [(call1cc)
            `(lambda ,(make-named-info-lambda 'call1cc '(1)) 0 ()
               ,(%seq
                  (set! ,(ref-reg %cp) ,(make-arg-opnd 1))
                  (set! ,%td ,(%tc-ref stack-link))
                  (set! ,%ac0
                    (literal ,(make-info-literal #f 'library-code
                                   (lookup-libspec dounderflow)
                                   (fx+ (constant code-data-disp) (constant size-rp-header)))))
                  (if (if ,(%inline eq? ,%ref-ret ,%ac0)
                          ,(%inline eq?
                             ,(%mref ,%td ,(constant continuation-winders-disp))
                             ,(%tc-ref winders))
                          (false))
                      ,(%seq
                         (set! ,(make-arg-opnd 1) ,%td)
                         ,(do-call 1))
                      ,(%seq
                         (set! ,%xp ,(%constant-alloc type-closure (constant size-continuation)))
                         ; TODO: remove next line once get-room preserves %td
                         (set! ,%td ,(%tc-ref stack-link))
                         (set! ,(%mref ,%xp ,(constant continuation-code-disp))
                           (literal ,(make-info-literal #f 'library (lookup-libspec nuate) (constant code-data-disp))))
                         (set! ,(%mref ,%xp ,(constant continuation-return-address-disp)) ,%ref-ret)
                         (set! ,(%mref ,%xp ,(constant continuation-winders-disp))
                           ,(%tc-ref winders))
                         ,(meta-cond
                            [(real-register? '%ret) `(set! ,%ret ,%ac0)]
                            [else `(nop)])
                         (set! ,(%mref ,%xp ,(constant continuation-link-disp)) ,%td)
                         (set! ,(%tc-ref stack-link) ,%xp)
                         (set! ,%ac0 ,(%tc-ref scheme-stack))
                         (set! ,(%mref ,%xp ,(constant continuation-stack-disp)) ,%ac0)
                         (set! ,(%mref ,%xp ,(constant continuation-stack-clength-disp))
                           ,(%inline - ,%sfp ,%ac0))
                         ; we need to get ourselves a new stack.  we carve it out of the old
                         ; one if the old one is large enough.  if not, we look for one in
                         ; the cache.  if the cache is empty, we allocate a new stack.
                         (set! ,%sfp ,(%inline + ,%sfp (immediate ,(fx* (constant one-shot-headroom) 2))))
                         (if ,(%inline <= ,%sfp ,(ref-reg %esp))
                             ,(%seq
                                (set! ,%sfp ,(%inline - ,%sfp ,(%constant one-shot-headroom)))
                                (set! ,%ac0 ,(%inline - ,%sfp ,%ac0))
                                (set! ,(%mref ,%xp ,(constant continuation-stack-length-disp)) ,%ac0)
                                (set! ,(%tc-ref scheme-stack) ,%sfp)
                                (set! ,(%tc-ref scheme-stack-size) ,(%inline - ,(%tc-ref scheme-stack-size) ,%ac0))
                                (set! ,(make-arg-opnd 1) ,%xp)
                                ,(meta-cond
                                   [(real-register? '%ret) `(nop)]
                                   [else `(set! ,%ref-ret
                                            (literal ,(make-info-literal #f 'library-code
                                                        (lookup-libspec dounderflow)
                                                        (fx+ (constant code-data-disp) (constant size-rp-header)))))])
                                ,(do-call 1))
                             ,(%seq
                                ; set continuation length to entire stack size
                                (set! ,(%mref ,%xp ,(constant continuation-stack-length-disp))
                                  ,(%tc-ref scheme-stack-size))
                                (set! ,%sfp ,(%tc-ref stack-cache))
                                (if ,(%inline eq? ,%sfp ,(%constant snil))
                                    ,(%seq
                                       (set! ,%ac0 ,%xp)
                                       (set! ,%xp ,(%constant-alloc typemod (constant default-stack-size)))
                                       (set! ,%sfp ,%xp)
                                       (set! ,(%tc-ref scheme-stack) ,%sfp)
                                       (set! ,(%tc-ref scheme-stack-size) ,(%constant default-stack-size))
                                       ,(%set-esp ,(%inline + ,%sfp
                                                     (immediate ,(fx- (constant default-stack-size) (constant stack-slop)))))
                                       (set! ,(make-arg-opnd 1) ,%ac0)
                                       ,(meta-cond
                                          [(real-register? '%ret) `(nop)]
                                          [else `(set! ,%ref-ret
                                                   (literal ,(make-info-literal #f 'library-code
                                                               (lookup-libspec dounderflow)
                                                               (fx+ (constant code-data-disp) (constant size-rp-header)))))])
                                       ,(do-call 1))
                                    ,(%seq
                                       (set! ,(%tc-ref stack-cache) ,(%mref ,%sfp ,(constant ptr-bytes))) ; next stack-segment
                                       (set! ,%ac0 ,(%mref ,%sfp 0)) ; stack-segment size
                                       (set! ,(%tc-ref scheme-stack) ,%sfp)
                                       (set! ,(%tc-ref scheme-stack-size) ,%ac0)
                                       ,(%set-esp ,(%lea ,%ac0 ,%sfp (fx- (constant stack-slop))))
                                       (set! ,(make-arg-opnd 1) ,%xp)
                                       ,(meta-cond
                                          [(real-register? '%ret) `(nop)]
                                          [else `(set! ,%ref-ret
                                                   (literal ,(make-info-literal #f 'library-code
                                                               (lookup-libspec dounderflow)
                                                               (fx+ (constant code-data-disp) (constant size-rp-header)))))])
                                       ,(do-call 1)))))))))]
           [(dounderflow)
            (let ([Lret (make-local-label 'Lret)] [Lmvreturn (make-local-label 'Lmvreturn)])
              `(lambda ,(make-named-info-lambda 'winder-dummy '()) 0 ()
                 ,(%seq
                    ; (asm align)
                    (label ,Lret)
                    (rp-header ,Lmvreturn 0 0)
                    (set! ,(make-arg-opnd 1) ,%ac0)
                    (set! ,%ac0 (immediate 1))
                    (label ,Lmvreturn)
                    (set! ,xp/cp ,(%tc-ref stack-link))
                    ,(meta-cond
                       [(real-register? '%cp) `(nop)]
                       [else `(set! ,(ref-reg %cp) ,xp/cp)])
                    (set! ,(ref-reg %yp) ,%sfp)
                    ,(nuate-help))))]
           [(nuate)
            (let ([info (make-named-info-lambda 'continuation '(-1))])
              (info-lambda-flags-set! info (fxlogor (constant code-flag-continuation) (constant code-flag-system)))
              `(lambda ,info 0 ()
                 ,(%seq
                    ,load-xp/cp
                    (if ,(%inline eq? ,(%tc-ref winders)
                           ,(%mref ,xp/cp ,(constant continuation-winders-disp)))
                        ,(%seq
                           (set! ,(ref-reg %yp) ,%sfp)
                           (set! ,%sfp ,(%tc-ref scheme-stack))
                           ,(nuate-help))
                        ,(%seq
                           (if ,(%inline eq? ,%ac0 (immediate 0))
                               (set! ,%xp ,(%constant snil))
                               ,(%seq
                                  ,(handle-do-rest 0 frame-args-offset #f)
                                  (set! ,%xp ,(make-arg-opnd 1))))
                           (set! ,%sfp ,(%tc-ref scheme-stack))
                           (set! ,(make-arg-opnd 2) ,%xp)
                           (set! ,(make-arg-opnd 1) ,(ref-reg %cp))
                           (jump (literal ,(make-info-literal #f 'library-code
                                             (lookup-libspec dounderflow*)
                                             (constant code-data-disp)))
                             (,(reg-cons* %cp arg-registers) ...)))))))]
           [else `(hand-coded ,sym)])])
      (Lvalue : Lvalue (ir) -> Lvalue ()
        [,x (Ref x)]
        [(mref ,x1 ,x2 ,imm) (%mref ,(Ref x1) ,(Ref x2) ,imm)])
      (Triv : Triv (ir) -> Triv ()
        [,x (Ref x)] ; TODO: cannot call ref in cata, as we don't allow top-level cata
        [(mref ,x1 ,x2 ,imm) (%mref ,(Ref x1) ,(Ref x2) ,imm)])
      (Rhs : Rhs (ir) -> Rhs ()
        [(mvcall ,info ,mdcl ,t0? ,t1* ... (,t* ...))
         ($oops who "Effect is responsible for handling mvcalls")])
      (Effect : Effect (ir) -> Effect ()
        [(do-rest ,fixed-args)
         (if (fx<= fixed-args dorest-intrinsic-max)
             `(inline ,(intrinsic-info-asmlib (vector-ref dorest-intrinsics fixed-args) #f) ,%asmlibcall!)
             (handle-do-rest fixed-args frame-args-offset #f))]
        ; TODO: get internal error when , is missing from ,l
        [(mventry-point (,x* ...) ,l)
         (%seq
           (remove-frame ,newframe-info-for-mventry-point)
           ,(let f ([x* x*])
              (if (null? x*)
                  (%seq
                    (restore-local-saves ,newframe-info-for-mventry-point)
                    (goto ,l))
                  (let ([x (car x*)])
                    (if (uvar-referenced? x)
                        `(seq (set! ,x ,(uvar-location x)) ,(f (cdr x*)))
                        (f (cdr x*)))))))]
        [(mverror-point)
         `(set! ,%ref-ret (label-ref ,label-for-mverror-point ,(constant size-rp-header)))]
        [(mvcall ,info ,mdcl ,t0? ,t1* ... (,t* ...))
         (let ([mrvl (make-local-label 'mrvl)])
           (build-nontail-call info mdcl t0? t1* t* '() mrvl #f
             (lambda (newframe-info rpl)
               (%seq (label ,mrvl) (remove-frame ,newframe-info) (restore-local-saves ,newframe-info)))))]
        [(mvset ,info (,mdcl ,t0? ,t1* ...) (,t* ...) ((,x** ...) ...) ,ebody)
         (let* ([frame-x** (map (lambda (x*) (set-formal-registers! x*)) x**)]
                [nfv** (map (lambda (x*) (map (lambda (x)
                                                 (let ([nfv (make-tmp 'mvset-nfv)])
                                                   (uvar-location-set! x nfv)
                                                   nfv))
                                            x*))
                         frame-x**)])
           (let ([mrvl (make-local-label 'mrvl)])
             (build-nontail-call info mdcl t0? t1* t* nfv** mrvl #t
               (lambda (newframe-info rpl)
                 (fluid-let ([newframe-info-for-mventry-point newframe-info]
                             [label-for-mverror-point rpl])
                   (Effect ebody))))))]
        [(set! ,[lvalue] (mvcall ,info ,mdcl ,t0? ,t1* ... (,t* ...)))
         (build-nontail-call info mdcl t0? t1* t* '() #f #f
           (lambda (newframe-info rpl)
             (let ([retval (make-tmp 'retval)])
               (%seq
                 (remove-frame ,newframe-info)
                 (set! ,retval ,%ac0)
                 (restore-local-saves ,newframe-info)
                 (set! ,lvalue ,retval)))))]
        [(foreign-call ,info ,[t0] ,[t1*] ...)
         (build-foreign-call info t0 t1* #f #t)]
        [(set! ,[lvalue] (foreign-call ,info ,[t0] ,[t1*] ...))
         (build-foreign-call info t0 t1* lvalue #t)])
      (Tail : Tail  (ir) -> Tail  ()
        [(entry-point (,x* ...) ,dcl ,mcp ,tlbody)
         (unless (andmap (lambda (x) (eq? (uvar-type x) 'ptr)) x*)
           ($oops who "can't handle anything but plain vanilla types yet"))
         ; clear and recompute referenced flags on entry-point formals in case tail-frame
         ; optimization eliminates all of the references
         (when mcp (uvar-referenced! mcp #f))
         (for-each (lambda (x) (uvar-referenced! x #f)) x*)
         (let do-frame ([x* (set-formal-registers! x*)] [fv-idx 1])
           (unless (null? x*)
             (let ([x (car x*)] [fv (get-fv fv-idx)])
               (uvar-location-set! x fv)
               (do-frame (cdr x*) (fx+ fv-idx 1)))))
         (let ()
           (define bind-formals
             (lambda (mcp x* tlbody)
               (define add-cpset
                 (lambda (mcp tlbody)
                   (if (and mcp (uvar-referenced? mcp)) `(seq (set! ,mcp ,(ref-reg %cp)) ,tlbody) tlbody)))
               ; we set cp after registers and before frame vars, since it might
               ; or might not be a register
               (let f ([x* x*] [mcp mcp])
                 (if (null? x*)
                     (add-cpset mcp tlbody)
                     (let ([x (car x*)])
                       (if (uvar-referenced? x)
                           (let ([loc (uvar-location x)])
                             (if (fv? loc)
                                 (begin
                                   (set! max-fv (fxmax max-fv (fv-offset loc)))
                                   (add-cpset mcp `(seq (set! ,x ,loc) ,(f (cdr x*) #f))))
                                 `(seq (set! ,x ,loc) ,(f (cdr x*) mcp))))
                           (f (cdr x*) mcp)))))))
           (let ([tlbody (Tail tlbody)])
             (%seq
               (label ,dcl)
               ; TODO: don't want to save ret for leaf routines
               ; TODO: don't necessarily want to position ret save here
               ,(meta-cond
                  [(real-register? '%ret) `(set! ,(get-fv 0) ,%ret)]
                  [else `(nop)])
               (overflood-check)
               ,(bind-formals mcp x* tlbody))))]
        [(mvcall ,info ,mdcl ,t0? ,t1* ... (,t* ...))
         (build-tail-call info mdcl t0? t1* t*)]
        [(foreign-call ,info ,[t0] ,[t1*] ...)
         `(seq
            ; CAUTION: fv0 must hold return address when we call into C
            ,(build-foreign-call info t0 t1* %ac0 #f)
            (jump ,(get-fv 0) (,%ac0)))]
        [,rhs (do-return ,(Rhs ir))]
        [(values ,info ,[t]) (do-return ,t)]
        [(values ,info ,t* ...) (build-mv-return t*)]))

    (define-pass np-expand-hand-coded : L13 (ir) -> L13.5 ()
      (definitions
        (import (only asm-module asm-enter))
        (define Ldoargerr (make-Ldoargerr))
        (define-$type-check (L13.5 Pred))
        (define make-info
          (lambda (name interface*)
            (make-info-lambda #f #f #f interface* name)))
        (define make-arg-opnd
          (lambda (n)
            (let ([regnum (length arg-registers)])
              (if (fx<= n regnum)
                  (list-ref arg-registers (fx- n 1))
                  (with-output-language (L13.5 Lvalue)
                    (%mref ,%sfp
                       ,(fx* (constant ptr-bytes) (fx- n regnum))))))))
        (define do-call
          (lambda ()
            (with-output-language (L13.5 Tail)
              (meta-cond
                [(real-register? '%cp)
                 `(jump ,(%mref ,%cp ,(constant closure-code-disp))
                    (,%ac0 ,%cp ,(reg-cons* %ret arg-registers) ...))]
                [else
                  (%seq
                    (set! ,%td ,(ref-reg %cp))
                    (jump ,(%mref ,%td ,(constant closure-code-disp))
                      (,%ac0 ,(reg-cons* %ret arg-registers) ...)))]))))
        (define (make-list*-procedure name)
          (with-output-language (L13.5 CaseLambdaExpr)
            (let ([Ltop (make-local-label 'ltop)])
              `(lambda ,(make-info name '(-2)) 0 ()
                 (seq
                   (set! ,%ac0 ,(%inline - ,%ac0 (immediate 1)))
                   ; TODO: would be nice to avoid cmpl here
                   (if ,(%inline eq? ,%ac0 (immediate 0))
                       (seq
                         (set! ,%ac0 ,(make-arg-opnd 1))
                         (jump ,%ref-ret (,%ac0)))
                       ; TODO: would be nice to avoid second cmpl here
                       (if ,(%inline < ,%ac0 (immediate 0))
                           (seq (pariah) (goto ,Ldoargerr))
                           ,(%seq
                              (set! ,%ac0 ,(%inline sll ,%ac0 ,(%constant pair-shift)))
                              (set! ,%xp (alloc ,(make-info-alloc (constant type-pair) #f #f) ,%ac0))
                              ,(let f ([reg* arg-registers] [i 0])
                                 (if (null? reg*)
                                     ; filled in first i pairs
                                     ; have at least two stack arguments
                                     ; ac0 is at least (i+1) * pair-size; also amount allocated
                                     (%seq
                                       ; point xp to last pair of list
                                       (set! ,%xp
                                         ,(%lea ,%xp ,%ac0
                                            (fx- (constant size-pair))))
                                       ; adjust from two ptrs per pair to one ptr per stack element
                                       (set! ,%ac0
                                         ,(%inline srl ,%ac0 (immediate 1)))
                                       ; point ac0 to second-to-last stack argument
                                       (set! ,%ac0
                                         ,(%lea ,%sfp ,%ac0
                                            (fx* i (fx- (constant ptr-bytes)))))
                                       (set! ,(%mref ,%xp ,(constant pair-cdr-disp))
                                         ,(%mref ,%ac0 ,(constant ptr-bytes)))
                                       (label ,Ltop)
                                       (set! ,(%mref ,%xp ,(constant pair-car-disp))
                                         ,(%mref ,%ac0 0))
                                       (set! ,%ac0 ,(%inline - ,%ac0 ,(%constant ptr-bytes)))
                                       (if ,(%inline eq? ,%ac0 ,%sfp)
                                           ,(%seq
                                              (set! ,%ac0 ,(%inline - ,%xp (immediate ,(fx* i (constant size-pair)))))
                                              (jump ,%ref-ret (,%ac0)))
                                           ,(%seq
                                              (set! ,(%mref ,%xp ,(fx- (constant pair-cdr-disp) (constant size-pair)))
                                                ,%xp)
                                              (set! ,%xp ,(%inline - ,%xp ,(%constant size-pair)))
                                              (goto ,Ltop))))
                                     (%seq
                                       (set! ,(%mref ,%xp
                                                ,(fx+ (fx* i (constant size-pair)) (constant pair-car-disp)))
                                         ,(car reg*))
                                       (if ,(%inline eq? ,%ac0 (immediate ,(fx* (fx+ i 1) (constant size-pair))))
                                           ,(%seq
                                              (set! ,(%mref ,%xp
                                                       ,(fx+ (fx* i (constant size-pair)) (constant pair-cdr-disp)))
                                                ,(make-arg-opnd (fx+ i 2)))
                                              (set! ,%ac0 ,%xp)
                                              (jump ,%ref-ret (,%ac0)))
                                           ,(%seq
                                              (set! ,(%mref ,%xp
                                                       ,(fx+ (fx* i (constant size-pair)) (constant pair-cdr-disp)))
                                                ,(%inline + ,%xp
                                                   (immediate ,(fx* (fx+ i 1) (constant size-pair)))))
                                              ,(f (cdr reg*) (fx+ i 1)))))))))))))))
        (module (make-do/call make-do/ret)
          (define make-do
            (lambda (enter e)
              ; ret-loc is relevant only on machines with %ret reg:
              ;   #f => ret is known to be at sfp[0]---no need to save or restore
              ;   non-#f => save and restore to/from ret-loc
              ; if C needs to know about or might change the return address, ret-loc
              ; must be either #f or sfp[0].  otherwise, it can be (%tc-ref ret), which
              ; is useful if we don't know if %ret holds the return address.  in that case,
              ; saving %ret to (%tc-ref ret) does no harm, nor does restoring it
              ; from there, but it might be harmful to save %ret to sfp[0], since %ret's
              ; contents are unknown.
              (lambda (ret-loc name entry)
                (with-output-language (L13.5 CaseLambdaExpr)
                  `(lambda ,(make-info name '()) 0 ()
                     ,(enter
                        (%seq
                          ,(meta-cond
                             [(real-register? '%ret) (if ret-loc `(set! ,ret-loc ,%ret) `(nop))]
                             [else `(nop)])
                          ,(with-saved-scheme-state
                             (in %ac0 %ac1 %cp %xp %yp %ts %td scheme-args extra-regs)
                             (out)
                             `(inline ,(make-info-c-simple-call #t entry) ,%c-simple-call))
                          ,(meta-cond
                             [(real-register? '%ret) (if ret-loc `(set! ,%ret ,ret-loc) `(nop))]
                             [else `(nop)])
                          ,e)))))))
          (define make-do/call (make-do (lambda (e) e) (do-call)))
          (define (make-do/ret entry-live* return-live*)
            (with-output-language (L13.5 Tail)
              (make-do (lambda (e) (asm-enter (%seq (check-live ,entry-live* ...) ,e)))
                `(asm-return ,return-live* ...)))))
        (define make-dofargint
          (lambda (name size entry-live* return-live*)
            (with-output-language (L13.5 CaseLambdaExpr)
              `(lambda ,(make-info name '()) 0 ()
                 ,(asm-enter
                    (%seq
                      (check-live ,entry-live* ...)
                      ,(cond
                         [(= (constant bigit-bits) size)
                          (%seq
                            (set! ,%td ,(%mref ,%ac0 ,(constant bignum-type-disp)))
                            (set! ,%ac0
                              (inline ,(make-info-load (bigit-type) #f) ,%load
                                ,%ac0 ,%zero
                                ,(%constant bignum-data-disp)))
                            (if ,(%inline eq? ,%td
                                   (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                 (constant type-positive-bignum))))
                                (nop)
                                (set! ,%ac0 ,(%inline - (immediate 0) ,%ac0))))]
                         [(= (* (constant bigit-bits) 2) (* (constant ptr-bits) 2) size)
                          (let ([ac1 (in-context Lvalue (ref-reg %ac1))])
                            (let ([Lnegative (make-local-label 'Lnegative)] [Lreturn (make-local-label 'Lreturn)])
                              (%seq
                                (set! ,%xp ,%ac0)
                                (set! ,%td ,(%mref ,%xp ,(constant bignum-type-disp)))
                                (if ,(%inline eq? ,%td
                                       (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                     (constant type-positive-bignum))))
                                    ,(%seq
                                       (set! ,%ac0
                                         (inline ,(make-info-load (bigit-type) #f) ,%load
                                           ,%xp ,%zero
                                           ,(%constant bignum-data-disp)))
                                       (set! ,ac1 (immediate 0))
                                       (goto ,Lreturn))
                                    (if ,(%inline eq? ,%td
                                           (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                         (constant type-negative-bignum))))
                                        ,(%seq
                                           (set! ,%ac0
                                             (inline ,(make-info-load (bigit-type) #f) ,%load
                                               ,%xp ,%zero
                                               ,(%constant bignum-data-disp)))
                                           (set! ,ac1 (immediate 0))
                                           (goto ,Lnegative))
                                        ,(%seq
                                           (set! ,ac1
                                             (inline ,(make-info-load (bigit-type) #f) ,%load
                                               ,%xp ,%zero
                                               ,(%constant bignum-data-disp)))
                                           (set! ,%ac0
                                             (inline ,(make-info-load (bigit-type) #f) ,%load
                                               ,%xp ,%zero
                                               (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))))
                                           (if ,(%inline eq? ,%td
                                                  (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                                (constant type-positive-bignum))))
                                               (goto ,Lreturn)
                                               (goto ,Lnegative)))))
                                (label ,Lnegative)
                                (set! ,%ac0 ,(%inline -/eq (immediate 0) ,%ac0))
                                (if (inline ,(make-info-condition-code 'eq? #f #t) ,%condition-code)
                                    (set! ,ac1 ,(%inline - (immediate 0) ,ac1))
                                    (set! ,ac1 ,(%inline lognot ,ac1)))
                                (label ,Lreturn))))]
                         [(= (* (constant bigit-bits) 2) (constant ptr-bits) size)
                          (let ([Lnegative (make-local-label 'Lnegative)] [Lreturn (make-local-label 'Lreturn)])
                            (%seq
                              (set! ,%xp ,%ac0)
                              (set! ,%td ,(%mref ,%xp ,(constant bignum-type-disp)))
                              (set! ,%ac0
                                (inline ,(make-info-load (bigit-type) #f) ,%load
                                  ,%xp ,%zero
                                  ,(%constant bignum-data-disp)))
                              (if ,(%inline eq? ,%td
                                     (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                   (constant type-positive-bignum))))
                                  (goto ,Lreturn)
                                  (if ,(%inline eq? ,%td
                                         (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                       (constant type-negative-bignum))))
                                      (goto ,Lnegative)
                                      ,(%seq
                                         (set! ,%xp
                                           (inline ,(make-info-load (bigit-type) #f) ,%load
                                             ,%xp ,%zero
                                             (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))))
                                         (set! ,%ac0
                                           ,(%inline sll ,%ac0 ,(%constant bigit-bits)))
                                         (set! ,%ac0 ,(%inline logor ,%ac0 ,%xp))
                                         (if ,(%inline eq? ,%td
                                                (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                              (constant type-positive-bignum))))
                                             (goto ,Lreturn)
                                             (goto ,Lnegative)))))
                              (label ,Lnegative)
                              (set! ,%ac0 ,(%inline - (immediate 0) ,%ac0))
                              (label ,Lreturn)))]
                         [else (sorry! name "cannot handle size ~s" size)])
                      (asm-return ,return-live* ...)))))))
        (define make-dofretint
          (lambda (name size entry-live* return-live*)
            (with-output-language (L13.5 CaseLambdaExpr)
              `(lambda ,(make-info name '()) 0 ()
                 ,(asm-enter
                    (%seq
                      (check-live ,entry-live* ...)
                      ,(cond
                         [(= (constant bigit-bits) size)
                          (%seq
                            (set! ,%xp
                              ,(%constant-alloc type-typed-object
                                 (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                 #f #t))
                            (if ,(%inline < ,%ac0 (immediate 0))
                                ,(%seq
                                   (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                     (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                   (constant type-negative-bignum))))
                                   (set! ,%ac0 ,(%inline - (immediate 0) ,%ac0)))
                                (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                  (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                (constant type-positive-bignum)))))
                            (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                              ,(%constant bignum-data-disp) ,%ac0)
                            (set! ,%ac0 ,%xp))]
                         [(= (* (constant bigit-bits) 2) (* (constant ptr-bits) 2) size)
                          (let ([ac1 (in-context Lvalue (ref-reg %ac1))]
                                [Lstore1 (make-local-label 'Lstore1)]
                                [Lstore2 (make-local-label 'Lstore2)])
                            (%seq
                              (if ,(%inline < ,ac1 (immediate 0))
                                  ,(%seq
                                     (set! ,ac1 ,(%inline lognot ,ac1))
                                     (set! ,%ac0 ,(%inline - (immediate 0) ,%ac0))
                                     ; TODO: use condition code here
                                     (if (if ,(%inline eq? ,%ac0 (immediate 0))
                                             ,(%seq
                                                (set! ,ac1 ,(%inline + ,ac1 (immediate 1)))
                                                (false))
                                             ,(%inline eq? ,ac1 (immediate 0)))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                            (constant type-negative-bignum))))
                                            (goto ,Lstore1))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                            (constant type-negative-bignum))))
                                            (goto ,Lstore2))))
                                  ; TODO: use condition code here
                                  (if ,(%inline eq? ,ac1 (immediate 0))
                                      ,(%seq
                                         (set! ,%xp
                                           ,(%constant-alloc type-typed-object
                                              (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                              #f #t))
                                         (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                           (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                         (constant type-positive-bignum))))
                                         (label ,Lstore1)
                                         (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                           ,(%constant bignum-data-disp) ,%ac0))
                                      ,(%seq
                                         (set! ,%xp
                                           ,(%constant-alloc type-typed-object
                                              (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                              #f #t))
                                         (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                           (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                         (constant type-positive-bignum))))
                                         (label ,Lstore2)
                                         (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                           ,(%constant bignum-data-disp)
                                           ,ac1)
                                         (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                           (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))
                                           ,%ac0))))
                              (set! ,%ac0 ,%xp)))]
                         [(= (* (constant bigit-bits) 2) (constant ptr-bits) size)
                          (let ([Lstore1 (make-local-label 'Lstore1)] [Lstore2 (make-local-label 'Lstore2)])
                            (%seq
                              (if ,(%inline < ,%ac0 (immediate 0))
                                  ,(%seq
                                     (set! ,%ac0 ,(%inline - (immediate 0) ,%ac0))
                                     (set! ,%td ,(%inline srl ,%ac0
                                                   ,(%constant bigit-bits)))
                                     (if ,(%inline eq? ,%td (immediate 0))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                            (constant type-negative-bignum))))
                                            (goto ,Lstore1))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                            (constant type-negative-bignum))))
                                            (goto ,Lstore2))))
                                  ,(%seq
                                     (set! ,%td ,(%inline srl ,%ac0
                                                   ,(%constant bigit-bits)))
                                     (if ,(%inline eq? ,%td (immediate 0))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                            (constant type-positive-bignum))))
                                            (label ,Lstore1)
                                            (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                              ,(%constant bignum-data-disp) ,%ac0))
                                         ,(%seq
                                            (set! ,%xp
                                              ,(%constant-alloc type-typed-object
                                                 (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                                 #f #t))
                                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                              (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                            (constant type-positive-bignum))))
                                            (label ,Lstore2)
                                            (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                              ,(%constant bignum-data-disp)
                                              ,%td)
                                            (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                              (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))
                                              ,%ac0)))))
                              (set! ,%ac0 ,%xp)))]
                         [else (sorry! name "cannot handle size ~s" size)])
                      (asm-return ,return-live* ...)))))))
        (define make-dofretuns
          (lambda (name size entry-live* return-live*)
            (with-output-language (L13.5 CaseLambdaExpr)
              `(lambda ,(make-info name '()) 0 ()
                 ,(asm-enter
                    (%seq
                      (check-live ,entry-live* ...)
                      ,(cond
                         [(= (constant bigit-bits) size)
                          (%seq
                            (set! ,%xp
                              ,(%constant-alloc type-typed-object
                                 (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                 #f #t))
                            (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                              (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                            (constant type-positive-bignum))))
                            (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                              ,(%constant bignum-data-disp) ,%ac0)
                            (set! ,%ac0 ,%xp))]
                         [(= (* (constant bigit-bits) 2) (* (constant ptr-bits) 2) size)
                          (let ([ac1 (in-context Lvalue (ref-reg %ac1))])
                            (%seq
                              (if ,(%inline eq? ,ac1 (immediate 0))
                                  ,(%seq
                                     (set! ,%xp
                                       ,(%constant-alloc type-typed-object
                                          (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                          #f #t))
                                     (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                       (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                     (constant type-positive-bignum))))
                                     (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                       ,(%constant bignum-data-disp) ,%ac0))
                                  ,(%seq
                                     (set! ,%xp
                                       ,(%constant-alloc type-typed-object
                                          (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                          #f #t))
                                     (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                       (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                     (constant type-positive-bignum))))
                                     (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                       ,(%constant bignum-data-disp)
                                       ,ac1)
                                     (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                       (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))
                                       ,%ac0)))
                              (set! ,%ac0 ,%xp)))]
                         [(= (* (constant bigit-bits) 2) (constant ptr-bits) size)
                          (%seq
                            (set! ,%td ,(%inline srl ,%ac0
                                          ,(%constant bigit-bits)))
                            (if ,(%inline eq? ,%td (immediate 0))
                                ,(%seq
                                   (set! ,%xp
                                     ,(%constant-alloc type-typed-object
                                        (fx+ (constant header-size-bignum) (constant bigit-bytes))
                                        #f #t))
                                   (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                     (immediate ,(fx+ (fxsll 1 (constant bignum-length-offset))
                                                   (constant type-positive-bignum))))
                                   (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                     ,(%constant bignum-data-disp) ,%ac0))
                                ,(%seq
                                   (set! ,%xp
                                     ,(%constant-alloc type-typed-object
                                        (fx+ (constant header-size-bignum) (fx* (constant bigit-bytes) 2))
                                        #f #t))
                                   (set! ,(%mref ,%xp ,(constant bignum-type-disp))
                                     (immediate ,(fx+ (fxsll 2 (constant bignum-length-offset))
                                                   (constant type-positive-bignum))))
                                   (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                     ,(%constant bignum-data-disp)
                                     ,%td)
                                   (inline ,(make-info-load (bigit-type) #f) ,%store ,%xp ,%zero
                                     (immediate ,(fx+ (constant bignum-data-disp) (constant bigit-bytes)))
                                     ,%ac0)))
                            (set! ,%ac0 ,%xp))]
                         [else (sorry! name "cannot handle size ~s" size)])
                      (asm-return ,return-live* ...)))))))
        (define make-dofretu*
          (lambda (name type size entry-live* return-live*)
            (with-output-language (L13.5 CaseLambdaExpr)
              (let ([Ltop1 (make-local-label 'Ltop1)] [Ltop2 (make-local-label 'Ltop2)])
                `(lambda ,(make-info name '()) 0 ()
                   ,(asm-enter
                      (%seq
                        (check-live ,entry-live* ...)
                        ; argument in ac0, return value in xp
                        (if ,(%inline eq? ,%ac0 (immediate 0))
                           ,(%seq
                              (set! ,%xp ,(%constant sfalse))
                              (asm-return ,return-live* ...))
                           ,(%seq
                              (set! ,%td (immediate 0))
                              (label ,Ltop1)
                              (set! ,%ts
                                (inline ,(make-info-load type #f) ,%load ,%ac0 ,%td
                                  (immediate 0)))
                              (if ,(%inline eq? ,%ts (immediate 0))
                                  (if ,(%inline eq? ,%td (immediate 0))
                                      ,(%seq
                                         (set! ,%xp (literal ,(make-info-literal #f 'object #vu8() 0)))
                                         (asm-return ,return-live* ...))
                                      ,(%seq
                                         (set! ,(ref-reg %ac1) ,%td)
                                         (set! ,%td ,(%inline + ,%td
                                                       (immediate
                                                         ,(fx+ (constant header-size-bytevector)
                                                            (fx- (constant byte-alignment) 1)))))
                                         (set! ,%td ,(%inline logand ,%td
                                                       (immediate ,(fx- (constant byte-alignment)))))
                                         (set! ,%xp (alloc ,(make-info-alloc (constant type-typed-object) #f #t) ,%td))
                                         (set! ,%td ,(ref-reg %ac1))
                                         (set! ,%td ,(%inline sll ,%td
                                                       ,(%constant bytevector-length-offset)))
                                         (set! ,%td ,(%inline logor ,%td
                                                       ,(%constant type-bytevector)))
                                         (set! ,(%mref ,%xp ,(constant bytevector-type-disp))
                                           ,%td)
                                         (set! ,%td ,(ref-reg %ac1))
                                         (label ,Ltop2)
                                         (if ,(%inline eq? ,%td (immediate 0))
                                             (asm-return ,return-live* ...)
                                             ,(%seq
                                                (set! ,%td ,(%inline - ,%td (immediate ,size)))
                                                (set! ,%ts
                                                  (inline ,(make-info-load type #f) ,%load ,%ac0 ,%td
                                                    (immediate 0)))
                                                (inline ,(make-info-load type #f) ,%store ,%xp ,%td
                                                  ,(%constant bytevector-data-disp)
                                                  ,%ts)
                                                (goto ,Ltop2)))))
                                  ,(%seq
                                     (set! ,%td ,(%inline + ,%td (immediate ,size)))
                                     (goto ,Ltop1)))))))))))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(hand-coded ,sym)
         (case sym
           [(values-procedure)
            (let ([regnum (length arg-registers)]
                  [Ltop (make-local-label 'top)])
              `(lambda ,(make-info "values" '(-1)) 0 ()
                 (if ,(%inline eq? ,%ac0 (immediate 1))
                     (seq
                       (set! ,%ac0 ,(make-arg-opnd 1))
                       (jump ,%ref-ret (,%ac0)))
                     ,(meta-cond
                        [(real-register? '%ret)
                         `(jump ,(%mref ,%ret ,(constant return-address-mv-return-address-disp))
                            (,%ac0 ,%ret ,arg-registers ...))]
                        [else
                          (%seq
                            (set! ,%xp ,%ref-ret)
                            (jump ,(%mref ,%xp
                                     ,(constant return-address-mv-return-address-disp))
                              (,%ac0 ,arg-registers ... ,(get-fv 0))))]))))]
           [($apply-procedure)
            (let ([Lloop (make-local-label 'loop)]
                  [Ldone (make-local-label 'done)])
              `(lambda ,(make-info "$apply" '(3)) 0 ()
                 ,(%seq
                    (set! ,(ref-reg %cp) ,(make-arg-opnd 1))
                    (set! ,%ac0 ,(make-arg-opnd 2))
                    (set! ,%xp ,(make-arg-opnd 3))
                    ;; TODO: when fixnum-offset = log2-ptr-bytes, we can avoid an sll by saving
                    ;; %ac0 before we shift it right.
                    (set! ,%ac0 ,(%inline sra ,%ac0 ,(%constant fixnum-offset)))
                    (if ,(%inline eq? ,%ac0 (immediate 0))
                        (goto ,Ldone)
                        ,(%seq
                           (set! ,%td ,(%inline sll ,%ac0 ,(%constant log2-ptr-bytes)))
                           (set! ,%td ,(%inline + ,%td ,%sfp))
                           (if ,(%inline > ,%td ,(ref-reg %esp))
                               (seq (pariah)
                                 ,(with-saved-ret-reg
                                    (with-saved-scheme-state
                                      (in %cp %xp %ac0)
                                      (out %ac1 %yp %ts %td scheme-args extra-regs)
                                      `(inline ,(make-info-c-simple-call #f (lookup-c-entry handle-apply-overflood)) ,%c-simple-call))))
                               (nop))
                           ,(let load-regs ([regs arg-registers])
                              (if (null? regs)
                                  (%seq
                                    (set! ,%td ,%sfp)
                                    (label ,Lloop)
                                    (set! ,(%mref ,%td ,(constant ptr-bytes))
                                      ,(%mref ,%xp ,(constant pair-car-disp)))
                                    (set! ,%xp
                                      ,(%mref ,%xp ,(constant pair-cdr-disp)))
                                    (if ,(%type-check mask-nil snil ,%xp)
                                        ,(%seq (label ,Ldone) ,(do-call))
                                        ,(%seq
                                           (set! ,%td ,(%inline + ,%td ,(%constant ptr-bytes)))
                                           (goto ,Lloop))))
                                  (%seq
                                    (set! ,(car regs) ,(%mref ,%xp ,(constant pair-car-disp)))
                                    (set! ,%xp ,(%mref ,%xp ,(constant pair-cdr-disp)))
                                    (if ,(%type-check mask-nil snil ,%xp)
                                        (goto ,Ldone)
                                        ,(load-regs (cdr regs)))))))))))]
           [(list*-procedure) (make-list*-procedure "list*")]
           [(cons*-procedure) (make-list*-procedure "cons*")]
           [($record-procedure)
            (let ([Ltop (make-local-label 'ltop)])
              `(lambda ,(make-info "$record" '(-2)) 0 ()
                 (if ,(%inline eq? ,%ac0 (immediate 0))
                     (seq (pariah) (goto ,Ldoargerr))
                     ,(%seq
                        (set! ,%ac0 ,(%inline sll ,%ac0 ,(%constant log2-ptr-bytes)))
                        (set! ,%td ,(%inline + ,%ac0 (immediate ,(fx- (constant byte-alignment) 1))))
                        (set! ,%td ,(%inline logand ,%td (immediate ,(- (constant byte-alignment)))))
                        (set! ,%xp (alloc ,(make-info-alloc (constant type-typed-object) #f #f) ,%td))
                        ,(let f ([reg* arg-registers] [i 0])
                           (if (null? reg*)
                               (%seq
                                 ; point xp to last element of record
                                 (set! ,%xp
                                   ,(%lea ,%xp ,%ac0 (fx- (constant ptr-bytes))))
                                 ; point ac0 to last stack argument
                                 (set! ,%ac0
                                   ,(%lea ,%sfp ,%ac0
                                      (fx* i (fx- (constant ptr-bytes)))))
                                 (label ,Ltop)
                                 (set! ,(%mref ,%xp ,(constant record-type-disp))
                                   ,(%mref ,%ac0 0))
                                 (set! ,%ac0 ,(%inline - ,%ac0 ,(%constant ptr-bytes)))
                                 (if ,(%inline eq? ,%ac0 ,%sfp)
                                     ,(%seq
                                        (set! ,%ac0 ,(%inline - ,%xp (immediate ,(fx* i (constant ptr-bytes)))))
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(%seq
                                        (set! ,%xp ,(%inline - ,%xp ,(%constant ptr-bytes)))
                                        (goto ,Ltop))))
                               (%seq
                                 (set! ,(%mref ,%xp
                                          ,(fx+ (fx* i (constant ptr-bytes)) (constant record-type-disp)))
                                   ,(car reg*))
                                 (if ,(%inline eq? ,%ac0 (immediate ,(fx* (fx+ i 1) (constant ptr-bytes))))
                                     ,(%seq
                                        (set! ,%ac0 ,%xp)
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(f (cdr reg*) (fx+ i 1))))))))))]
           [(vector-procedure)
            (let ([Ltop (make-local-label 'ltop)])
              `(lambda ,(make-info "vector" '(-1)) 0 ()
                 (if ,(%inline eq? ,%ac0 (immediate 0))
                     ,(%seq
                        (set! ,%ac0 (literal ,(make-info-literal #f 'object '#() 0)))
                        (jump ,%ref-ret (,%ac0)))
                     ,(%seq
                        (set! ,%ac0 ,(%inline sll ,%ac0 ,(%constant log2-ptr-bytes)))
                        (set! ,%td ,(%inline + ,%ac0 (immediate ,(fx+ (constant ptr-bytes) (fx- (constant byte-alignment) 1)))))
                        (set! ,%td ,(%inline logand ,%td (immediate ,(- (constant byte-alignment)))))
                        (set! ,%xp (alloc ,(make-info-alloc (constant type-typed-object) #f #f) ,%td))
                        ,(let ([delta (fx- (constant vector-length-offset) (constant log2-ptr-bytes))])
                           (safe-assert (fx>= delta 0))
                           (if (fx= delta 0)
                               (if (fx= (constant type-vector) 0)
                                   `(set! ,(%mref ,%xp ,(constant vector-type-disp)) ,%ac0)
                                   (%seq
                                     (set! ,%td ,(%inline logor ,%ac0 (immediate ,(constant type-vector))))
                                     (set! ,(%mref ,%xp ,(constant vector-type-disp)) ,%td)))
                               (%seq
                                 (set! ,%td ,(%inline sll ,%ac0 (immediate ,delta)))
                                 ,(if (fx= (constant type-vector) 0)
                                      `(set! ,(%mref ,%xp ,(constant vector-type-disp)) ,%td)
                                      (%seq
                                        (set! ,%td ,(%inline logor ,%td (immediate ,(constant type-vector))))
                                        (set! ,(%mref ,%xp ,(constant vector-type-disp)) ,%td))))))
                        ,(let f ([reg* arg-registers] [i 0])
                           (if (null? reg*)
                               (%seq
                                 ; point xp to last element of vector
                                 (set! ,%xp ,(%inline + ,%xp ,%ac0))
                                 ; point ac0 to last stack argument
                                 (set! ,%ac0
                                   ,(%lea ,%sfp ,%ac0
                                      (fx* i (fx- (constant ptr-bytes)))))
                                 (label ,Ltop)
                                 (set! ,(%mref ,%xp ,(fx- (constant vector-data-disp) (constant ptr-bytes)))
                                   ,(%mref ,%ac0 0))
                                 (set! ,%ac0 ,(%inline - ,%ac0 ,(%constant ptr-bytes)))
                                 (if ,(%inline eq? ,%ac0 ,%sfp)
                                     ,(%seq
                                        (set! ,%ac0 ,(%inline - ,%xp (immediate ,(fx* (fx+ i 1) (constant ptr-bytes)))))
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(%seq
                                        (set! ,%xp ,(%inline - ,%xp ,(%constant ptr-bytes)))
                                        (goto ,Ltop))))
                               (%seq
                                 (set! ,(%mref ,%xp
                                          ,(fx+ (fx* i (constant ptr-bytes)) (constant vector-data-disp)))
                                   ,(car reg*))
                                 (if ,(%inline eq? ,%ac0 (immediate ,(fx* (fx+ i 1) (constant ptr-bytes))))
                                     ,(%seq
                                        (set! ,%ac0 ,%xp)
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(f (cdr reg*) (fx+ i 1))))))))))]
           [(list-procedure)
            (let ([Ltop (make-local-label 'ltop)])
              `(lambda ,(make-info "list" '(-1)) 0 ()
                 (if ,(%inline eq? ,%ac0 (immediate 0))
                     (seq
                       (set! ,%ac0 ,(%constant snil))
                       (jump ,%ref-ret (,%ac0)))
                     ,(%seq
                        (set! ,%ac0 ,(%inline sll ,%ac0 ,(%constant pair-shift)))
                        (set! ,%xp (alloc ,(make-info-alloc (constant type-pair) #f #f) ,%ac0))
                        ,(let f ([reg* arg-registers] [i 0])
                           (if (null? reg*)
                               ; filled in first i pairs
                               ; have at least one stack argument
                               ; ac0 is amount allocated, or size-pair * # elements
                               (%seq
                                 ; point xp to last pair of list
                                 (set! ,%xp
                                   ,(%lea ,%xp ,%ac0 (fx- (constant size-pair))))
                                 ; adjust from two ptrs per pair to one ptr per stack element
                                 (set! ,%ac0
                                   ,(%inline srl ,%ac0 (immediate 1)))
                                 ; point ac0 to last stack argument
                                 (set! ,%ac0
                                   ,(%lea ,%sfp ,%ac0
                                      (fx* i (fx- (constant ptr-bytes)))))
                                 (set! ,(%mref ,%xp ,(constant pair-cdr-disp))
                                   ,(%constant snil))
                                 (label ,Ltop)
                                 (set! ,(%mref ,%xp ,(constant pair-car-disp))
                                   ,(%mref ,%ac0 0))
                                 (set! ,%ac0 ,(%inline - ,%ac0 ,(%constant ptr-bytes)))
                                 (if ,(%inline eq? ,%ac0 ,%sfp)
                                     ,(%seq
                                        (set! ,%ac0 ,(%inline - ,%xp (immediate ,(fx* i (constant size-pair)))))
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(%seq
                                        (set! ,(%mref ,%xp ,(fx- (constant pair-cdr-disp) (constant size-pair)))
                                          ,%xp)
                                        (set! ,%xp ,(%inline - ,%xp ,(%constant size-pair)))
                                        (goto ,Ltop))))
                               (%seq
                                 (set! ,(%mref ,%xp
                                          ,(fx+ (fx* i (constant size-pair)) (constant pair-car-disp)))
                                   ,(car reg*))
                                 (if ,(%inline eq? ,%ac0 (immediate ,(fx* (fx+ i 1) (constant size-pair))))
                                     ,(%seq
                                        (set! ,(%mref ,%xp
                                                 ,(fx+ (fx* i (constant size-pair)) (constant pair-cdr-disp)))
                                          ,(%constant snil))
                                        (set! ,%ac0 ,%xp)
                                        (jump ,%ref-ret (,%ac0)))
                                     ,(%seq
                                        (set! ,(%mref ,%xp
                                                 ,(fx+ (fx* i (constant size-pair)) (constant pair-cdr-disp)))
                                          ,(%inline + ,%xp
                                             (immediate ,(fx* (fx+ i 1) (constant size-pair)))))
                                        ,(f (cdr reg*) (fx+ i 1)))))))))))]
           [($instantiate-code-object)
            `(lambda ,(make-info "$instantiate-code-object" '(3)) 0 ()
               ,(%seq
                  ,(with-saved-ret-reg
                     (%seq
                       ,(save-scheme-state
                          (in scheme-args)
                          (out %ac0 %ac1 %cp %xp %yp %ts %td extra-regs))
                       (inline ,(make-info-c-simple-call #f (lookup-c-entry instantiate-code-object))
                         ,%c-simple-call)
                       ,(restore-scheme-state
                          (in %ac0)
                          (out %ac1 %cp %xp %yp %ts %td scheme-args extra-regs))))
                  (jump ,%ref-ret (,%ac0))))]
           [(values-error) (make-do/call (in-context Lvalue (%tc-ref ret)) "values-error" (lookup-c-entry handle-values-error))]
           [(domvleterr) (make-do/call (in-context Lvalue (%tc-ref ret)) "domvleterr" (lookup-c-entry handle-mvlet-error))]
           [(doargerr) (make-do/call (in-context Lvalue (%tc-ref ret)) "doargerr" (lookup-c-entry handle-arg-error))]
           [(call-error) (make-do/call (in-context Lvalue (%tc-ref ret)) "call-error" (lookup-c-entry handle-docall-error))]
           [(dooverflow) ((make-do/ret (intrinsic-entry-live* dooverflow) (intrinsic-return-live* dooverflow)) #f "dooverflow" (lookup-c-entry handle-overflow))]
           [(dooverflood) ((make-do/ret (intrinsic-entry-live* dooverflood) (intrinsic-return-live* dooverflood)) #f "dooverflood" (lookup-c-entry handle-overflood))]
           [(scan-remembered-set) ((make-do/ret (intrinsic-entry-live* scan-remembered-set) (intrinsic-return-live* scan-remembered-set)) (in-context Lvalue (%tc-ref ret)) "scan-remembered-set" (lookup-c-entry scan-remembered-set))]
           [(get-room) ((make-do/ret (intrinsic-entry-live* get-room) (intrinsic-return-live* get-room)) (in-context Lvalue (%tc-ref ret)) "get-room" (lookup-c-entry get-more-room))]
           [(nonprocedure-code)
            `(lambda ,(make-info "nonprocedure-code" '()) 0 ()
               ,(%seq
                  (set! ,%td ,(%mref ,%xp ,(constant symbol-value-disp)))
                  (if ,(%type-check mask-closure type-closure ,%td)
                      (seq
                        (set! ,(ref-reg %cp) ,%td)
                        (set! ,(%mref ,%xp ,(constant symbol-pvalue-disp))
                          ,(%mref ,%td ,(constant closure-code-disp))))
                      ,(with-saved-ret-reg
                         (with-saved-scheme-state
                           (in %ac0 %ac1 %cp %xp %yp scheme-args)
                           (out %ts %td extra-regs)
                           `(inline ,(make-info-c-simple-call #f (lookup-c-entry handle-nonprocedure-symbol))
                              ,%c-simple-call))))
                  ,(do-call)))]
           [($foreign-entry-procedure)
            `(lambda ,(make-info "$foreign-entry" '(1)) 0 ()
               ,(%seq
                  (set! ,%ac0 ,(make-arg-opnd 1))
                  ,(with-saved-ret-reg
                     (with-saved-scheme-state
                       (in %ac0)
                       (out %cp %xp %yp %ac1 %ts %td scheme-args extra-regs)
                       `(inline ,(make-info-c-simple-call #f (lookup-c-entry foreign-entry))
                          ,%c-simple-call)))
                  (jump ,%ref-ret (,%ac0))))]
           [($install-library-entry-procedure)
            `(lambda ,(make-info "$install-library-entry" '(2)) 0 ()
               ,(%seq
                  ,(with-saved-ret-reg
                     (%seq
                       ,(save-scheme-state
                          (in scheme-args)
                          (out %ac0 %ac1 %cp %xp %yp %ts %td extra-regs))
                       (inline ,(make-info-c-simple-call #f (lookup-c-entry install-library-entry))
                         ,%c-simple-call)
                       ,(restore-scheme-state
                          (in)
                          (out %ac0 %ac1 %cp %xp %yp %ts %td scheme-args extra-regs))))
                  (set! ,%ac0 ,(%constant svoid))
                  (jump ,%ref-ret (,%ac0))))]
           [(bytevector=?)
            (let ([bv1 (make-tmp 'bv1)] [bv2 (make-tmp 'bv2)] [idx (make-tmp 'idx)] [len2 (make-tmp 'len2)])
              (define (argcnt->max-fv n) (max (- n (length arg-registers)) 0))
              (let ([Ltop (make-local-label 'Ltop)] [Ltrue (make-local-label 'Ltrue)] [Lfail (make-local-label 'Lfail)])
                (define iptr-bytes (in-context Triv (%constant ptr-bytes)))
                `(lambda ,(make-info "bytevector=?" '(2)) ,(argcnt->max-fv 2) (,bv1 ,bv2 ,idx ,len2)
                   ,(%seq
                      (set! ,bv1 ,(make-arg-opnd 1))
                      (set! ,bv2 ,(make-arg-opnd 2))
                      (if ,(%inline eq? ,bv1 ,bv2)
                          (goto ,Ltrue)
                          ,(%seq
                             (set! ,idx ,(%inline srl
                                           ,(%mref ,bv1 ,(constant bytevector-type-disp))
                                           ,(%constant bytevector-length-offset)))
                             (set! ,len2 ,(%inline srl
                                            ,(%mref ,bv2 ,(constant bytevector-type-disp))
                                            ,(%constant bytevector-length-offset)))
                             (if ,(%inline eq? ,len2 ,idx)
                                 ,(%seq
                                    (label ,Ltop)
                                    (if ,(%inline >= ,idx ,iptr-bytes)
                                        (if ,(%inline eq?
                                               ,(%mref ,bv1 ,(constant bytevector-data-disp))
                                               ,(%mref ,bv2 ,(constant bytevector-data-disp)))
                                            ,(%seq
                                               (set! ,idx ,(%inline - ,idx ,iptr-bytes))
                                               (set! ,bv1 ,(%inline + ,bv1 ,iptr-bytes))
                                               (set! ,bv2 ,(%inline + ,bv2 ,iptr-bytes))
                                               (goto ,Ltop))
                                            (goto ,Lfail))
                                        (if (if ,(%inline eq? ,idx (immediate 0))
                                                (true)
                                                ,(%seq
                                                   (set! ,bv1 ,(%mref ,bv1 ,(constant bytevector-data-disp)))
                                                   (set! ,bv2 ,(%mref ,bv2 ,(constant bytevector-data-disp)))
                                                   (set! ,idx ,(%inline - ,iptr-bytes ,idx))
                                                   (set! ,idx ,(%inline sll ,idx (immediate 3)))
                                                   ,(constant-case native-endianness
                                                      [(little)
                                                       (%seq
                                                         (set! ,bv1 ,(%inline sll ,bv1 ,idx))
                                                         (set! ,bv2 ,(%inline sll ,bv2 ,idx)))]
                                                      [(big)
                                                       (%seq
                                                         (set! ,bv1 ,(%inline srl ,bv1 ,idx))
                                                         (set! ,bv2 ,(%inline srl ,bv2 ,idx)))])
                                                   ,(%inline eq? ,bv1 ,bv2)))
                                            ,(%seq
                                               (label ,Ltrue)
                                               (set! ,%ac0 ,(%constant strue))
                                               (jump ,%ref-ret (,%ac0)))
                                            (goto ,Lfail))))
                                 ,(%seq
                                    (label ,Lfail)
                                    (set! ,%ac0 ,(%constant sfalse))
                                    (jump ,%ref-ret (,%ac0))))))))))]
           [(dofargint32) (make-dofargint "dofargint32" 32 (intrinsic-entry-live* dofargint32) (intrinsic-return-live* dofargint32))]
           [(dofargint64) (make-dofargint "dofargint64" 64 (intrinsic-entry-live* dofargint64) (intrinsic-return-live* dofargint64))]
           [(dofretint32) (make-dofretint "doretint32" 32 (intrinsic-entry-live* dofretint32) (intrinsic-return-live* dofretint32))]
           [(dofretint64) (make-dofretint "doretint64" 64 (intrinsic-entry-live* dofretint64) (intrinsic-return-live* dofretint64))]
           [(dofretuns32) (make-dofretuns "doretuns32" 32 (intrinsic-entry-live* dofretuns32) (intrinsic-return-live* dofretuns32))]
           [(dofretuns64) (make-dofretuns "doretuns64" 64 (intrinsic-entry-live* dofretuns64) (intrinsic-return-live* dofretuns64))]
           [(dofretu8*) (make-dofretu* "dofretu8*" 'unsigned-8 1 (intrinsic-entry-live* dofretu8*) (intrinsic-return-live* dofretu8*))]
           [(dofretu16*) (make-dofretu* "dofretu16*" 'unsigned-16 2 (intrinsic-entry-live* dofretu16*) (intrinsic-return-live* dofretu16*))]
           [(dofretu32*) (make-dofretu* "dofretu32*" 'unsigned-32 4 (intrinsic-entry-live* dofretu32*) (intrinsic-return-live* dofretu32*))]
           [(error-invoke) ; more generally "tail-reentry"
            `(lambda ,(make-info "error-invoke" '()) 0 ()
               ,(%seq
                  ,(%inline invoke-prelude)
                  ,(restore-scheme-state
                     (in %ac0 %ac1 %cp %xp %yp scheme-args)
                     (out %ts %td extra-regs))
                  ,(meta-cond
                     [(real-register? '%ret) `(set! ,%ret ,(%mref ,%sfp 0))]
                     [else `(nop)])
                  ,(do-call)))]
           [(invoke)
            (let ([Lret (make-local-label 'Lret)]
                  [Lexit (make-local-label 'Lexit)]
                  [Lmvreturn (make-local-label 'Lmvreturn)])
              `(lambda ,(make-info "invoke" '()) 0 ()
                 ,(%seq
                    ; TODO: add alignment
                    #;(asm align) ; must start aligned or align below may fail
                    ,(%inline invoke-prelude)
                    ,(restore-scheme-state
                       (in %ac0 %cp scheme-args)
                       (out %ac1 %xp %yp %ts %td extra-regs))
                    (new-frame ,(make-info-newframe #f #f '() '() '()) ,'() ... ,Lret)
                    ; NB: hack!!!
                    (set! ,%sfp ,(%inline - ,%sfp (immediate ,(constant ptr-bytes))))
                    (set! ,%ref-ret (label-ref ,Lret ,(constant size-rp-header)))
                    (tail ,(do-call)) ; argcnt already in ac0
                    #;(asm align)
                    (label ,Lret)
                    (rp-header ,Lmvreturn ,(* 2 (constant ptr-bytes)) 1) ; cchain is live at sfp[ptr-bytes]
                    (set! ,(ref-reg %ac1) (immediate 1)) ; single-value as expected
                    ,(save-scheme-state
                       (in %ac0 %ac1)
                       (out %cp %xp %yp %ts %td scheme-args extra-regs))
                    (label ,Lexit)
                    (inline ,(make-info-c-simple-call #f (lookup-c-entry Sreturn)) ,%c-simple-call)
                    (label ,Lmvreturn)
                    (set! ,(ref-reg %ac1) ,%ac0)
                    ,(save-scheme-state
                       (in %ac0 %ac1 scheme-args)
                       (out %cp %xp %yp %ts %td extra-regs))
                    (goto ,Lexit))))]
           [else ($oops who "unrecognized hand-coded name ~s" sym)])]))

    (define-pass np-expose-allocation-pointer : L13.5 (ir) -> L14 ()
      ; NB: uses %ts when %ap is not a real register
      ; NB: should use an unspillable, but we don't have unspillables yet
      (definitions
        (define local*)
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-tmp))
            (let ([x (make-tmp x)])
              (set! local* (cons x local*))
              x)))
        (define refap (with-output-language (L14 Triv) (ref-reg %ap)))
        (define refeap (with-output-language (L14 Triv) (ref-reg %eap)))
        (with-output-language (L14 Effect)
          (define build-alloc
            (lambda (info lvalue t)
              (let ([Lget-room (make-local-label 'Lget-room)])
                ((lambda (p)
                   (meta-cond
                     [(real-register? '%ap) (p %ap values)]
                     [else `(seq (set! ,%ts ,refap) ,(p %ts (lambda (e) `(seq ,e (set! ,refap ,%ts)))))]))
                 (lambda (ap store-ap)
                   (%seq
                     (set! ,%xp ,(%inline + ,ap (immediate ,(- (info-alloc-tag info) (constant typemod)))))
                     ,(nanopass-case (L14 Triv) t
                        [(immediate ,imm)
                         (guard (fixnum? imm) (fx< imm (constant bytes-per-segment)))
                         ; reset_allocation_pointer never uses the last segment of the address
                         ; space, so we can allocate less than bytes-per-segment w/o carry check
                         (store-ap `(set! ,ap ,(%inline + ,ap ,t)))]
                        [else
                         (%seq
                           ,(store-ap `(set! ,ap ,(%inline +/carry ,ap ,t)))
                           (if (inline ,(make-info-condition-code 'carry #f #t) ,%condition-code)
                               (goto ,Lget-room)
                               (nop)))])
                     (if ,(%inline u< ,refeap ,ap)
                         ,(%seq
                            (label ,Lget-room)
                            (pariah)
                            ,((lambda (e)
                                (if (info-alloc-save-flrv? info)
                                    (%seq ,(%inline save-flrv) ,e ,(%inline restore-flrv))
                                    e))
                              `(set! ,%xp (inline ,(intrinsic-info-asmlib get-room (info-alloc-save-ra? info)) ,%asmlibcall))))
                         (nop))
                     (set! ,lvalue ,%xp)))))))
          (define (build-inc-cc-counter arg)
            (%inline inc-cc-counter ,%tc ,(%constant tc-alloc-counter-disp) ,arg))
          (define (build-shift-and-inc-cc-counter t)
            (let ([tcnt (make-tmp 'tcnt)])
              (%seq
                (set! ,tcnt ,(%inline sra ,t ,(%constant log2-ptr-bytes)))
                ,(build-inc-cc-counter tcnt))))
          (define alloc-helper
            (lambda (info lvalue t)
              (if (generate-allocation-counts)
                  (nanopass-case (L14 Triv) t
                    [(immediate ,imm)
                     (%seq
                       ,(build-inc-cc-counter
                          (in-context Triv
                            `(immediate ,(fxsra imm (constant log2-ptr-bytes)))))
                       ,(build-alloc info lvalue t))]
                    [else
                     (if (var? t)
                         (%seq ,(build-shift-and-inc-cc-counter t) ,(build-alloc info lvalue t))
                         (let ([talloc (make-tmp 'talloc)])
                           (%seq
                             (set! ,talloc ,t)
                             ,(build-shift-and-inc-cc-counter talloc)
                             ,(build-alloc info lvalue talloc))))])
                  (build-alloc info lvalue t))))))
      (Effect : Effect (ir) -> Effect ()
        [(inline ,info ,effect-prim ,t)
         (guard (eq? effect-prim %remember))
         (if (real-register? '%eap)
             (%seq
               (if ,(%inline u< ,refap ,refeap)
                   (nop)
                   (seq
                     (pariah)
                     (inline ,(intrinsic-info-asmlib scan-remembered-set #f) ,%asmlibcall!)))
               (set! ,refeap ,(%inline - ,refeap ,(%constant ptr-bytes)))
               ; write through to tc so dirty-list bounds are always known in case of an
               ; invalid memory reference or illegal instruction
               (set! (mref ,%tc ,%zero ,(tc-disp %eap)) ,refeap)
               (set! ,(%mref ,refeap 0) ,t))
             (%seq
               (set! ,%td ,refeap)
               (if ,(%inline u< ,refap ,%td)
                   (nop)
                   ,(%seq
                      (pariah)
                      (inline ,(intrinsic-info-asmlib scan-remembered-set #f) ,%asmlibcall!)
                      (set! ,%td ,refeap)))
               (set! ,%td ,(%inline - ,%td ,(%constant ptr-bytes)))
               (set! ,refeap ,%td)
               (set! ,(%mref ,%td 0) ,t)))]
        [(set! ,lvalue (alloc ,info ,[t])) (alloc-helper info lvalue t)])
      (Tail : Tail (ir) -> Tail ())
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local0* ...) ,tlbody)
         (fluid-let ([local* local0*])
           (let ([tlbody (Tail tlbody)])
             `(lambda ,info ,max-fv (,local* ...) ,tlbody)))]))

    (define-record-type goto-block
      (parent block)
      (fields (mutable next))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (rec make-goto-block
            (case-lambda
              [() (make-goto-block #f)]
              [(next) ((pargs->new) next)])))))

    (define-record-type if-block
      (parent block)
      (fields
        (mutable pred)
        (mutable true)
        (mutable false)
        (mutable live-out))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda (true false)
            ((pargs->new) #f true false 'uninitialized)))))

    (define-record-type newframe-block
      (parent block)
      (fields
        info
        (mutable next)
        (mutable rp*)
        (mutable rp)
        (mutable live-rp)
        (mutable live-call)
        (mutable live-out))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda (info next)
            ((pargs->new) info next #f #f 'uninitialized 'uninitialized 'uninitialized)))))

    (define-record-type joto-block
      (parent block)
      (fields nfv* (mutable next))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda (nfv*)
            ((pargs->new) nfv* #f)))))

    (define-record-type tail-block
      (parent block)
      (fields (mutable tail) (mutable exit))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda ()
            ((pargs->new) #f #f)))))

    (define-record-type bcache
      (fields effect*)
      (nongenerative)
      (protocol
        (lambda (new)
          (lambda (block)
            (new (block-effect* block))))))

    (define-record-type if-bcache
      (parent bcache)
      (fields pred)
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda (block)
            ((pargs->new block) (if-block-pred block))))))

    (define-record-type tail-bcache
      (parent bcache)
      (fields tail)
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (pargs->new)
          (lambda (block)
            ((pargs->new block) (tail-block-tail block))))))

    (define-who cache-block-info
      (lambda (block)
        (cond
          [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (make-bcache block)]
          [(if-block? block) (make-if-bcache block)]
          [(tail-block? block) (make-tail-bcache block)]
          [else (sorry! who "unrecognized block ~s" block)])))

    (define-who restore-block-info!
      (lambda (block bcache)
        (block-effect*-set! block (bcache-effect* bcache))
        (cond
          [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (void)]
          [(if-block? block) (if-block-pred-set! block (if-bcache-pred bcache))]
          [(tail-block? block) (tail-block-tail-set! block (tail-bcache-tail bcache))]
          [else (sorry! who "unrecognized block ~s" block)])))

    (define-pass np-expose-basic-blocks : L14 (ir) -> L15a ()
      (definitions
        (define add-instr!
          (lambda (block ir)
            (block-effect*-set! block (cons ir (block-effect* block)))))

        (define add-label-link!
          (lambda (from l setter)
            (let ([x (local-label-block l)])
              (if (block? x)
                  (setter from x)
                  (local-label-block-set! l (cons (lambda (to) (setter from to)) (or x '())))))))

        (define resolve-waiting-links!
          (lambda (l to)
            (let ([x (local-label-block l)])
              (safe-assert (not (block? x)))
              (when x (for-each (lambda (add-link!) (add-link! to)) x))
              (local-label-block-set! l to))))

        (define-pass build-graph : (L14 Tail) (ir) -> * (block block*)
          (definitions
            (define add-goto-block
              (lambda (l block*)
                (if (local-label? l)
                    (let ([block (make-goto-block)])
                      (add-label-link! block l goto-block-next-set!)
                      (values block (cons block block*)))
                    (let ([block (make-tail-block)])
                      (tail-block-tail-set! block (with-output-language (L15a Tail) `(goto ,l)))
                      (values block (cons block block*))))))
            (define add-true/false-block
              (lambda (target block* label-name)
                (let ([block (make-goto-block target)])
                  (unless (block-label target)
                    (block-label-set! target (make-local-label label-name)))
                  (values block (cons block block*))))))
          (Lvalue : Lvalue (ir target) -> * (ir)
            [,x x]
            [(mref ,x1 ,x2 ,imm) (with-output-language (L15a Lvalue) `(mref ,x1 ,x2 ,imm))])
          (Triv : Triv (ir target) -> * (ir)
            [(literal ,info) (with-output-language (L15a Triv) `(literal ,info))]
            [(immediate ,imm) (with-output-language (L15a Triv) `(immediate ,imm))]
            [,lvalue (Lvalue lvalue target)]
            [(label-ref ,l ,offset) (with-output-language (L15a Triv) `(label-ref ,l ,offset))])
          ;; TODO: framework should come up with some way of handling or complaining about a
          ;; (maybe foo) when returning from a multiple value case.
          (Rhs : Rhs (ir target) -> * (ir)
            [(inline ,info ,value-prim ,[Triv : t target -> t] ...)
             (with-output-language (L15a Rhs) `(inline ,info ,value-prim ,t ...))]
            [,t (Triv t target)])
          (Tail : Tail (ir block*) -> * (block block*)
            [(goto ,l) (add-goto-block l block*)]
            [(seq ,e0 ,[block block*]) (Effect e0 block block*)]
            [(if ,p0 ,tl1 ,[f-block block*])
             (let-values ([(t-block block*) (Tail tl1 block*)])
               (Pred p0 t-block f-block block*))]
            [(jump ,t (,var* ...))
             (let ([block (make-tail-block)])
               (tail-block-tail-set! block
                 (with-output-language (L15a Tail)
                   `(jump ,(make-live-info) ,(Triv t block) (,var* ...))))
               (values block (cons block block*)))]
            [(joto ,l (,nfv* ...))
             (let ([block (make-joto-block nfv*)])
               (add-label-link! block l joto-block-next-set!)
               (values block (cons block block*)))]
            [(asm-return ,reg* ...)
             (let ([block (make-tail-block)])
               (tail-block-tail-set! block (with-output-language (L15a Tail) `(asm-return ,reg* ...)))
               (values block (cons block block*)))]
            [(asm-c-return ,info ,reg* ...)
             (let ([block (make-tail-block)])
               (tail-block-tail-set! block (with-output-language (L15a Tail) `(asm-c-return ,info ,reg* ...)))
               (values block (cons block block*)))]
            [else ($oops who "unexpected Tail ~s" ir)])
          (Effect : Effect (ir target block*) -> * (target block*)
            [(nop) (values target block*)]
            [(inline ,info ,effect-prim ,[Triv : t target -> t] ...)
             (add-instr! target (with-output-language (L15a Effect) `(inline ,(make-live-info) ,info ,effect-prim ,t ...)))
             (values target block*)]
            [(overflow-check)
             (add-instr! target (with-output-language (L15a Effect) `(overflow-check ,(make-live-info))))
             (values target block*)]
            [(overflood-check)
             (add-instr! target (with-output-language (L15a Effect) `(overflood-check ,(make-live-info))))
             (values target block*)]
            [(fcallable-overflow-check)
             (add-instr! target (with-output-language (L15a Effect) `(fcallable-overflow-check ,(make-live-info))))
             (values target block*)]
            [(new-frame ,info ,rpl* ... ,rpl)
             (let ([block (make-newframe-block info target)] [l (make-local-label 'docall)])
               (block-label-set! target l)
               (let ([rp* (fold-left (lambda (ls rp) (cons #f ls)) '() rpl*)])
                 (newframe-block-rp*-set! block rp*)
                 (let loop ([rpl* rpl*] [rp* rp*])
                   (unless (null? rpl*)
                     (add-label-link! rp* (car rpl*) set-car!)
                     (loop (cdr rpl*) (cdr rp*)))))
               (add-label-link! block rpl newframe-block-rp-set!)
               (values block (cons block block*)))]
            [(remove-frame ,info)
             (add-instr! target (with-output-language (L15a Effect) `(remove-frame ,(make-live-info) ,info)))
             (values target block*)]
            [(restore-local-saves ,info)
             (add-instr! target (with-output-language (L15a Effect) `(restore-local-saves ,(make-live-info) ,info)))
             (values target block*)]
            [(return-point ,info ,rpl ,mrvl (,cnfv* ...))
             (add-instr! target (with-output-language (L15a Effect) `(return-point ,info ,rpl ,mrvl (,cnfv* ...))))
             (block-return-point! target #t)
             (values target block*)]
            [(rp-header ,mrvl ,fs ,lpm)
             (add-instr! target (with-output-language (L15a Effect) `(rp-header ,mrvl ,fs ,lpm)))
             (block-return-point! target #t)
             (values target block*)]
            [(shift-arg ,reg ,imm ,info)
             (add-instr! target (with-output-language (L15a Effect) `(shift-arg ,(make-live-info) ,reg ,imm ,info)))
             (values target block*)]
            [(pariah)
             (block-pariah! target #t)
             (values target block*)]
            [(profile ,src)
             (block-src*-set! target (cons src (block-src* target)))
             (values target block*)]
            [(tail ,tl) (Tail tl block*)]
            [(label ,l)
             (block-label-set! target l)
             (resolve-waiting-links! l target)
             (let ([block (make-goto-block target)])
               (values block (cons block block*)))]
            [(goto ,l) (add-goto-block l block*)]
            [(seq ,e0 ,[block block*]) (Effect e0 block block*)]
            [(set! ,[Lvalue : lvalue target -> lvalue] ,[Rhs : rhs target -> rhs])
             (add-instr! target (with-output-language (L15a Effect) `(set! ,(make-live-info) ,lvalue ,rhs)))
             (values target block*)]
            [(if ,p0 ,e1 ,e2)
             (let ([t-block (make-goto-block target)] [f-block (make-goto-block target)] [l (make-local-label 'ej)])
               (let ([block* (cons* t-block f-block block*)])
                 (block-label-set! target l)
                 (let-values ([(f-block block*) (Effect e2 f-block block*)])
                   (let-values ([(t-block block*) (Effect e1 t-block block*)])
                     (Pred p0 t-block f-block block*)))))]
            [(check-live ,reg* ...)
             (add-instr! target (with-output-language (L15a Effect) `(check-live ,(make-live-info) ,reg* ...)))
             (values target block*)]
            [else ($oops who "unexpected Effect ~s" ir)])
          (Pred : Pred (ir t-target f-target block*) -> * (block block*)
            [(true) (add-true/false-block t-target block* 'lt)]
            [(false) (add-true/false-block f-target block* 'lf)]
            [(inline ,info ,pred-prim ,t* ...)
             (let ([block (make-if-block t-target f-target)])
               (unless (block-label t-target) (block-label-set! t-target (make-local-label 'lt)))
               (unless (block-label f-target) (block-label-set! f-target (make-local-label 'lf)))
               (if-block-pred-set! block
                 (with-output-language (L15a Pred)
                   `(inline ,(make-live-info) ,info ,pred-prim ,(map (lambda (t) (Triv t block)) t*) ...)))
               (values block (cons block block*)))]
            [(seq ,e0 ,[block block*]) (Effect e0 block block*)]
            [(goto ,l) (add-goto-block l block*)]
            [(if ,p0 ,p1 ,[f-block block*])
             (let-values ([(t-block block*) (Pred p1 t-target f-target block*)])
               (Pred p0 t-block f-block block*))]
            [(mlabel ,p (,l* ,p*) ...)
             (let loop ([l* l*] [p* p*] [block* block*])
               (if (null? l*)
                   (Pred p t-target f-target block*)
                   (let-values ([(block block*) (Pred (car p*) t-target f-target block*)])
                     (let ([l (car l*)])
                       (resolve-waiting-links! l block)
                       (block-label-set! block l)
                       (loop (cdr l*) (cdr p*) block*)))))]
            [else ($oops who "unexpected Pred ~s" ir)])
          (Tail ir '())))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) ,tlbody)
         (let-values ([(entry-block block*) (build-graph tlbody)])
           (unless (block-label entry-block)
             (let ([label (make-local-label 'entry)])
               (local-label-block-set! label entry-block)
               (block-label-set! entry-block label)))
           ; NB: if entry-block is not a dcl block, it must appear first in entry-block*,
           ; NB: as it is the generic entry point for the procedure
           (let ([entry-block* (let ([block* (fold-left
                                               (lambda (block* dcl)
                                                 (let ([block (local-label-block dcl)])
                                                   (if (block? block) (cons block block*) block*)))
                                               '() (info-lambda-dcl* info))])
                                 (if (memq entry-block block*) block* (cons entry-block block*)))])
             ; mark reachable blocks
             (for-each
               (rec mark!
                 (lambda (from)
                   (unless (block-seen? from)
                     (block-seen! from #t)
                     (cond
                       [(goto-block? from) (mark! (goto-block-next from))]
                       [(joto-block? from) (mark! (joto-block-next from))]
                       [(if-block? from) (mark! (if-block-true from)) (mark! (if-block-false from))]
                       [(newframe-block? from)
                        (mark! (newframe-block-next from))
                        (for-each mark! (newframe-block-rp* from))
                        (mark! (newframe-block-rp from))]
                       [(tail-block? from) (void)]
                       [else (sorry! who "unrecognized from ~s" from)]))))
               entry-block*)
             ; discard unreachable blocks, some of of which build-graph stupidly produces
             (let ([block* (filter block-seen? block*)])
               (for-each (lambda (block) (block-seen! block #f)) block*)
               (safe-assert (andmap block-label (append entry-block* block*)))
               (safe-assert (lambda (b) (eq? (local-label-block (block-label b)) b)) (append entry-block* block*))
               `(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...)))))]))


    (define-pass np-add-block-source! : L15a (ir) -> L15a ()
      (definitions
        (define block-checksum
          (lambda (block)
            (fxlogor
              (fxsll (fxlogand (length (block-effect* block)) (fxsrl (most-positive-fixnum) 3)) 3)
              (cond
                [(goto-block? block) #x001]
                [(joto-block? block) #x010]
                [(if-block? block) #x011]
                [(newframe-block? block) #x100]
                [(tail-block? block) #x101]
                [else (sorry! who "unrecognized block ~s" block)])))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (for-each
           (lambda (block)
             (include "types.ss")
             (let ([n (fx- ($block-counter) 1)])
               ($block-counter n)
               (block-pseudo-src-set! block
                 (make-source ($sfd) n (block-checksum block)))))
           block*)
         ir]))

    (define-pass np-remove-repeater-blocks! : L15a (ir) -> L15a ()
      (definitions
        (define path-compress!
          (lambda (b)
            (cond
              [(block-repeater? b) (goto-block-next b)]
              [(and (goto-block? b) (null? (block-effect* b)) (null? (block-src* b)))
               (block-repeater! b #t)
               (let ([end (path-compress! (goto-block-next b))])
                 (goto-block-next-set! b end)
                 end)]
              [else b])))
        (define resolve
          (lambda (b)
            (if (block-repeater? b)
                (goto-block-next b)
                b))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (for-each path-compress! block*)
         (for-each
           (lambda (from)
             (define resolve!
               (lambda (get put!)
                 (let ([to (get from)])
                   (when (block-repeater? to)
                     (put! from (goto-block-next to))))))
             (cond
               [(goto-block? from)
                (unless (block-repeater? from)
                  (resolve! goto-block-next goto-block-next-set!))]
               [(joto-block? from)
                (resolve! joto-block-next joto-block-next-set!)]
               [(if-block? from)
                (resolve! if-block-true if-block-true-set!)
                (resolve! if-block-false if-block-false-set!)]
               [(newframe-block? from)
                (resolve! newframe-block-next newframe-block-next-set!)
                (newframe-block-rp*-set! from (map resolve (newframe-block-rp* from)))
                (resolve! newframe-block-rp newframe-block-rp-set!)]
               [(tail-block? from) (void)]
               [else (sorry! who "unrecognized block ~s" from)]))
           block*)
         (for-each (lambda (dcl)
                     (let* ([b0 (local-label-block dcl)] [b (and b0 (resolve b0))])
                       (unless (eq? b b0)
                         (local-label-block-set! dcl b)
                         (block-label-set! b dcl))))
           (info-lambda-dcl* info))
         `(lambda ,info ,max-fv (,local* ...)
            (,(map resolve entry-block*) ...)
            (,(filter (lambda (b) (or (not (block-repeater? b)) (eq? (goto-block-next b) b))) block*) ...))]))

    (define-pass np-propagate-pariahty! : L15a (ir) -> L15a ()
      (definitions
        (define propagate!
          (lambda (b)
            (unless (block-seen? b)
              (block-seen! b #t)
              (block-pariah! b #f)
              (cond
                [(goto-block? b) (propagate! (goto-block-next b))]
                [(joto-block? b) (propagate! (joto-block-next b))]
                [(if-block? b)
                 ; could set likely branch direction before marking targets as pariahs,
                 ; but these are all pariah blocks anyway
                 (propagate! (if-block-true b))
                 (propagate! (if-block-false b))]
                [(newframe-block? b)
                 (propagate! (newframe-block-next b))
                 (for-each propagate! (newframe-block-rp* b))
                 (propagate! (newframe-block-rp b))]
                [(tail-block? b) (void)]
                [else (sorry! who "unrecognized block ~s" b)])))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (safe-assert (not (ormap block-seen? block*)))
         ; optimistically assume all blocks are pariahs, then un-pariah anything reachable from
         ; the entry block without going through a known pariah block
         (for-each (lambda (b) (if (block-pariah? b) (block-seen! b #t) (block-pariah! b #t))) block*)
         (for-each propagate! entry-block*)
         (for-each (lambda (b) (block-seen! b #f)) block*)
         ir]))

    (module (np-insert-profiling)
      (include "types.ss")

      (define-record-type start-block
        (parent block)
        (fields
          (mutable link*))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (pargs->new)
            (lambda ()
              ((pargs->new) '())))))

      (define-record-type link
        (fields
          from
          (mutable to)
          (mutable weight)
          (mutable mst)
          (mutable counter)
          (mutable op))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (new)
            (lambda (from to)
              (new from to 0 #f #f #f)))))

      (define-who add-link-records!
        ; also adds exit-block links
        (lambda (start-block exit-block entry-block* block*)
          (define do-link
            (lambda (from to)
              (let ([link (make-link from to)])
                (block-in-link*-set! to (cons link (block-in-link* to)))
                (unless (block-seen? to)
                  (block-seen! to #t)
                  (cond
                    [(goto-block? to) (goto-block-next-set! to (do-link to (goto-block-next to)))]
                    [(joto-block? to) (joto-block-next-set! to (do-link to (joto-block-next to)))]
                    [(if-block? to)
                     (if-block-true-set! to (do-link to (if-block-true to)))
                     (if-block-false-set! to (do-link to (if-block-false to)))]
                    [(tail-block? to) (tail-block-exit-set! to (do-link to exit-block))]
                    [(newframe-block? to)
                     (newframe-block-next-set! to (do-link to (newframe-block-next to)))
                     ; link start-block to rp blocks since they are, in reality, extra entry points that
                     ; need to be measured separately due to the potential for control operations
                     (let ([rplink* (map (lambda (rp) (do-link start-block rp)) (newframe-block-rp* to))]
                           [rplink (do-link start-block (newframe-block-rp to))])
                       (start-block-link*-set! start-block (append rplink* (cons rplink (start-block-link* start-block))))
                       ; and also record links in newframe-block for remove-link-records!
                       (newframe-block-rp*-set! to rplink*)
                       (newframe-block-rp-set! to rplink))]
                    [else (sorry! who "unrecognized block ~s" to)]))
                link)))
          (let ([all-block* (cons* start-block exit-block block*)])
            (for-each (lambda (block) (block-in-link*-set! block '())) all-block*)
            (block-seen! start-block #t)
            (let ([entry-link* (map (lambda (to) (do-link start-block to)) entry-block*)])
              (start-block-link*-set! start-block (append entry-link* (start-block-link* start-block)))
              (for-each (lambda (block) (block-seen! block #f)) all-block*)
              entry-link*))))

      (define-who remove-link-records!
        (lambda (block*)
          (for-each
            (lambda (block)
              (cond
                [(goto-block? block) (goto-block-next-set! block (link-to (goto-block-next block)))]
                [(joto-block? block) (joto-block-next-set! block (link-to (joto-block-next block)))]
                [(if-block? block)
                 (if-block-true-set! block (link-to (if-block-true block)))
                 (if-block-false-set! block (link-to (if-block-false block)))]
                [(tail-block? block) (tail-block-exit-set! block #f)]
                [(newframe-block? block)
                 (newframe-block-next-set! block (link-to (newframe-block-next block)))
                 (newframe-block-rp*-set! block (map link-to (newframe-block-rp* block)))
                 (newframe-block-rp-set! block (link-to (newframe-block-rp block)))]
                [else (sorry! who "unrecognized block ~s" block)])
              (block-in-link*-set! block '()))
            block*)))

      (define weight-graph!
        (lambda (start-block exit-block block*)
          (define sum-link-weights
            (lambda (links)
              ; using #3$fx+ to ensure that we wrap when we go over the fixnum range
              (fold-left (lambda (n link) (#3%fx+ (link-weight link) n)) 0 links)))
          (define-who process-link
            (lambda (ls link)
              (let ([block (link-to link)])
                (cond
                  [(block-finished? block) ls]
                  [(block-seen? block) ; cycle?
                   (link-weight-set! link 500)
                   ls]
                  [else
                   (block-seen! block #t)
                   (let ([ls (cond
                               [(goto-block? block) (process-link ls (goto-block-next block))]
                               [(joto-block? block) (process-link ls (joto-block-next block))]
                               [(if-block? block) (process-link (process-link ls (if-block-false block)) (if-block-true block))]
                               [(tail-block? block) ls]
                               [(newframe-block? block) (process-link ls (newframe-block-next block))]
                               [else (sorry! who "unrecognized block ~s" block)])])
                     (block-finished! block #t)
                     (cons block ls))]))))
          (define-who propagate-flow
            (lambda (block)
              (let ([sum (sum-link-weights (block-in-link* block))]
                    [links (cond
                             [(goto-block? block) (list (goto-block-next block))]
                             [(joto-block? block) (list (joto-block-next block))]
                             [(if-block? block) (list (if-block-true block) (if-block-false block))]
                             [(tail-block? block) (list (tail-block-exit block))]
                             [(newframe-block? block) (list (newframe-block-next block))]
                             [else (sorry! who "unrecognized block ~s" block)])])
                (safe-assert (not (null? links)))
                ; AWK: we are missing the notion of those instructions that usually
                ; succeed (dooverflow, dooverflood, call-error, fx+? and fx-? in
                ; the original blocks.ss code)
                (let-values ([(pariah* non-pariah*)
                              (partition (lambda (link) (block-pariah? (link-to link))) links)])
                  (if (null? non-pariah*)
                      (divide-flow sum (length pariah*) pariah*)
                      (divide-flow sum (length non-pariah*) non-pariah*))))))
          (define divide-flow
            (lambda (flow n ls)
              (safe-assert (fx> n 0))
              (if (fx= n 1)
                  (link-weight-set! (car ls) flow)
                  (let ([x (fxquotient flow n)])
                    (link-weight-set! (car ls) x)
                    (divide-flow (fx- flow x) (fx- n 1) (cdr ls))))))
          (let ([exit->start (goto-block-next exit-block)])
            (block-finished! start-block #t)
            (block-finished! exit-block #t)
            ; DFS to find cycles & determine order to propagate flow
            (link-weight-set! exit->start 1000)
            (for-each propagate-flow (fold-left process-link '() (start-block-link* start-block)))
            (for-each (lambda (block) (block-seen! block #f)) (cons* start-block exit-block block*)))))

      (module (mst-top)
        (define-who mst-top
          (lambda (start-block exit-block block*)
            (block-seen! start-block #t)
            (block-seen! exit-block #t)
            (let ([pq (pqinitialize (length block*))])
              (define (mst-in-link link) (pqupdate link (link-from link) pq))
              (define (mst-out-link link) (pqupdate link (link-to link) pq))
              ; add the exit->start link to the mst
              (link-mst-set! (goto-block-next exit-block) exit-block)
              (for-each mst-out-link (start-block-link* start-block))
              (let mst ()
                (unless (pqempty? pq)
                  (let ([r (pqremove pq)])
                    (let ([block (cdr r)] [link (car r)])
                      (link-mst-set! link block)
                      (for-each mst-in-link (block-in-link* block))
                      (cond
                        [(goto-block? block) (mst-out-link (goto-block-next block))]
                        [(joto-block? block) (mst-out-link (joto-block-next block))]
                        [(if-block? block) (mst-out-link (if-block-true block)) (mst-out-link (if-block-false block))]
                        [(tail-block? block) (mst-out-link (tail-block-exit block))]
                        [(newframe-block? block) (mst-out-link (newframe-block-next block))]
                        [else (sorry! who "unrecognized block ~s" block)])
                      (mst))))))))

        (define pqinitialize
          (let ([b (make-block)]) ;; add dummy first block in the priority-queue
            (let ([l (make-link #f b)])
              (link-weight-set! l (most-positive-fixnum))
              (let ([pqfirst (cons l b)])
                (lambda (size)
                  (cons 0 (make-vector (fx+ size 1) pqfirst)))))))

        (define pqupheap
          (lambda (heap k w)
            (let ([y (vector-ref heap (fx/ k 2))])
              (if (fx> w (link-weight (car y)))
                  (begin
                    (vector-set! heap k y)
                    (block-seen! (cdr y) k)
                    (pqupheap heap (fx/ k 2) w))
                  k))))

        (define pqdownheap
          (lambda (heap n k w)
            (if (fx< (fx/ n 2) k)
                k
                (let ([j (fx* k 2)])
                  (let ([y1 (vector-ref heap j)]
                        [y2 (and (fx< j n) (vector-ref heap (fx+ j 1)))])
                    (let ([w1 (link-weight (car y1))]
                          [w2 (if y2 (link-weight (car y2)) (most-negative-fixnum))])
                      (if (fx>= w1 w2)
                          (if (fx>= w w1)
                              k
                              (begin
                                (vector-set! heap k y1)
                                (block-seen! (cdr y1) k)
                                (pqdownheap heap n j w)))
                          (if (fx>= w w2)
                              k
                              (begin
                                (vector-set! heap k y2)
                                (block-seen! (cdr y2) k)
                                (pqdownheap heap n (fx+ j 1) w))))))))))

        (define pqempty?
          (lambda (pq)
            (fx= (car pq) 0)))

        (define pqremove
          (lambda (pq)
            (let ([n (fx- (car pq) 1)]
                  [heap (cdr pq)])
              (set-car! pq n)
              (let ([r (vector-ref heap 1)]
                    [x (vector-ref heap (fx+ n 1))])
                (let ([k (pqdownheap heap n 1 (link-weight (car x)))])
                  (vector-set! heap k x)
                  (block-seen! (cdr x) k))
                (block-seen! (cdr r) #t)
                r))))

        (define pqupdate
          (lambda (link block pq)
            (let ([k (block-seen? block)])
              (cond
                [(eq? k #t) (void)]
                [(eq? k #f)
                 (let ([n (fx+ (car pq) 1)] [heap (cdr pq)])
                   (set-car! pq n)
                   (let ([k (pqupheap heap n (link-weight link))])
                     (vector-set! heap k (cons link block))
                     (block-seen! block k)))]
                [else
                 (let ([heap (cdr pq)])
                   (let ([x (vector-ref heap k)]
                         [w (link-weight link)])
                     (when (fx> w (link-weight (car x)))
                       (let ([k (pqupheap heap k w)])
                         (vector-set! heap k (cons link block))
                         (block-seen! block k)))))])))))

      (define-who instrument
        (lambda (start-block exit-block block*)
          (define checks-cc?
            (lambda (block)
              (and (if-block? block)
                   (null? (block-effect* block))
                   (nanopass-case (L15a Pred) (if-block-pred block)
                     [(inline ,live-info ,info ,pred-prim ,t* ...) (eq? pred-prim %condition-code)]
                     [else #f]))))
          (define add-counter!
            (lambda (block counter)
              (define add-instr!
                (lambda (block ir)
                  (let ([effect* (block-effect* block)])
                    (block-effect*-set! block
                      (if (block-return-point? block)
                          ; rp-header / return-point form must be first
                          (cons* (car effect*) ir (cdr effect*))
                          (cons ir effect*))))))
              (with-output-language (L15a Effect)
                (add-instr! block
                  `(inline ,(make-live-info) ,null-info ,%inc-profile-counter
                     (literal ,(make-info-literal #t 'object counter (constant record-data-disp)))
                     (immediate 1))))))
          (define maybe-add-counter
            (lambda (new* link)
              (cond
                [(link-counter link) =>
                 (lambda (counter)
                   (let ([from (link-from link)] [to (link-to link)])
                     (cond
                       [(and (fx= (length (block-in-link* to)) 1) (not (eq? to exit-block)))
                        (assert (not (checks-cc? to)))
                        (add-counter! to counter)
                        new*]
                       [(or (goto-block? from) (tail-block? from))
                        (assert (not (checks-cc? from)))
                        (add-counter! from counter)
                        new*]
                       [else
                        (safe-assert (not (eq? to exit-block)))
                        (assert (not (checks-cc? to)))
                        (let* ([block (make-goto-block)] [l (make-link block to)])
                          (let ([label (block-label to)])
                            (if (and (eq? from start-block) (and (direct-call-label? label) (direct-call-label-referenced label)))
                                (begin
                                  ; we're adding the new block between the (virtual) start block and one
                                  ; of our (referenced) dcls.  we need to move the dcl label to the new
                                  ; block so the counter is incremented when we come in from the outside
                                  (block-label-set! block label)
                                  (local-label-block-set! label block)
                                  (let ([label (make-local-label 'exdcl)])
                                    (block-label-set! to label)
                                    (local-label-block-set! label to)))
                                (let ([label (make-local-label 'profile)])
                                  (block-label-set! block label)
                                  (local-label-block-set! label block))))
                          (link-to-set! link block)
                          ; set link mst for p-dot-graph/profiling's benefit
                          (link-mst-set! l block)
                          (block-in-link*-set! block (list link))
                          (goto-block-next-set! block l)
                          (block-in-link*-set! to (cons l (remq link (block-in-link* to))))
                          (add-counter! block counter)
                          (cons block new*))])))]
                [else new*])))
          (fold-left
            (lambda (new* block)
              (fold-left maybe-add-counter
                new* (block-in-link* block)))
            block*
            (cons exit-block block*))))

      (define build-pinfo
        (lambda (exit-block block*)
          ; op -> counter | (plus-counter* . minus-counter*)
          ; plus-counter* -> (op ...)
          ; minus-counter* -> (op ...)
          (define make-op
            (lambda (plus minus)
              ; optimize ((op) . ()) => op
              (if (and (null? minus) (fx= (length plus) 1))
                  (car plus)
                  (cons plus minus))))
          (define-who exit-ops
            (lambda (block l)
              (define maybe-build-op
                (lambda (link ls)
                  (if (eq? link l)
                      ls
                      (cons (build-op link) ls))))
              (cond
                [(goto-block? block) (maybe-build-op (goto-block-next block) '())]
                [(joto-block? block) (maybe-build-op (joto-block-next block) '())]
                [(if-block? block) (maybe-build-op (if-block-true block) (maybe-build-op (if-block-false block) '()))]
                [(tail-block? block) (maybe-build-op (tail-block-exit block) '())]
                [(newframe-block? block) (maybe-build-op (newframe-block-next block) '())]
                [else (sorry! who "unrecognized block ~s" block)])))
          (define enter-ops
            (lambda (n l)
              (let ([ls (block-in-link* n)])
                (map build-op (if (not l) ls (remq l ls))))))
          (define build-op
            (lambda (l)
              (cond
                [(link-mst l) =>
                 (lambda (n)
                   (let ([op (if (eq? (link-to l) n)
                                 (make-op (exit-ops n #f) (enter-ops n l))
                                 (make-op (enter-ops n #f) (exit-ops n l)))])
                     (link-op-set! l op)
                     op))]
                [else
                 (or (link-counter l)
                     (let ([counter (make-profile-counter 0)])
                       (link-counter-set! l counter)
                       (link-op-set! l counter)
                       counter))])))
          (define (filter-src* block)
            (cond
              [(eq? ($compile-profile) 'source) (block-src* block)]
              [(block-pseudo-src block) => list]
              [else '()]))
          (fold-left
            (lambda (ls block)
              (let ([src* (filter-src* block)])
                (if (null? src*)
                    ls
                    (cons (make-rblock src* (make-op (map build-op (block-in-link* block)) '())) ls))))
            '() block*)))

      (module (p-graph/profiling p-dot-graph/profiling)
        (define-who block-link*
          (lambda (block)
            (cond
              [(goto-block? block) `(,(goto-block-next block))]
              [(joto-block? block) `(,(joto-block-next block))]
              [(if-block? block) `(,(if-block-true block) ,(if-block-false block))]
              ; leave out newframe-block => rp links, since we profiler uses its own start-block => rp links
              [(newframe-block? block) `(,(newframe-block-next block))]
              [(tail-block? block) `(,(tail-block-exit block))]
              [(start-block? block) (start-block-link* block)]
              [else (sorry! who "unrecognized block ~s" block)])))
        (define block->pretty-name
          (lambda (block)
            (define block->label
              (lambda (block)
                (let ([label (block-label block)])
                  (or label
                      (let ([label (make-local-label 'unknown)])
                        (block-label-set! block label)
                        label)))))
            (parameterize ([print-gensym 'pretty/suffix]) (format "~s" (block->label block)))))
        (define p-dot-graph/profiling
          (lambda (block* exit-block p)
            (define print-link
              (lambda (reversed?)
                (lambda (link)
                  (let-values ([(from to) (if reversed?
                                              (values (link-to link) (link-from link))
                                              (values (link-from link) (link-to link)))])
                    (display "   " p)
                    (display (block->pretty-string from) p)
                    (display " -> " p)
                    (display (block->pretty-string to) p)
                    #;(when (and (block-non-tail-call? (link-from link)) (eq? (link-to link) exit-block))
                      (display " [color=grey]" p))
                    (if (link-mst link)
                        (if reversed?
                            (display " [color=blue]" p)
                            (display " [color=black]" p))
                        (if reversed?
                            (display " [color=pink]" p)
                            (display " [color=red]" p)))
                    (write-char #\; p)
                    (newline p))
                  ; print the tree in green
                  #;(when (link-mst link)
                    (let-values ([(from to) (if (eq? (link-mst link) (link-to link))
                                                (values (link-from link) (link-to link))
                                                (values (link-to link) (link-from link)))])
                      (display "   " p)
                      (display (block->pretty-string from) p)
                      (display " -> " p)
                      (display (block->pretty-string to) p)
                      (display "  [color=green];\n" p))))))
            (define block->pretty-string
              (lambda (block)
                (list->string (subst #\_ #\. (subst #\_ #\- (string->list (block->pretty-name block)))))))
            (newline p)
            (display "digraph PROFILE {\n" p)
            (display "   node [shape = box];" p)
            (let f ([block* block*] [link* '()] [in-link* '()])
              (if (null? block*)
                  (begin
                    (newline p)
                    (newline p)
                    (for-each (print-link #f) link*)
                    (when #f (for-each (print-link #t) in-link*))
                    (display "}\n" p))
                  (let ([block (car block*)])
                    (display " " p)
                    (display (block->pretty-string block) p)
                    (f (cdr block*)
                       (append (block-link* block) link*)
                       (append (block-in-link* block) in-link*)))))))
      (define-who p-graph/profiling
        (lambda (block* name p)
          (newline p)
          (when name (fprintf p "~a:\n" name))
          (parameterize ([print-graph #t] [print-length 6] [print-level 3] [print-gensym 'pretty/suffix])
            (for-each
              (lambda (block)
                (fprintf p "~a: " (block->pretty-name block))
                (let loop ([links (block-link* block)])
                  (unless (null? links)
                    (let ([link (car links)])
                      (fprintf p "~a(~d)~a"
                        (block->pretty-name (link-to link))
                        (link-weight link)
                        (if (link-mst link)
                            ""
                            "*"))
                      (unless (null? (cdr links)) (display ", " p))
                      (loop (cdr links)))))
                (fprintf p " in=~d:" (length (block-in-link* block)))
                (begin
                  (newline p)
                  (for-each
                    (lambda (link)
                      (cond
                        [(link-counter link) (fprintf p "   Bump count to ~a\n" (block->pretty-name (link-to link)))]
                        [(link-op link) (fprintf p "   Link count to ~a computed from other counts\n" (block->pretty-name (link-to link)))])
                      (fprintf p "   ~a -> ~a -- ~s\n" (block->pretty-name (link-from link))
                        (block->pretty-name (link-to link)) (link-op link)))
                    (block-link* block))
                  ; We no longer have the code to report here, so we're reporting from source
                  (fprintf p "~{   ~s~%~}" (map unparse-L15a (block-effect* block)))
                  (cond
                    [(or (goto-block? block) (joto-block? block) (newframe-block? block) (start-block? block)) (void)]
                    [(if-block? block) (fprintf p "   ~s~%" (unparse-L15a (if-block-pred block)))]
                    [(tail-block? block) (fprintf p "   ~s~%" (unparse-L15a (tail-block-tail block)))]
                    [else (sorry! who "unrecognized block ~s" block)])))
              block*)))))

      (define-pass np-insert-profiling : L15a (ir) -> L15a ()
        (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
          [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
           (let* ([start-block (make-start-block)]
                  [exit-block (make-goto-block start-block)])
             (block-label-set! start-block 'start)
             (block-label-set! exit-block 'exit)
             (let ([entry-link* (add-link-records! start-block exit-block entry-block* block*)])
               (weight-graph! start-block exit-block block*)
               (mst-top start-block exit-block block*)
               (info-lambda-pinfo*-set! info (append (build-pinfo exit-block block*) (info-lambda-pinfo* info)))
               ; now insert increments for counters added by build-pinfo
               (let* ([block* (instrument start-block exit-block block*)]
                      [entry-block* (map link-to entry-link*)])
                 (safe-assert (andmap (lambda (block) (not (null? (block-in-link* block)))) block*))
                 (when ($assembly-output)
                   (let ([block* (cons start-block (append block* (list exit-block)))])
                     (p-graph/profiling block* (info-lambda-name info) ($assembly-output))
                     (p-dot-graph/profiling block* exit-block ($assembly-output))))
                 (remove-link-records! block*)
                 (for-each (lambda (block) (block-seen! block #f) (block-finished! block #f)) block*)
                 (safe-assert (andmap block-label (append entry-block* block*)))
                 (safe-assert (lambda (b) (eq? (local-label-block (block-label b)) b)) (append entry-block* block*))
                 `(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...)))))])))

    (module (p-graph p-dot-graph)
      (define block->pretty-name
        (lambda (block)
          (define block->label
            (lambda (block)
              (let ([label (block-label block)])
                (or label
                    (let ([label (make-local-label 'unknown)])
                      (block-label-set! block label)
                      label)))))
          (parameterize ([print-gensym 'pretty/suffix]) (format "~s" (block->label block)))))
      (define p-dot-graph
        (lambda (block* p)
          (define print-link
            (lambda (link)
              (display "   " p)
              (display (car link) p)
              (display " -> " p)
              (display (cdr link) p)
              (write-char #\; p)
              (newline p)))
          (define block->pretty-string
            (lambda (block)
              (list->string (subst #\_ #\. (subst #\_ #\- (string->list (block->pretty-name block)))))))
          (define-who block-link*
            (lambda (block)
              (let ([block-name (block->pretty-string block)])
                (map (lambda (x) (cons block-name (block->pretty-string x)))
                  (cond
                    [(goto-block? block) `(,(goto-block-next block))]
                    [(joto-block? block) `(,(joto-block-next block))]
                    [(if-block? block) `(,(if-block-true block) ,(if-block-false block))]
                    [(newframe-block? block) `(,(newframe-block-next block) ,@(newframe-block-rp* block) ,(newframe-block-rp block))]
                    [(tail-block? block) '()]
                    [else (sorry! who "unrecognized block ~s" block)])))))
          (display "digraph BLOCKS {\n" p)
          (display "   node [shape = box];" p)
          (let f ([block* block*] [link* '()])
            (if (null? block*)
                (begin
                  (newline p)
                  (newline p)
                  (for-each print-link link*)
                  (display "}\n" p))
                (let ([block (car block*)])
                  (display " " p)
                  (display (block->pretty-string block) p)
                  (when (block-pariah? block) (display " [color=red]" p))
                  (f (cdr block*) (append (block-link* block) link*)))))))
      (define-who p-graph
        (lambda (block* name p unparser)
          (when name (fprintf p "\n~a:" name))
          (parameterize ([print-graph #t] [print-length 6] [print-level 3] [print-gensym 'pretty/suffix])
            (for-each
              (lambda (block)
                (fprintf p "~a (depth = ~s~@[, pariah~]):\n" (block->pretty-name block) (block-depth block) (block-pariah? block))
                (fprintf p "~{   ~s~%~}" (map unparser (block-effect* block)))
                (cond
                  [(goto-block? block) (fprintf p "   ~s\n" `(goto ,(block->pretty-name (goto-block-next block))))]
                  [(joto-block? block) (fprintf p "   ~s\n" `(joto ,(block->pretty-name (joto-block-next block))))]
                  [(if-block? block) (fprintf p "   ~s\n" `(if ,(unparser (if-block-pred block))
                                                             (goto ,(block->pretty-name (if-block-true block)))
                                                             (goto ,(block->pretty-name (if-block-false block)))))]
                  [(tail-block? block) (fprintf p "   ~s\n" (unparser (tail-block-tail block)))]
                  [(newframe-block? block) (fprintf p "   ~s\n" `(goto ,(block->pretty-name (newframe-block-next block))))]
                  [else (sorry! who "unrecognized block ~s" block)]))
              block*)))))

    (define-pass np-add-in-links! : L15a (ir) -> L15a ()
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (safe-assert (andmap (lambda (block) (eq? (block-in-link* block) '())) block*))
         (for-each
           (lambda (from)
             (define add-in-link!
               (lambda (to)
                 (block-in-link*-set! to (cons from (block-in-link* to)))))
             (cond
               [(goto-block? from) (add-in-link! (goto-block-next from))]
               [(if-block? from) (add-in-link! (if-block-true from)) (add-in-link! (if-block-false from))]
               [(newframe-block? from)
                (add-in-link! (newframe-block-next from))
                (for-each add-in-link! (newframe-block-rp* from))
                (add-in-link! (newframe-block-rp from))]
               [(joto-block? from) (add-in-link! (joto-block-next from))]
               [(tail-block? from) (void)]
               [else (sorry! who "unrecognized block ~s" from)]))
           block*)
         ir]))

    (define-pass np-compute-loop-depth! : L15a (ir) -> L15a ()
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (safe-assert (not (ormap block-seen? block*)) (not (ormap block-finished? block*)))
         (let ([lh* '()])
           (for-each
             (rec f
               (lambda (b)
                 (unless (block-finished? b)
                   (if (block-seen? b)
                       (begin
                         (block-loop-header! b #t)
                         (set! lh* (cons b lh*)))
                       (begin
                         (block-seen! b #t)
                         (cond
                           [(goto-block? b) (f (goto-block-next b))]
                           [(joto-block? b) (f (joto-block-next b))]
                           [(if-block? b) (f (if-block-true b)) (f (if-block-false b))]
                           [(tail-block? b) (void)]
                           [(newframe-block? b)
                            (f (newframe-block-next b))
                            (for-each f (newframe-block-rp* b))
                            (f (newframe-block-rp b))]
                           [else (sorry! who "unrecognized block ~s" b)])
                         (block-seen! b #f)
                         (block-finished! b #t))))))
             entry-block*)
           (unless (null? lh*)
             (fold-left (lambda (i b) (block-index-set! b i) (fx+ i 1)) 0 lh*)
             (let ([tree-size (length lh*)] [blockvec (list->vector lh*)] [lb* lh*])
               (define remove-block
                 (lambda (b tree)
                   (let ([index (block-index b)])
                     (if index (tree-bit-unset tree tree-size index) tree))))
               ; invert sense of block-finished so we don't have to reset
               (let ([block-finished? (lambda (b) (not (block-finished? b)))]
                     [block-finished! (lambda (b bool) (block-finished! b (not bool)))])
                 (for-each
                   (rec f
                     (lambda (b)
                       (cond
                         [(block-finished? b)
                          (tree-fold-left (lambda (lhs index)
                                            (let ([b (vector-ref blockvec index)])
                                              (if (block-finished? b)
                                                  lhs
                                                  (tree-bit-set lhs tree-size index))))
                            tree-size empty-tree (block-loop-headers b))]
                         [(block-seen? b)
                          (safe-assert (block-index b))
                          (tree-bit-set empty-tree tree-size (block-index b))]
                         [(tail-block? b) empty-tree]
                         [else
                          (block-seen! b #t)
                          (let ([lhs (remove-block b
                                       (cond
                                         [(goto-block? b) (f (goto-block-next b))]
                                         [(joto-block? b) (f (joto-block-next b))]
                                         [(if-block? b)
                                          ; must follow same order as loop above so we find the same loop headers
                                          (let ([lhs (f (if-block-true b))])
                                            (tree-merge lhs (f (if-block-false b)) tree-size))]
                                         [(newframe-block? b)
                                          ; must follow same order as loop above so we find the same loop headers
                                          (fold-left (lambda (lhs b) (tree-merge lhs (f b) tree-size))
                                            (let ([lhs (f (newframe-block-next b))]) (tree-merge lhs (f (newframe-block-rp b)) tree-size))
                                            (newframe-block-rp* b))]
                                         [else (sorry! who "unrecognized block ~s" b)]))])
                            (unless (or (block-loop-header? b) (eqv? (block-loop-headers b) empty-tree))
                              (set! lb* (cons b lb*)))
                            (block-seen! b #f)
                            (block-finished! b #t)
                            (block-loop-headers-set! b lhs)
                            lhs)])))
                   ; seems like we should be able to use (reverse lh*) rather than entry-block* here
                   ; but we end up finding different loop headers in some cases
                   entry-block*))
               (for-each
                 (rec g
                   (lambda (b)
                     (if (block-seen? b)
                         (block-depth b)
                         (begin
                           (block-seen! b #t)
                           (let ([depth (tree-fold-left (lambda (depth index) (fxmax (g (vector-ref blockvec index)) depth))
                                          tree-size 0 (block-loop-headers b))])
                             (let ([depth (if (block-loop-header? b) (fx+ depth 1) depth)])
                               (block-depth-set! b depth)
                               depth))))))
                   lb*))
             (for-each (lambda (b) (block-seen! b #f)) block*)
             #;(p-dot-graph block* (current-output-port))
             #;(p-graph block* (info-lambda-name info) (current-output-port) unparse-L15a)))
         (for-each (lambda (b) (block-finished! b #f)) block*)
         ir]))

    (define-pass np-weight-references! : L15a (ir) -> L15a ()
      (definitions
        (define weight-block!
          (lambda (max-weight)
            (lambda (block weight)
              (let ([weight (if (and weight (not (fl= max-weight 0.0)))
                                (flonum->fixnum (fl/ weight (fl/ max-weight 1024.0)))
                                (if (block-pariah? block)
                                    0
                                    (expt 4 (fxmin (block-depth block) 5))))])
                (block-weight-set! block weight)
                (unless (fx= weight 0)
                  (let ()
                    (define fixnum (lambda (x) (if (fixnum? x) x (most-positive-fixnum))))
                    ; refs and sets are weighted equally
                    (define process-var
                      (lambda (x)
                        (when (uvar? x)
                          (uvar-ref-weight-set! x (fixnum (+ (uvar-ref-weight x) weight))))))
                    (define Lvalue
                      (lambda (lvalue)
                        (nanopass-case (L15a Lvalue) lvalue
                          [,x (process-var x)]
                          [(mref ,x1 ,x2 ,imm) (process-var x1) (process-var x2)])))
                    (define Triv
                      (lambda (t)
                        (nanopass-case (L15a Triv) t
                          [,lvalue (Lvalue lvalue)]
                          [else (void)])))
                    (define Rhs
                      (lambda (rhs)
                        (nanopass-case (L15a Rhs) rhs
                          [,lvalue (Lvalue lvalue)]
                          [(inline ,info ,value-prim ,t* ...)
                           (for-each Triv t*)]
                          [else (void)])))
                    (define Pred
                      (lambda (p)
                        (nanopass-case (L15a Pred) p
                          [(inline ,live-info ,info ,pred-prim ,t* ...)
                           (for-each Triv t*)]
                          [else (sorry! who "unexpected pred ~s" p)])))
                    (define Tail
                      (lambda (tl)
                        (nanopass-case (L15a Tail) tl
                          [(jump ,live-info ,t (,var* ...)) (Triv t)]
                          [else (void)])))
                    (for-each
                      (lambda (instr)
                        (nanopass-case (L15a Effect) instr
                          [(set! ,live-info ,lvalue ,rhs) (Lvalue lvalue) (Rhs rhs)]
                          [(inline ,live-info ,info ,effect-prim ,t* ...)
                           (for-each Triv t*)]
                          [else (void)]))
                      (block-effect* block))
                    (cond
                      [(or (goto-block? block) (joto-block? block)) (void)]
                      [(if-block? block) (Pred (if-block-pred block))]
                      [(newframe-block? block)
                       (let ([newframe-info (newframe-block-info block)])
                         (info-newframe-weight-set! newframe-info
                           (fixnum (+ (info-newframe-weight newframe-info) weight))))]
                      [(tail-block? block) (Tail (tail-block-tail block))]
                      [else (sorry! who "unrecognized block ~s" block)]))))))))
      ; now know for each block its loop nesting depth and pariahty
      ; now weight calls and refs accordingly
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv (,local* ...) (,entry-block* ...) (,block* ...))
         (if ($profile-block-data?)
             (let* ([weight* (map (lambda (block)
                                    (let ([psrc (block-pseudo-src block)])
                                      (and psrc (profile-query-weight psrc))))
                               block*)]
                    [max-weight (fold-left (lambda (m block weight)
                                             (if weight (flmax m weight) m))
                                  0.0 block* weight*)])
               (for-each (weight-block! max-weight) block* weight*))
             (let ([wb (weight-block! #f)])
               (for-each (lambda (block) (wb block #f)) block*)))
         ir]))

    ; this must come before np-allocate-registers since asm-module is imported
    ; by the included file <architecture>-instructions.ss
    (module (np-generate-code asm-module)
      (define-threaded aop)
      (define-threaded funcrel*)
      (define-threaded current-func)
      (define make-funcrel
        (lambda (reloc l offset)
          (let ([stuff (list offset l)])
            (set! funcrel* (cons stuff funcrel*))
            (cons reloc stuff))))
     ; TODO: generate code forward => backward and thread through a machine-state
     ; record that says what each register contains, including the condition-code
     ; register, so that we can avoid redundant loads and tests.  For example,
     ; second set! of td in (seq (set! td ,(%mref tc 20)) ... (set! td ,(%mref tc 20)))
     ; should go away with no intervening assignment of td or tc[20].  Similarly,
     ; in (seq (mset! tc 36 (incr ,(%mref tc 36))) (if (eq? ,(%mref tc 36) 0) L1 L2),
     ; the test should reduce to a check of the 'z' flag.
     ; plain chunks arise only as the destination for a rachunk
      (define-record-type chunk
        (nongenerative)
        (fields size code*)
        (protocol
          (lambda (new)
            (lambda (code*) (new (asm-size* code*) code*)))))
      (define-record-type lchunk
        (parent chunk)
        (nongenerative)
        (sealed #t)
        (fields l)
        (protocol
          (lambda (pargs->new)
            (lambda (l code*)
              ((pargs->new code*) l)))))
      (define-record-type gchunk
        (parent chunk)
        (nongenerative)
        (sealed #t)
        (fields l laddr next-offset)
        (protocol
          (lambda (pargs->new)
            (lambda (l next-offset code*)
              ((pargs->new code*) l (local-label-offset l) next-offset)))))
      (define-record-type cgchunk
        (parent chunk)
        (nongenerative)
        (sealed #t)
        (fields info l1 l2 laddr1 laddr2 next-offset)
        (protocol
          (lambda (pargs->new)
            (lambda (info l1 l2 next-offset code*)
              (define label-offset
                (lambda (l)
                  (and (local-label? l) (local-label-offset l))))
              ((pargs->new code*) info l1 l2 (label-offset l1) (label-offset l2) next-offset)))))
      ; rachunks arise only during code generation to support machines like the ARM that determine
      ; return addresses for Scheme calls using pc-relative add or lea instructions
      (define-record-type rachunk
        (parent chunk)
        (nongenerative)
        (sealed #t)
        (fields dest l incr-offset laddr next-offset)
        (protocol
          (lambda (pargs->new)
            (lambda (dest l incr-offset next-offset code*)
              ((pargs->new code*) dest l incr-offset (local-label-offset l) next-offset)))))

      (define-pass np-generate-code : L16 (ir) -> * (code)
        (definitions
          (define munge-recur?)
          (define c-trace
            ; copied from compile.ss
            (lambda (name size trace-list p)
              (when p
                (newline p)
                (when name (fprintf p "~a: ~%" name))
                (parameterize ([print-length 5] [print-level 3] [print-gensym 'pretty/suffix])
                  (let dump ([trace-list trace-list] [last-addr size])
                    (when (pair? trace-list)
                      (apply (lambda (addr op . args)
                               (if (eq? op 'label)
                                   (begin
                                     (fprintf p "~{~s~^, ~}:\n" addr)
                                     (dump (cdr trace-list) last-addr))
                                   (begin
                                     (fprintf p "~d:~9t~a~24t" (- size last-addr) op)
                                     (do ((args args (cdr args)))
                                       ((null? args))
                                       (let ([arg (car args)])
                                         (if (string? arg) (display arg p) (write arg p)))
                                       (unless (null? (cdr args)) (display ", " p)))
                                     (newline p)
                                     (dump (cdr trace-list) addr))))
                        (car trace-list)))))
                (fprintf p "~d:~9t<end~@[ ~a~]>\n" size name))))
          ; munge gets the code in forward order, but really wants to process it
          ; backwards to find the label offsets.  Maybe the size would be better
          ; tracked by doing it more like cp2 does right now and then patching in
          ; the forward jumps and tightening up the code.
          (define-who munge
            (lambda (c* size)
              (define (munge-pass c* iteration)
                (define get-local-label-offset
                  (lambda (l)
                    (local-label-iteration-set! l iteration)
                    (local-label-offset l)))
                (let f ([rc* (reverse c*)] [c* '()] [offset 0])
                  (if (null? rc*)
                      (values c* offset)
                      (let ([c (car rc*)] [rc* (cdr rc*)])
                        (cond
                          [(lchunk? c)
                           (let ([l (lchunk-l c)] [offset (fx+ offset (chunk-size c))])
                             (when l
                               (unless (eq? (get-local-label-offset l) offset)
                                 (local-label-offset-set! l offset)
                                 (when (fx= (local-label-iteration l) iteration)
                                   (set! munge-recur? #t))))
                             (f rc* (cons c c*) offset))]
                          [(gchunk? c)
                           (let ([l (gchunk-l c)])
                             (if (and (eq? (get-local-label-offset l) (gchunk-laddr c))
                                      (eq? (gchunk-next-offset c) offset))
                                 (f rc* (cons c c*) (fx+ offset (chunk-size c)))
                                 (let ([c (asm-jump l offset)])
                                   (f rc* (cons c c*) (fx+ offset (chunk-size c))))))]
                          [(cgchunk? c)
                           (let ([l1 (cgchunk-l1 c)] [l2 (cgchunk-l2 c)])
                             (if (and (or (libspec-label? l1) (eq? (get-local-label-offset l1) (cgchunk-laddr1 c)))
                                      (or (libspec-label? l2) (eq? (get-local-label-offset l2) (cgchunk-laddr2 c)))
                                      (eq? (cgchunk-next-offset c) offset))
                                 (f rc* (cons c c*) (fx+ offset (chunk-size c)))
                                 (let ([c (asm-conditional-jump (cgchunk-info c) l1 l2 offset)])
                                   (f rc* (cons c c*) (fx+ offset (chunk-size c))))))]
                          [(rachunk? c)
                           (let ([c (let ([l (rachunk-l c)])
                                      (if (and (eq? (get-local-label-offset l) (rachunk-laddr c))
                                               (eq? (rachunk-next-offset c) offset))
                                          c
                                          (asm-return-address (rachunk-dest c) l (rachunk-incr-offset c) offset)))])
                             (f rc* (cons c c*) (fx+ offset (chunk-size c))))]
                          ; NB: generic test, so must be last!
                          [(chunk? c) (f rc* (cons c c*) (fx+ offset (chunk-size c)))]
                          [else (sorry! who "unexpected chunk ~s" c)])))))
              (define (asm-fixup-opnd x)
                (define-syntax tc-offset-map
                  (let ([q (datum->syntax #'*
                             (map (lambda (x) (cons (caddr x) (string->symbol (format "$~s" (car x)))))
                                  (getprop 'tc '*fields*)))])
                    (lambda (x) #`'#,q)))
                (if (pair? x)
                    (record-case x
                      [(library) (x) `(library ,(libspec-name x))]
                      [(library-code) (x) `(library-code ,(libspec-name x))]
                      [(entry) (i) `(entry ,(vector-ref (constant c-entry-name-vector) i))]
                      [(disp) (offset reg)
                       (cond
                         [(and (eq? reg %tc) (assv offset tc-offset-map)) => cdr]
                         [else `(disp ,offset ,(reg-name reg))])]
                      [(index) (offset reg1 reg2)
                       `(index ,offset ,(reg-name reg1) ,(reg-name reg2))]
                      [(reg) r (reg-name r)]
                      [(label) (offset l)
                       (if (local-label? l)
                           (parameterize ([print-gensym 'pretty/suffix])
                             (format "~s(~d)" l offset))
                           (format "~s" l))]
                      [else x])
                    x))
              (define (extract-trace-code code*)
                (let-values ([(trace* size)
                              (let f ([code* code*])
                                (if (null? code*)
                                    (values '() 0)
                                    (let ([code (car code*)])
                                      (let-values ([(trace* offset) (f (cdr code*))])
                                        (record-case code
                                          [(asm) (op . opnd*)
                                           (values
                                             `((,offset ,op ,@(map asm-fixup-opnd opnd*)) ,@trace*)
                                             offset)]
                                          [(label) l*
                                           (values
                                             (if (null? l*)
                                                 trace*
                                                 `((,l* label) ,@trace*))
                                             offset)]
                                          [else (values trace* (fx+ (asm-size code) offset))])))))])
                  trace*))
              (define (extract-code c*)
                (let f ([c* c*])
                  (if (null? c*)
                      '()
                      (let ([c (car c*)])
                        (let ([code (append (chunk-code* (car c*)) (f (cdr c*)))])
                          (if (and aop (lchunk? c))
                              (let ([l (lchunk-l c)])
                                (if l (cons `(label ,l) code) code))
                              code))))))
              (let f ([c* c*] [size size] [iteration 2])
                (if munge-recur?
                    (begin
                      (set! munge-recur? #f)
                      (let-values ([(c* new-size) (munge-pass c* iteration)])
                        (f c* new-size (fx+ iteration 1))))
                    (let ([code* (extract-code c*)])
                      (if aop
                          (values
                            (remp (lambda (code) (record-case code [(asm label) stuff #t] [else #f])) code*)
                            (extract-trace-code code*)
                            size)
                          (values code* '() size)))))))
          ; TODO: teach c-mkcode & c-faslcode how to indirect labels
          (define-who resolve-funcrel!
            (lambda (funcrel)
              (let* ([l (cadr funcrel)] [code ($c-func-code-record (local-label-func l))])
                (record-case code
                  [(code) (func subtype free name arity-mask size code-list info)
                   (set-car!
                     funcrel
                     (let ([offset (local-label-offset l)])
                       (if offset
                           (fx+ (fx- size offset) (car funcrel) (constant code-data-disp))
                           (car funcrel))))
                   (set-car! (cdr funcrel) code)]
                  [else (sorry! who "unexpected record ~s" code)]))))
          (define touch-label!
            (lambda (l)
              (unless (libspec-label? l) (local-label-iteration-set! l 1))))
          (define LambdaBody
            (lambda (entry-block* block* func)
              #;(when (#%$assembly-output)
                (p-dot-graph block* (current-output-port))
                (p-graph block* 'whatever (current-output-port) unparse-L16))
              (let ([block* (cons (car entry-block*) (remq (car entry-block*) block*))])
                (for-each (lambda (block) (let ([l (block-label block)]) (when l (local-label-iteration-set! l 0) (local-label-func-set! l func)))) block*)
                (fluid-let ([current-func func])
                  (let loop ([block* (reverse block*)] [chunk* '()] [offset 0])
                    (if (null? block*)
                        (munge chunk* offset)
                        (let ([block (car block*)])
                          (let-values ([(code* chunk* offset) (Block block chunk* offset)])
                            (let ([chunk (make-lchunk (block-label block) code*)])
                              (let ([offset (fx+ (chunk-size chunk) offset)])
                                (let ([l (block-label block)])
                                  (when l
                                    (local-label-offset-set! l offset)
                                    (when (fx= (local-label-iteration l) 1) (set! munge-recur? #t))))
                                (loop (cdr block*) (cons chunk chunk*) offset)))))))))))
          (define Block
            (lambda (block chunk* offset)
              (let f ([e* (block-effect* block)])
                (if (null? e*)
                    (Exit block chunk* offset)
                    (let-values ([(code* chunk* offset) (f (cdr e*))])
                      (Effect (car e*) code* chunk* offset))))))
          (define Exit
            (lambda (block chunk* offset)
              (define do-goto
                (lambda (b)
                  (let ([l (block-label b)])
                    (safe-assert l)
                    (touch-label! l)
                    (let ([chunk (asm-jump l offset)])
                      (values '() (cons chunk chunk*) (fx+ (chunk-size chunk) offset))))))
              (cond
                [(goto-block? block) (do-goto (goto-block-next block))]
                [(joto-block? block) (do-goto (joto-block-next block))]
                [(if-block? block)
                 (let ([l1 (block-label (if-block-true block))] [l2 (block-label (if-block-false block))])
                   (safe-assert l1 l2)
                   (touch-label! l1)
                   (touch-label! l2)
                   (let-values ([(code* chunk) (Pred (if-block-pred block) l1 l2 offset)])
                     (values code* (cons chunk chunk*) (fx+ (chunk-size chunk) offset))))]
                [(tail-block? block) (Tail (tail-block-tail block) chunk* offset)]
                [(newframe-block? block) (do-goto (newframe-block-next block))]
                [else (sorry! who "unrecognized block ~s" block)]))))
        (Tail : Tail (ir chunk* offset) -> * (code* chunk* offset)
          [(asm-return) (values (asm-return) chunk* offset)]
          [(asm-c-return ,info) (values (asm-c-return info) chunk* offset)]
          [(jump (label-ref ,l ,offset0))
           (values (asm-direct-jump l offset0) chunk* offset)]
          [(jump (literal ,info))
           (values (asm-literal-jump info) chunk* offset)]
          [(jump ,t)
           (values (asm-indirect-jump t) chunk* offset)]
          [(goto ,l)
           (safe-assert (libspec-label? l))
           (values (asm-library-jump l) chunk* offset)])
        (Program : Program (ir) -> * (code)
          [(labels ([,l* ,[Lambda->func : le* -> func*]] ...) ,l)
           (define-syntax traceit
             (syntax-rules (x)
               [(_ name) (set! name (let ([t name]) (lambda args (apply t args))))]))
           (fluid-let ([funcrel* '()] [aop ($assembly-output)] [munge-recur? #f])
             (for-each local-label-func-set! l* func*)
             (let ([ptrace* (map CaseLambdaExpr le* func*)])
               (for-each resolve-funcrel! funcrel*)
               (when aop
                 (for-each (lambda (ptrace) (ptrace aop)) ptrace*)
                 (flush-output-port aop))
               (local-label-func l)))])
        (Lambda->func : CaseLambdaExpr (ir) -> * (func)
          [(lambda ,info (,entry-block* ...) (,block* ...)) (make-$c-func)])
        ; the final version of code* (which has things resolved)
        (CaseLambdaExpr : CaseLambdaExpr (ir func) -> * ()
          [(lambda ,info (,entry-block* ...) (,block* ...))
           #;(let ()
               (define block-printer
                 (lambda (unparser name block*)
                   (p-dot-graph block* (current-output-port))
                   (p-graph block* name (current-output-port) unparser)))
               (block-printer unparse-L16 (info-lambda-name info) block*))
           (let-values ([(code* trace* code-size) (LambdaBody entry-block* block* func)])
             ($c-make-code
               func
               (info-lambda-flags info)
               (length (info-lambda-fv* info))
               (info-lambda-name info)
               (interface*->mask (info-lambda-interface* info))
               code-size
               code*
               (cond
                 [(info-lambda-ctci info) =>
                  (lambda (ctci)
                    (include "types.ss")
                    (make-code-info
                      (info-lambda-src info)
                      (info-lambda-sexpr info)
                      (and (eq? (info-lambda-closure-rep info) 'closure)
                           (let f ([fv* (info-lambda-fv* info)] [n 0])
                             (if (null? fv*)
                                 (make-vector n #f)
                                 (let ([v (f (cdr fv*) (fx+ n 1))])
                                   (cond
                                     [(uvar-source (car fv*)) =>
                                      (lambda (source) (vector-set! v n (unannotate source)))])
                                   v))))
                      (ctci-live ctci)
                      (let ([v (vector-map
                                 (let ([n (fx+ (constant code-data-disp) (constant size-rp-header) code-size)])
                                   (lambda (ctrpi)
                                     (make-rp-info
                                       (fx- n (local-label-offset (ctrpi-label ctrpi)))
                                       (ctrpi-src ctrpi)
                                       (ctrpi-sexpr ctrpi)
                                       (ctrpi-mask ctrpi))))
                                 (list->vector (ctci-rpi* ctci)))])
                        (vector-sort! (lambda (x y) (fx< (rp-info-offset x) (rp-info-offset y))) v)
                        v)))]
                 [(and (generate-procedure-source-information)
                       (info-lambda-src info)) =>
                  (lambda (src)
                    (include "types.ss")
                    (make-code-info src #f #f #f #f))]
                 [else #f])
               (info-lambda-pinfo* info))
             (lambda (p) (c-trace (info-lambda-name info) code-size trace* p)))])
        (Effect : Effect (ir code* chunk* offset) -> * (code* chunk* offset)
          [(rp-header ,mrvl ,fs ,lpm) (values (asm-rp-header code* mrvl fs lpm current-func #f) chunk* offset)]
          [(set! ,x (label-ref ,l ,offset1))
           (guard (eq? (local-label-func l) current-func))
           (let ([chunk (make-chunk code*)])
             (let ([offset (fx+ (chunk-size chunk) offset)] [chunk* (cons chunk chunk*)])
               (let ([chunk (asm-return-address x l offset1 offset)])
                 (values '() (cons chunk chunk*) (fx+ (chunk-size chunk) offset)))))]
          [(set! ,lvalue (asm ,info ,proc ,t* ...)) (values (apply proc code* lvalue t*) chunk* offset)]
          [(set! ,lvalue ,rhs) (values (asm-move code* lvalue rhs) chunk* offset)]
          [(asm ,info ,proc ,t* ...) (values (apply proc code* t*) chunk* offset)])
        (Pred : Pred (ir l1 l2 offset) -> * (code* chunk)
          [(asm ,info ,proc ,t* ...) (apply proc l1 l2 offset t*)])
        (Program ir))

      (define-pass Triv->rand : (L16 Triv) (ir) -> * (operand)
        (Triv : Triv (ir) -> * (operand)
          [,x (cons 'reg x)]
          [(mref ,x1 ,x2 ,imm)
           (if (eq? x2 %zero)
               `(disp ,imm ,x1)
               `(index ,imm ,x2 ,x1))]
          [(literal ,info)
           `(,(if (info-literal-indirect? info) 'literal@ 'literal)
              ,(info-literal-offset info)
              ,(let ([type (info-literal-type info)])
                 (if (eq? type 'closure)
                     ($c-make-closure (local-label-func (info-literal-addr info)))
                     `(,type ,(info-literal-addr info)))))]
          [(immediate ,imm) `(imm ,imm)]
          [(label-ref ,l ,offset) (make-funcrel 'literal l offset)])
        (Triv ir))

      (define build-mem-opnd
        (lambda (base index offset)
          (let ([offset (nanopass-case (L16 Triv) offset [(immediate ,imm) imm])])
            (if (eq? index %zero)
                `(disp ,offset ,base)
                `(index ,offset ,base ,index)))))

      (define asm-size*
        (lambda (x*)
          (fold-left (lambda (size x) (fx+ size (asm-size x))) 0 x*)))

      (define-syntax Trivit
        (syntax-rules ()
          [(_ (x ...) b0 b1 ...) (let ([x (Triv->rand x)] ...) b0 b1 ...)]))

      (define-syntax aop-cons*
        (syntax-rules ()
          [(_ asm e1 e2 ...)
           (let ([ls (cons* e1 e2 ...)])
             (if aop (cons asm ls) ls))]))

      (define interface*->mask
        (lambda (i*)
          (fold-left (lambda (mask i)
                       (logor mask
                         (if (< i 0)
                             (- (ash 1 (- -1 i)))
                             (ash 1 i))))
            0 i*)))

      (architecture assembler)

      (import asm-module))

    (module (np-allocate-registers)
      (define-threaded spillable*)
      (define-threaded unspillable*)
      (define-threaded max-fv)
      (define-threaded max-fs@call)
      (define-threaded poison-cset)

      (define no-live* empty-tree)

      (define union-live
       ; union live1 and live2.  result is eq? to live1 if result is same as live1.
        (lambda (live1 live2 live-size)
          (tree-merge live1 live2 live-size)))

      (define same-live?
        (lambda (live1 live2)
          (tree-same? live1 live2)))

      (define live?
        (lambda (live* live-size x)
          (tree-bit-set? live* live-size (var-index x))))

      (define get-live-vars
        (lambda (live* live-size v)
          (tree-extract live* live-size v)))

      (define make-add-var
        (lambda (live-size)
          ; add x to live*.  result is eq? to live* if x is already in live*.
          (lambda (live* x)
            (let ([index (var-index x)])
              (if index
                  (let ([new (tree-bit-set live* live-size index)])
                    (safe-assert (or (eq? new live*) (not (tree-same? new live*))))
                    new)
                  live*)))))

      (define make-remove-var
        ; remove x from live*.  result is eq? to live* if x is not in live*.
        (lambda (live-size)
          (lambda (live* x)
            (let ([index (var-index x)])
              (if index
                  (let ([new (tree-bit-unset live* live-size (var-index x))])
                    (safe-assert (or (eq? new live*) (not (tree-same? new live*))))
                    new)
                  live*)))))

      (module (make-empty-cset make-full-cset cset-full? conflict-bit-set! conflict-bit-unset! conflict-bit-set? conflict-bit-count cset-merge! cset-copy cset-for-each extract-conflicts)
        (define-record-type cset
          (nongenerative)
          (fields size (mutable tree)))

        (define make-empty-cset
          (lambda (size)
            (make-cset size empty-tree)))

        (define make-full-cset
          (lambda (size)
            (make-cset size full-tree)))

        (define cset-full?
          (lambda (cset)
            (eq? (cset-tree cset) full-tree)))

        (define conflict-bit-set!
          (lambda (cset offset)
            (cset-tree-set! cset
              (tree-bit-set (cset-tree cset) (cset-size cset) offset))))

        (define conflict-bit-unset!
          (lambda (cset offset)
            (cset-tree-set! cset
              (tree-bit-unset (cset-tree cset) (cset-size cset) offset))))

        (define conflict-bit-set?
          (lambda (cset offset)
            (tree-bit-set? (cset-tree cset) (cset-size cset) offset)))

        (define conflict-bit-count
          (lambda (cset)
            (tree-bit-count (cset-tree cset) (cset-size cset))))

        (define cset-merge!
          (lambda (cset1 cset2)
            (cset-tree-set! cset1 (tree-merge (cset-tree cset1) (cset-tree cset2) (cset-size cset1)))))

        (define cset-copy
          (lambda (cset)
            (make-cset (cset-size cset) (cset-tree cset))))

        (define cset-for-each
          (lambda (cset proc)
            (tree-for-each (cset-tree cset) (cset-size cset) 0 (cset-size cset) proc)))

        (define extract-conflicts
          (lambda (cset v)
            (tree-extract (cset-tree cset) (cset-size cset) v)))
        )

      (define do-live-analysis!
        (lambda (live-size entry-block*)
          (define add-var (make-add-var live-size))
          (define remove-var (make-remove-var live-size))
          (define-who scan-block
            ; if we maintain a list of kills and a list of useless variables for
            ; each block, and we discover on entry to scan-block that the useless
            ; variables are still useless (not live in "out"), we can compute the
            ; new in set without scanning the block by removing the kills from
            ; the out set and unioning the result with the saved in set.  should
            ; try this and see if it is enough of a win to justify the added
            ; complexity.
            (lambda (block out)
              (define Triv
                (lambda (out t)
                  (nanopass-case (L15a Triv) t
                    [(mref ,x1 ,x2 ,imm) (add-var (add-var out x2) x1)]
                    [,x (add-var out x)]
                    [else out])))
              (define Rhs
                (lambda (out rhs)
                  (nanopass-case (L15a Rhs) rhs
                    [(inline ,info ,value-prim ,t* ...)
                     (let* ([out (if (info-kill*? info) (fold-left remove-var out (info-kill*-kill* info)) out)]
                            [out (if (info-kill*-live*? info) (fold-left add-var out (info-kill*-live*-live* info)) out)])
                       (fold-left Triv out t*))]
                    [else (Triv out rhs)])))
              (define Pred
                (lambda (out p)
                  (nanopass-case (L15a Pred) p
                    [(inline ,live-info ,info ,pred-prim ,t* ...)
                     (let* ([out (if (info-kill*? info) (fold-left remove-var out (info-kill*-kill* info)) out)]
                            [out (if (info-kill*-live*? info) (fold-left add-var out (info-kill*-live*-live* info)) out)])
                       (live-info-live-set! live-info out)
                       (fold-left Triv out t*))]
                    [else (sorry! who "unexpected pred ~s" p)])))
              (define Tail
                (lambda (out tl)
                  (nanopass-case (L15a Tail) tl
                    [(goto ,l)
                     (safe-assert (libspec-label? l))
                     (fold-left add-var no-live* (libspec-label-live-reg* l))]
                    [(asm-return ,reg* ...)
                     (safe-assert (eq? out no-live*))
                     (fold-left add-var no-live* reg*)]
                    [(asm-c-return ,info ,reg* ...)
                     (safe-assert (eq? out no-live*))
                     (fold-left add-var no-live* reg*)]
                    [(jump ,live-info ,t (,var* ...))
                     (let ([out (fold-left add-var out var*)])
                       (live-info-live-set! live-info out)
                       (Triv out t))]
                    [else (sorry! who "unexpected tail instruction ~s" tl)])))
              (define Effect*
                (lambda (out instr*)
                  (fold-left
                    (lambda (out instr)
                      (nanopass-case (L15a Effect) instr
                        [(set! ,live-info ,x ,rhs)
                         (if (var-index x)
                             (let ([new-out (remove-var out x)])
                               (if (and (eq? new-out out)
                                        (nanopass-case (L15a Rhs) rhs
                                          [(inline ,info ,value-prim ,t* ...) (primitive-pure? value-prim)]
                                          [else #t]))
                                   (begin
                                     (live-info-useless-set! live-info #t)
                                     out)
                                   (begin
                                     (live-info-useless-set! live-info #f)
                                     (live-info-live-set! live-info new-out)
                                     (Rhs new-out rhs))))
                             (begin
                               (live-info-live-set! live-info out)
                               (Rhs out rhs)))]
                        [(set! ,live-info (mref ,x1 ,x2 ,imm) ,rhs)
                         (live-info-live-set! live-info out)
                         (Rhs (add-var (add-var out x1) x2) rhs)]
                        [(inline ,live-info ,info ,effect-prim ,t* ...)
                         (let ([out (if (info-kill*? info) (fold-left remove-var out (info-kill*-kill* info)) out)])
                           (live-info-live-set! live-info out)
                           (let ([out (fold-left Triv out t*)])
                             (if (info-kill*-live*? info)
                                 (fold-left add-var out (info-kill*-live*-live* info))
                                 out)))]
                        [(remove-frame ,live-info ,info) (live-info-live-set! live-info out) out]
                        [(restore-local-saves ,live-info ,info) (live-info-live-set! live-info out) out]
                        [(shift-arg ,live-info ,reg ,imm ,info) (live-info-live-set! live-info out) out]
                        [(overflow-check ,live-info) (live-info-live-set! live-info out) out]
                        [(overflood-check ,live-info) (live-info-live-set! live-info out) out]
                        [(fcallable-overflow-check ,live-info) (live-info-live-set! live-info out) out]
                        [(check-live ,live-info ,reg* ...) (live-info-live-set! live-info out) out]
                        [else out]))
                    out instr*)))
              ; NB: consider storing instructions in reverse order back in expose-basic-blocks
              (let ([effect* (reverse (block-effect* block))])
                (cond
                  [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (Effect* out effect*)]
                  [(if-block? block) (Effect* (Pred out (if-block-pred block)) effect*)]
                  [(tail-block? block) (Effect* (Tail out (tail-block-tail block)) effect*)]
                  [else (sorry! who "unrecognized block ~s" block)]))))
          (define force-live-in!
            (lambda (block)
              (when (eq? (block-live-in block) 'uninitialized)
                (if (block-seen? block)
                    ; think we need need not recur on in-link* here even though we changed in
                    ; - if an in-link is seen, it's already on the worklist
                    ; - if an in-link is not seen, we must not have visited it yet or it would
                    ;   have already forced us.  someone will visit it later unless it's
                    ;   orphaned, and we think we have no orphaned blocks
                    (block-live-in-set! block no-live*)
                    (begin
                      (block-seen! block #t)
                      (do-live! block))))))
          (define different?
            (lambda (out old-out)
              (or (eq? old-out 'uninitialized)
                  (not (same-live? out old-out)))))
          (define propagate-live!
            (lambda (block out)
              ; NB: could record out, and if out hasn't changed, skip the scan
              (let ([in (scan-block block out)])
                (when (different? in (block-live-in block))
                  (block-live-in-set! block in)
                  (let f ([block* (block-in-link* block)])
                    (unless (null? block*)
                      (let ([block (car block*)])
                        (if (block-seen? block)
                            (f (cdr block*))
                            (begin
                              (block-seen! block #t)
                              (f (cdr block*))
                              (do-live! block))))))))))
          (define-who do-live!
            (lambda (block)
              (safe-assert (block-seen? block))
              (cond
                [(goto-block? block)
                 (let ([next-block (goto-block-next block)])
                   (force-live-in! next-block)
                   (block-seen! block #f)
                   (propagate-live! block (block-live-in next-block)))]
                [(if-block? block)
                 (let ([true-block (if-block-true block)] [false-block (if-block-false block)])
                   (force-live-in! true-block)
                   (force-live-in! false-block)
                   (block-seen! block #f)
                   (let ([out (union-live (block-live-in true-block) (block-live-in false-block) live-size)])
                     (when (different? out (if-block-live-out block))
                       (if-block-live-out-set! block out)
                       (propagate-live! block out))))]
                [(joto-block? block)
                 (let ([next-block (joto-block-next block)])
                   (force-live-in! next-block)
                   (block-seen! block #f)
                   (propagate-live! block
                     (let loop ([nfv* (joto-block-nfv* block)] [i 1] [next (block-live-in next-block)])
                       (if (or (null? nfv*) (fx> i max-fv))
                           next
                           (loop (cdr nfv*) (fx+ i 1)
                             (let ([new-next (remove-var next (get-fv i))])
                               (if (eq? new-next next)
                                   next
                                   (add-var next (car nfv*)))))))))]
                [(newframe-block? block)
                 (let ([next-block (newframe-block-next block)]
                       [rp-block* (newframe-block-rp* block)]
                       [rp-block (newframe-block-rp block)])
                   (force-live-in! next-block)
                   (for-each force-live-in! rp-block*)
                   (force-live-in! rp-block)
                   (block-seen! block #f)
                   (let ([rp (block-live-in rp-block)] [newframe-info (newframe-block-info block)])
                     (let ([call (if (eq? (newframe-block-live-rp block) rp)
                                     (newframe-block-live-call block)
                                     (begin
                                       (newframe-block-live-rp-set! block rp)
                                       (let ([call (add-var
                                                     (fold-left
                                                       (lambda (live* x*) (fold-left remove-var live* x*))
                                                       rp
                                                       (cons*
                                                         ; could base set of registers to kill on expected return values
                                                         (reg-cons* %ret %ac0 arg-registers)
                                                         (info-newframe-cnfv* newframe-info)
                                                         (info-newframe-nfv** newframe-info)))
                                                     (get-fv 0))])
                                         (newframe-block-live-call-set! block call)
                                         call)))])
                       (let ([out (union-live
                                    (fold-left (lambda (live b) (union-live (block-live-in b) live live-size))
                                      (block-live-in next-block) rp-block*)
                                    (fold-left add-var call (info-newframe-cnfv* newframe-info))
                                    live-size)])
                         (when (different? out (newframe-block-live-out block))
                           (newframe-block-live-out-set! block out)
                           (propagate-live! block out))))))]
                [(tail-block? block)
                 (block-seen! block #f)
                 (propagate-live! block no-live*)]
                [else (sorry! who "unrecognized block ~s" block)])))
          (for-each
            (lambda (entry-block)
              (when (eq? (block-live-in entry-block) 'uninitialized)
                (block-seen! entry-block #t)
                (do-live! entry-block)))
            entry-block*)))

      (define-who check-entry-live!
        ; when enabled, spits out messages about uvars and unexpected registers that are live
        ; on entry.  there should never be any live uvars.  for procedures that started life
        ; as ordinary lambda expressions, there shouldn't be anything but ac0, cp, and argument
        ; registers, which we weed out here.  for library routines, there are often additional
        ; registers, sometimes for good reason and sometimes because we are lazy and didn't give
        ; ourselves a mechanism to prune out unneeded saves and restores.  for foreign-callable
        ; procedures, C argument registers and callee-save registers might show up live.
        ; we could enable a variant of this always that just checks normal procedures.  also,
        ; it might be nice to make it a bit more efficient, though it probably doesn't matter.
        (lambda (name live-size varvec entry-block*)
          (for-each
            (lambda (entry-block)
              (define okay-live?
                (lambda (x)
                  (or (fv? x)
                      (eq? x %ac0)
                      (meta-cond
                        [(real-register? '%cp) (eq? x %cp)]
                        [else #f])
                      (memq x arg-registers))))
              (let ([undead (remp okay-live? (get-live-vars (block-live-in entry-block) live-size varvec))])
                (unless (null? undead)
                  (printf "Warning: live on entry to ~a: ~s\n" name undead))))
            entry-block*)))

      (define-who record-call-live!
        (lambda (block* varvec)
          (for-each
            (lambda (block)
              (when (newframe-block? block)
                (let ([newframe-info (newframe-block-info block)])
                  (let ([call-live* (get-live-vars (newframe-block-live-call block) (vector-length varvec) varvec)])
                    (for-each
                      (lambda (x)
                        (define fixnum (lambda (x) (if (fixnum? x) x (most-positive-fixnum))))
                        (when (uvar? x)
                          (uvar-spilled! x #t)
                          (unless (block-pariah? block)
                            (uvar-save-weight-set! x
                              (fixnum
                                (+ (uvar-save-weight x)
                                   (* (info-newframe-weight newframe-info) 2)))))))
                      call-live*)
                    (info-newframe-call-live*-set! newframe-info call-live*)))))
              block*)))

      ; maintain move sets as (var . weight) lists, sorted by weight (largest first)
      ; 2014/06/26: allx move set size averages .79 elements with a max of 12, so no
      ; need for anything fancier than this weighted version of insertion sort
      (define $add-move!
        (lambda (x1 x2 weight)
          (when (uvar? x1)
            (when (or (not (uvar-poison? x1)) (fv? x2))
              (uvar-move*-set! x1
                (call-with-values
                  (lambda ()
                    (let f ([move* (uvar-move* x1)])
                      (if (null? move*)
                          (values (cons x2 weight) move*)
                          (let ([move (car move*)] [move* (cdr move*)])
                            (if (eq? (car move) x2)
                                (values (cons (car move) (fx+ (cdr move) weight)) move*)
                                (let-values ([(move2 move*) (f move*)])
                                  (if (fx> (cdr move2) (cdr move))
                                      (values move2 (cons move move*))
                                      (values move (cons move2 move*)))))))))
                  cons))))))

      (define-who identify-poison!
        (lambda (kspillable varvec live-size block*)
          (define kpoison 0)
          (define increment-live-counts!
            (lambda (live)
              (tree-for-each live live-size 0 kspillable
                (lambda (offset)
                  (let ([x (vector-ref varvec offset)])
                    (let ([range (fx+ (uvar-live-count x) 1)])
                      (when (fx= range 2)
                        (uvar-poison! x #t)
                        (set! kpoison (fx+ kpoison 1)))
                      (uvar-live-count-set! x range)))))))
          (define Effect
            (lambda (live* e)
              (nanopass-case (L15a Effect) e
                [(set! ,live-info ,x ,rhs)
                 (guard (uvar? x))
                 (if (live-info-useless live-info)
                     live*
                     (cons (live-info-live live-info) live*))]
                [else live*])))
          (let ([vlive (list->vector (fold-left (lambda (live* block) (fold-left Effect live* (block-effect* block))) '() block*))])
            (let ([nvlive (vector-length vlive)])
              (let refine ([skip 64] [stride 64])
                (do ([i (fx- skip 1) (fx+ i stride)])
                    ((fx>= i nvlive))
                  (increment-live-counts! (vector-ref vlive i)))
                (unless (or (fx= stride 16) (< (* (fx- kspillable kpoison) (fx* stride 2)) 1000000))
                  (refine (fxsrl skip 1) skip)))))))

      (define-who do-spillable-conflict!
        (lambda (kspillable kfv varvec live-size block*)
          (define remove-var (make-remove-var live-size))
          (define add-move!
            (lambda (x1 x2)
              (when (var-index x2)
                ($add-move! x1 x2 2)
                ($add-move! x2 x1 2))))
          (define add-conflict!
            (lambda (x out)
              ; invariants:
              ;   all poison spillables explicitly point to all spillables
              ;   all non-poison spillables implicitly point to all poison spillables via poison-cset
              (let ([x-offset (var-index x)])
                (when x-offset
                  (if (and (fx< x-offset kspillable) (uvar-poison? x))
                      (tree-for-each out live-size kspillable (fx+ kspillable kfv)
                        (lambda (y-offset)
                          ; frame y -> poison spillable x
                          (conflict-bit-set! (var-spillable-conflict* (vector-ref varvec y-offset)) x-offset)))
                      (let ([cset (var-spillable-conflict* x)])
                        (if (fx< x-offset kspillable)
                            (begin
                              (tree-for-each out live-size 0 kspillable
                                (lambda (y-offset)
                                  (let ([y (vector-ref varvec y-offset)])
                                    (unless (uvar-poison? y)
                                      ; non-poison spillable x -> non-poison spillable y
                                      (conflict-bit-set! cset y-offset)
                                      ; and vice versa
                                      (conflict-bit-set! (var-spillable-conflict* y) x-offset)))))
                              (tree-for-each out live-size kspillable live-size
                                (lambda (y-offset)
                                  (let ([y (vector-ref varvec y-offset)])
                                    ; frame or register y -> non-poison spillable x
                                    (conflict-bit-set! (var-spillable-conflict* y) x-offset)))))
                            (if (fx< x-offset (fx+ kspillable kfv))
                                (tree-for-each out live-size 0 kspillable
                                  (lambda (y-offset)
                                    ; frame x -> poison or non-poison spillable y
                                    (conflict-bit-set! cset y-offset)))
                                (tree-for-each out live-size 0 kspillable
                                  (lambda (y-offset)
                                    (unless (uvar-poison? (vector-ref varvec y-offset))
                                      ; register x -> non-poison spillable y
                                      (conflict-bit-set! cset y-offset))))))))))))
          (define Rhs
            (lambda (rhs live)
              (nanopass-case (L15a Rhs) rhs
                [(inline ,info ,value-prim ,t* ...)
                 (guard (info-kill*? info))
                 (for-each (lambda (x) (add-conflict! x live)) (info-kill*-kill* info))]
                [else (void)])))
          (define Effect
            (lambda (e new-effect*)
              (nanopass-case (L15a Effect) e
                [(set! ,live-info ,x ,rhs)
                 (if (live-info-useless live-info)
                     new-effect*
                     (let ([live (live-info-live live-info)])
                       (when (var-index x)
                         (if (and (var? rhs) (var-index rhs))
                             (begin
                               (add-conflict! x (remove-var live rhs))
                               (add-move! x rhs))
                             (add-conflict! x live)))
                       (Rhs rhs live)
                       (cons e new-effect*)))]
                [(set! ,live-info ,lvalue ,rhs) (Rhs rhs (live-info-live live-info)) (cons e new-effect*)]
                [(inline ,live-info ,info ,effect-prim ,t* ...)
                 (guard (info-kill*? info))
                 (let ([live (live-info-live live-info)])
                   (for-each (lambda (x) (add-conflict! x live)) (info-kill*-kill* info)))
                 (cons e new-effect*)]
                [else (cons e new-effect*)])))
          (do ([i 0 (fx+ i 1)])
              ((fx= i kspillable))
            (let ([x (vector-ref varvec i)])
              (if (uvar-poison? x)
                  (begin
                    (conflict-bit-set! poison-cset i)
                    ; leaving each poison spillable in conflict with itself, but this shouldn't matter
                    ; since we never ask for the degree of a poison spillable
                    (var-spillable-conflict*-set! x (make-full-cset kspillable)))
                  (var-spillable-conflict*-set! x (make-empty-cset kspillable)))))
          (do ([i kspillable (fx+ i 1)])
              ((fx= i live-size))
            (var-spillable-conflict*-set! (vector-ref varvec i) (make-empty-cset kspillable)))
          (for-each
            (lambda (block)
              (block-effect*-set! block
                (fold-right Effect '() (block-effect* block))))
            block*)))

      (define-who show-conflicts
        (lambda (name varvec unvarvec)
          (define any? #f)
          (printf "\n~s conflicts:" name)
          (for-each
            (lambda (x)
              (let ([ls (append
                          (let ([cset (var-spillable-conflict* x)])
                            (if cset (extract-conflicts cset varvec) '()))
                          (let ([cset (var-unspillable-conflict* x)])
                            (if cset (extract-conflicts cset unvarvec) '())))])
                (unless (null? ls) (set! any? #t) (printf "\n~s:~{ ~s~}" x ls))))
            (append spillable* unspillable* (vector->list regvec) (map get-fv (iota (fx+ max-fv 1)))))
          (unless any? (printf " none"))
          (newline)))

      (module (assign-frame! assign-new-frame!)
        (define update-conflict!
          (lambda (fv spill)
            (let ([cset1 (var-spillable-conflict* fv)]
                  [cset2 (var-spillable-conflict* spill)])
              (if cset1
                  (cset-merge! cset1 cset2)
                  ; tempting to set to cset2 rather than (cset-copy cset2), but this would not be
                  ; correct for local saves, which need their unaltered sets for later, and copying
                  ; is cheap anyway.
                  (var-spillable-conflict*-set! fv (cset-copy cset2))))
            (unless (uvar-poison? spill) (cset-merge! (var-spillable-conflict* fv) poison-cset))))

        (define assign-frame!
          (lambda (spill*)
            (define sort-spill*
              ; NB: sorts based on likelihood of successfully assigning move-related vars to the same location
              ; NB: probably should sort based on value of assigning move-related vars to the same location,
              ; NB: i.e., taking into account the ref-weight
              (lambda (spill*)
                (map car
                  (list-sort
                    (lambda (x y) (fx> (cdr x) (cdr y)))
                    (map (lambda (x)
                           (define relevant?
                             (lambda (x)
                               (or (fv? x) (and (uvar? x) (uvar-spilled? x)))))
                           (do ([move* (uvar-move* x) (cdr move*)]
                                [w 0 (let ([move (car move*)])
                                       (if (relevant? (car move))
                                           (fx+ w (cdr move))
                                           w))])
                             ((null? move*) (cons x w))))
                      spill*)))))
            (define find-move-related-home
              (lambda (x0 succ fail)
                (define conflict-fv?
                  (lambda (x fv)
                    (let ([cset (var-spillable-conflict* fv)])
                      (and cset (conflict-bit-set? cset (var-index x))))))
                (let f ([x x0] [work* '()] [clear-seen! void])
                  (if (uvar-seen? x)
                      (if (null? work*) (begin (clear-seen!) (fail)) (f (car work*) (cdr work*) clear-seen!))
                      (let ([clear-seen! (lambda () (uvar-seen! x #f) (clear-seen!))])
                        (uvar-seen! x #t)
                        (let loop ([move* (uvar-move* x)] [work* work*])
                          (if (null? move*)
                              (if (null? work*) (begin (clear-seen!) (fail)) (f (car work*) (cdr work*) clear-seen!))
                              (let ([var (caar move*)] [move* (cdr move*)])
                                (define try-fv
                                  (lambda (fv)
                                    (if (conflict-fv? x0 fv)
                                        (loop move* work*)
                                        (begin
                                          (safe-assert (not (eq? fv (get-fv 0))))
                                          (begin (clear-seen!) (succ fv))))))
                                (if (fv? var)
                                    (try-fv var)
                                    (if (uvar? var)
                                        (let ([fv (uvar-location var)])
                                          (if (fv? fv)
                                              (try-fv fv)
                                              (loop move* (cons var work*))))
                                        (loop move* work*)))))))))))
            (define find-home!
              (lambda (spill max-fv first-open)
                (define return
                  (lambda (home max-fv first-open)
                    (uvar-location-set! spill home)
                    (update-conflict! home spill)
                    (values max-fv first-open)))
                (find-move-related-home spill
                  (lambda (home) (return home max-fv first-open))
                  (lambda ()
                    (let f ([first-open first-open])
                      (let* ([fv (get-fv first-open)] [cset (var-spillable-conflict* fv)])
                        (if (and cset (cset-full? cset))
                            (f (fx+ first-open 1))
                            (let ([spill-offset (var-index spill)])
                              (let f ([fv-offset first-open] [fv fv] [cset cset])
                                (if (and cset (conflict-bit-set? cset spill-offset))
                                    (let* ([fv-offset (fx+ fv-offset 1)] [fv (get-fv fv-offset)] [cset (var-spillable-conflict* fv)])
                                      (f fv-offset fv cset))
                                    (return fv (fxmax fv-offset max-fv) first-open)))))))))))
            (define find-homes!
              (lambda (spill* max-fv first-open)
                (if (null? spill*)
                    max-fv
                    (let-values ([(max-fv first-open) (find-home! (car spill*) max-fv first-open)])
                      (find-homes! (cdr spill*) max-fv first-open)))))
            ; NOTE: call-live uvars should be sorted so that those that are call-live with few other
            ; variables are earlier in the list (and more likely to get a low frame location);
            ; additionally if they are live across many frames they should be prioritized over those
            ; live across only a few (only when setup-nfv?)
            (set! max-fv (find-homes! (sort-spill* spill*) max-fv 1))))

        (define-pass assign-new-frame! : (L15a Dummy) (ir lambda-info live-size varvec block*) -> (L15b Dummy) ()
          (definitions
            (define remove-var (make-remove-var live-size))
            (define find-max-fv
              (lambda (call-live*)
                (fold-left
                  (lambda (call-max-fv x)
                    (fxmax (fv-offset (if (uvar? x) (uvar-location x) x)) call-max-fv))
                  -1 call-live*)))
            (define cool?
              (lambda (base nfv*)
                (let loop ([nfv* nfv*] [offset base])
                  (or (null? nfv*)
                      (and (or (not (car nfv*))
                               (let ([cset (var-spillable-conflict* (get-fv offset))])
                                 (not (and cset (conflict-bit-set? cset (var-index (car nfv*)))))))
                           (loop (cdr nfv*) (fx+ offset 1)))))))
            (define assign-new-frame!
              (lambda (cnfv* nfv** call-live*)
                (define set-offsets!
                  (lambda (nfv* offset)
                    (if (null? nfv*)
                        (set! max-fv (fxmax offset max-fv))
                        (let ([nfv (car nfv*)] [home (get-fv offset)])
                          (uvar-location-set! nfv home)
                          (update-conflict! home nfv)
                          (set-offsets! (cdr nfv*) (fx+ offset 1))))))
                (let ([arg-offset (fx+ (length cnfv*) 1)]) ; +1 for return address slot
                  (let loop ([base (fx+ (find-max-fv call-live*) 1)])
                    (let ([arg-base (fx+ base arg-offset)])
                      (if (and (cool? base cnfv*) (andmap (lambda (nfv*) (cool? arg-base nfv*)) nfv**))
                          (begin
                            (set! max-fs@call (fxmax max-fs@call base)) ; max frame size @ call in ptrs
                            (set-offsets! cnfv* base)
                            (for-each (lambda (nfv*) (set-offsets! nfv* arg-base)) nfv**)
                            base)
                          (loop (fx+ base 1))))))))
            (define build-mask
              (lambda (index*)
                (define bucket-width (if (fx> (fixnum-width) 32) 32 16))
                (let* ([nbits (fx+ (fold-left (lambda (m index) (fxmax m index)) -1 index*) 1)]
                       [nbuckets (fxdiv (fx+ nbits (fx- bucket-width 1)) bucket-width)]
                       [buckets (make-fxvector nbuckets 0)])
                  (for-each
                    (lambda (index)
                      (let-values ([(i j) (fxdiv-and-mod index bucket-width)])
                        (fxvector-set! buckets i (fxlogbit1 j (fxvector-ref buckets i)))))
                    index*)
                  (let f ([base 0] [len nbuckets])
                    (if (fx< len 2)
                        (if (fx= len 0)
                            0
                            (fxvector-ref buckets base))
                        (let ([half (fxsrl len 1)])
                          (logor
                            (bitwise-arithmetic-shift-left (f (fx+ base half) (fx- len half)) (fx* half bucket-width))
                            (f base half))))))))
            (define build-live-pointer-mask
              (lambda (live*)
                (build-mask
                  (fold-left
                    (lambda (index* live)
                      (define (cons-fv fv index*)
                        (let ([offset (fv-offset fv)])
                          (if (fx= offset 0) ; no bit for fv0
                              index*
                              (cons (fx- offset 1) index*))))
                      (cond
                        [(fv? live) (cons-fv live index*)]
                        [(eq? (uvar-type live) 'ptr) (cons-fv (uvar-location live) index*)]
                        [else index*]))
                    '() live*))))
            (define (process-info-newframe! info)
              (unless (info-newframe-frame-words info)
                (let ([call-live* (info-newframe-call-live* info)])
                  (info-newframe-frame-words-set! info
                    (let ([cnfv* (info-newframe-cnfv* info)])
                      (fx+ (assign-new-frame! cnfv* (cons (info-newframe-nfv* info) (info-newframe-nfv** info)) call-live*)
                        (length cnfv*))))
                  (info-newframe-local-save*-set! info
                    (filter (lambda (x) (and (uvar? x) (uvar-local-save? x))) call-live*)))))
            (define record-inspector-info!
              (lambda (src sexpr rpl call-live* lpm)
                (safe-assert (if call-live* rpl (not rpl)))
                (cond
                  [(and call-live* (info-lambda-ctci lambda-info)) =>
                   (lambda (ctci)
                     (let ([mask (build-mask
                                   (fold-left
                                     (lambda (i* x)
                                       (cond
                                         [(and (uvar? x) (uvar-iii x)) =>
                                          (lambda (index)
                                            (safe-assert
                                              (let ([name.offset (vector-ref (ctci-live ctci) index)])
                                                (logbit? (fx- (cdr name.offset) 1) lpm)))
                                            (cons index i*))]
                                         [else i*]))
                                     '() call-live*))])
                       (when (or src sexpr (not (eqv? mask 0)))
                         (ctci-rpi*-set! ctci (cons (make-ctrpi rpl src sexpr mask) (ctci-rpi* ctci))))))]))))
          (Pred : Pred (ir) -> Pred ())
          (Tail : Tail (ir) -> Tail ()
            [(jump ,live-info ,[t] (,var* ...)) `(jump ,live-info ,t)]
            [(asm-return ,reg* ...) `(asm-return)]
            [(asm-c-return ,info ,reg* ...) `(asm-c-return ,info)])
          (Effect : Effect (ir) -> Effect ())
          (foldable-Effect : Effect (ir new-effect*) -> * (new-effect*)
            [(return-point ,info ,rpl ,mrvl (,cnfv* ...))
             (process-info-newframe! info)
             (let ([lpm (build-live-pointer-mask (append cnfv* (info-newframe-call-live* info)))])
               (record-inspector-info! (info-newframe-src info) (info-newframe-sexpr info) rpl (info-newframe-call-live* info) lpm)
               (with-output-language (L15b Effect)
                 (safe-assert (< -1 lpm (ash 1 (fx- (info-newframe-frame-words info) 1))))
                 (cons `(rp-header ,mrvl ,(fx* (info-newframe-frame-words info) (constant ptr-bytes)) ,lpm) new-effect*)))]
            [(remove-frame ,live-info ,info)
             (process-info-newframe! info)
             (with-output-language (L15b Effect)
               (let ([live (live-info-live live-info)])
                 (cons*
                   `(fp-offset ,live-info ,(fx- (fx* (info-newframe-frame-words info) (constant ptr-bytes))))
                   `(overflood-check ,(make-live-info live))
                   new-effect*)))]
            [(restore-local-saves ,live-info ,info)
             (with-output-language (L15b Effect)
               (let ([live (live-info-live live-info)])
                 (let loop ([x* (filter (lambda (x) (live? live live-size x)) (info-newframe-local-save* info))]
                            [live live]
                            [new-effect* new-effect*])
                   (if (null? x*)
                       new-effect*
                       (let* ([x (car x*)] [live (remove-var live x)])
                         (loop (cdr x*) live
                           (cons `(set! ,(make-live-info live) ,x ,(uvar-location x)) new-effect*)))))))]
            [(shift-arg ,live-info ,reg ,imm ,info)
             (process-info-newframe! info)
             (with-output-language (L15b Effect)
               (let ([frame-words (info-newframe-frame-words info)])
                 (safe-assert (not (fx= frame-words 0)))
                 (let ([shift-offset (fx* frame-words (constant ptr-bytes))])
                   (safe-assert (fx> shift-offset 0))
                   (cons `(set! ,live-info (mref ,reg ,%zero ,imm) (mref ,reg ,%zero ,shift-offset)) new-effect*))))]
            [(check-live ,live-info ,reg* ...)
             (let ([live (fold-left (lambda (live reg)
                                      (let ([t (remove-var live reg)])
                                        (when (eqv? t live) (sorry! who "(check-live) ~s is not live" reg))
                                        t))
                           (live-info-live live-info)
                           reg*)])
               (unless (eqv? live no-live*)
                 (sorry! who "(check-live) unexpected live vars ~s" (get-live-vars live live-size varvec))))
             new-effect*]
            [else (cons (Effect ir) new-effect*)])
          (begin
            (for-each
              (lambda (x)
                ; NB: experiment with different comparisions.  might want ref weight
                ; NB: to be at least more than save weight to relieve register pressure.
                (when (and (uvar-spilled? x) (not (uvar-poison? x)) (fx>= (uvar-ref-weight x) (uvar-save-weight x)))
                  (uvar-local-save! x #t)))
              spillable*)
            (for-each
              (lambda (block)
                (block-effect*-set! block
                  (fold-right foldable-Effect
                    (cond
                      [(or (goto-block? block) (joto-block? block)) '()]
                      [(if-block? block) (if-block-pred-set! block (Pred (if-block-pred block))) '()]
                      [(tail-block? block) (tail-block-tail-set! block (Tail (tail-block-tail block))) '()]
                      [(newframe-block? block)
                       (let ([info (newframe-block-info block)])
                         (process-info-newframe! info)
                         (safe-assert (andmap (lambda (x) (live? (newframe-block-live-call block) live-size x)) (info-newframe-local-save* info)))
                         (with-output-language (L15b Effect)
                           (let ([live (newframe-block-live-out block)])
                             (fold-left
                               (lambda (new-effect* x)
                                 (let ([loc (uvar-location x)])
                                   ($add-move! x loc 2)
                                   (cons `(set! ,(make-live-info live) ,loc ,x) new-effect*)))
                               (cons `(fp-offset ,(make-live-info live) ,(fx* (info-newframe-frame-words info) (constant ptr-bytes))) '())
                               (info-newframe-local-save* info)))))]
                      [else (sorry! who "unrecognized block ~s" block)])
                    (block-effect* block))))
              block*)
            (for-each
              (lambda (x)
                (when (uvar-local-save? x)
                  (uvar-location-set! x #f)
                  (uvar-spilled! x #f)
                  (uvar-save-weight-set! x 0)))
              spillable*)
            `(dummy))))

      (define record-fp-offsets!
        (lambda (block*)
          (define-who record-fp-offsets!
            (lambda (block cur-off)
              (define Effect
                (lambda (cur-off effect)
                  (nanopass-case (L15b Effect) effect
                    [(fp-offset ,live-info ,imm)
                     (let ([cur-off (fx+ cur-off imm)])
                       (safe-assert (fx>= cur-off 0))
                       cur-off)]
                    [else cur-off])))
              (let ([block-off (block-fp-offset block)])
                (if block-off
                    (unless (fx= cur-off block-off)
                      (sorry! who "conflicting fp-offset value for block ~s" block))
                    (let ([effect* (block-effect* block)])
                      (block-fp-offset-set! block cur-off)
                      (cond
                        [(goto-block? block)
                         (record-fp-offsets! (goto-block-next block) (fold-left Effect cur-off effect*))]
                        [(joto-block? block)
                         (record-fp-offsets! (joto-block-next block) 0)]
                        [(if-block? block)
                         (let ([cur-off (fold-left Effect cur-off effect*)])
                           (record-fp-offsets! (if-block-true block) cur-off)
                           (record-fp-offsets! (if-block-false block) cur-off))]
                        [(tail-block? block) (void)]
                        [(newframe-block? block)
                         (let ([cur-off (fold-left Effect cur-off effect*)])
                           (record-fp-offsets! (newframe-block-next block) cur-off)
                           (for-each (lambda (rp) (record-fp-offsets! rp cur-off)) (newframe-block-rp* block))
                           (record-fp-offsets! (newframe-block-rp block) cur-off))]
                        [else (sorry! who "unrecognized block ~s" block)]))))))
          (for-each (lambda (block) (record-fp-offsets! block 0)) block*)))

      (define-pass finalize-frame-locations! : (L15b Dummy) (ir block*) -> (L15c Dummy) ()
        (definitions
          (define var->loc
            (lambda (x)
              (or (and (uvar? x) (uvar-location x)) x)))
          (define fv->mref
            (lambda (x cur-off)
              (if (fv? x)
                  (with-output-language (L15c Lvalue)
                    `(mref ,%sfp ,%zero ,(fx- (fx* (fv-offset x) (constant ptr-bytes)) cur-off)))
                  x))))
        (Lvalue : Lvalue (ir cur-off) -> Lvalue ()
          [(mref ,x0 ,x1 ,imm)
           `(mref ,(fv->mref (var->loc x0) cur-off) ,(fv->mref (var->loc x1) cur-off) ,imm)]
          [,x (fv->mref (var->loc x) cur-off)])
        ; NB: defining Triv & Rhs with cur-off argument so we actually get to our version of Lvalue
        (Triv : Triv (ir cur-off) -> Triv ())
        (Rhs : Rhs (ir cur-off) -> Rhs ())
        (Pred : Pred (ir cur-off) -> Pred ())
        (Tail : Tail (ir cur-off) -> Tail ())
        (Effect : Effect (ir cur-off) -> Effect ())
        (begin
          (for-each
            (lambda (block)
              (block-effect*-set! block
                (let f ([effect* (block-effect* block)] [cur-off (block-fp-offset block)])
                  (if (null? effect*)
                      (begin
                        (cond
                          [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (void)]
                          [(if-block? block) (if-block-pred-set! block (Pred (if-block-pred block) cur-off))]
                          [(tail-block? block) (tail-block-tail-set! block (Tail (tail-block-tail block) cur-off))]
                          [else (sorry! who "unrecognized block ~s" block)])
                        '())
                      (with-output-language (L15c Effect)
                        (nanopass-case (L15b Effect) (car effect*)
                          [(fp-offset ,live-info ,imm)
                           (cons `(set! ,live-info ,%sfp
                                    ,(if (fx< imm 0)
                                         ; subtract just to make the generated code more clear
                                         `(inline ,null-info ,%- ,%sfp (immediate ,(fx- imm)))
                                         `(inline ,null-info ,%+ ,%sfp (immediate ,imm))))
                             (f (cdr effect*) (fx+ cur-off imm)))]
                          [(set! ,live-info ,x0 ,x1)
                           (let ([x0 (var->loc x0)] [x1 (var->loc x1)])
                             (if (eq? x0 x1)
                                 (f (cdr effect*) cur-off)
                                 (cons `(set! ,live-info ,(fv->mref x0 cur-off) ,(fv->mref x1 cur-off)) (f (cdr effect*) cur-off))))]
                          [else (cons (Effect (car effect*) cur-off) (f (cdr effect*) cur-off))]))))))
            block*)
          `(dummy)))

      (module (select-instructions!)
        (define make-tmp
          (lambda (x)
            (import (only np-languages make-unspillable))
            (let ([tmp (make-unspillable x)])
              (set! unspillable* (cons tmp unspillable*))
              tmp)))
        (define make-restricted-unspillable
          (lambda (x reg*)
            (import (only np-languages make-restricted-unspillable))
            (safe-assert (andmap reg? reg*) (andmap var-index reg*))
            (let ([tmp (make-restricted-unspillable x reg*)])
              (set! unspillable* (cons tmp unspillable*))
              tmp)))
        (define make-precolored-unspillable
          ; instead of using machine registers like eax explicitly, we use an unspillable that
          ; conflicts with everything but the machine register.  this is semantically equivalent
          ; for correct code but causes a spilled unspillable error if we try to use the same
          ; machine register for two conflicting variables
          (lambda (name reg)
            (or (reg-precolored reg)
                (let ([tmp (make-restricted-unspillable name (remq reg (vector->list regvec)))])
                  (safe-assert (memq reg (vector->list regvec)))
                  (reg-precolored-set! reg tmp)
                  tmp))))

        (define-syntax build-set!
          (lambda (x)
            (syntax-case x ()
              [(k lhs rhs)
               (with-implicit (k quasiquote with-output-language)
                 #`(with-output-language (L15d Effect)
                     `(set! ,(make-live-info) lhs rhs)))])))
        (define imm?
          (lambda (x)
            (nanopass-case (L15c Triv) x
              [(immediate ,imm) #t]
              [(literal ,info) (not (info-literal-indirect? info))]
              [(label-ref ,l ,offset) #t]
              [else #f])))
        (define imm0?
          (lambda (x)
            (nanopass-case (L15c Triv) x
              [(immediate ,imm) (eqv? imm 0)]
              [else #f])))
        (define imm32?
          (lambda (x)
            (nanopass-case (L15c Triv) x
              [(immediate ,imm)
               (constant-case ptr-bits
                 [(32) #t]                   ; allows 2^31...2^32-1 per immediate?
                 [(64) (signed-32? imm)])]   ; 2^31...2^32-1 aren't 32-bit values on 64-bit machines
              [(literal ,info)
               (constant-case ptr-bits
                 [(32) (not (info-literal-indirect? info))]
                 [(64) #f])]
              [(label-ref ,l ,offset)
               (constant-case ptr-bits
                 [(32) #t]
                 [(64) #f])]
              [else #f])))
        (define literal@?
          (lambda (x)
            (nanopass-case (L15c Triv) x
              [(literal ,info) (info-literal-indirect? info)]
              [else #f])))
        (define mref?
          (lambda (x)
            (nanopass-case (L15c Triv) x
              [(mref ,lvalue1 ,lvalue2 ,imm) #t]
              [else #f])))
        (define same?
          (lambda (a b)
            (or (eq? a b)
                (nanopass-case (L15c Triv) a
                  [(mref ,lvalue11 ,lvalue12 ,imm1)
                   (nanopass-case (L15c Triv) b
                     [(mref ,lvalue21 ,lvalue22 ,imm2)
                      (and (or (and (eq? lvalue11 lvalue21) (eq? lvalue12 lvalue22))
                               (and (eq? lvalue11 lvalue22) (eq? lvalue12 lvalue21)))
                           (eqv? imm1 imm2))]
                     [else #f])]
                  [else #f]))))

        (define-pass imm->imm : (L15c Triv) (ir) -> (L15d Triv) ()
          (Lvalue : Lvalue (ir) -> Lvalue ()
            [(mref ,lvalue1 ,lvalue2 ,imm) (sorry! who "unexpected mref ~s" ir)])
          (Triv : Triv (ir) -> Triv ()))

        (define-pass literal@->literal : (L15c Triv) (ir) -> (L15d Triv) ()
          (Triv : Triv (ir) -> Triv ()
            [(literal ,info)
             `(literal
                ,(make-info-literal #f (info-literal-type info)
                   (info-literal-addr info) (info-literal-offset info)))]
            [else (sorry! who "unexpected literal ~s" ir)]))

        (define-pass select-instructions! : (L15c Dummy) (ir block* live-size force-overflow?) -> (L15d Dummy) ()
          (definitions
            (module (handle-jump handle-effect-inline handle-pred-inline handle-value-inline)
              (define add-var (make-add-var live-size))
              (define Triv
                (lambda (out t)
                  (nanopass-case (L15d Triv) t
                    [(mref ,x1 ,x2 ,imm) (add-var (add-var out x2) x1)]
                    [,x (add-var out x)]
                    [else out])))
              (define Rhs
                (lambda (out rhs)
                  (nanopass-case (L15d Rhs) rhs
                    [(asm ,info ,proc ,t* ...) (fold-left Triv out t*)]
                    [else (Triv out rhs)])))
              (define Pred
                (lambda (out pred)
                  (nanopass-case (L15d Pred) pred
                    [(asm ,info ,proc ,t* ...) (fold-left Triv out t*)])))
              (define Tail
                (lambda (out tail)
                  (nanopass-case (L15d Tail) tail
                    [(jump ,t) (Triv out t)])))
              (define unwrap
                (lambda (etree effect* out)
                  (safe-assert (not (eq? out 'uninitialized)))
                  (with-values
                    (let f ([etree etree] [effect* effect*] [out out])
                      (if (pair? etree)
                          (let-values ([(effect* out) (f (cdr etree) effect* out)])
                            (f (car etree) effect* out))
                          (if (null? etree)
                              (values effect* out)
                              (values
                                (cons etree effect*)
                                (nanopass-case (L15d Effect) etree
                                  [(set! ,live-info ,x ,rhs)
                                   (live-info-live-set! live-info out)
                                   (Rhs out rhs)]
                                  [(set! ,live-info ,lvalue ,rhs)
                                   (live-info-live-set! live-info out)
                                   (Triv (Rhs out rhs) lvalue)]
                                  [(asm ,info ,proc ,t* ...) (fold-left Triv out t*)]
                                  [else out])))))
                    (lambda (effect* out) effect*))))
              (define-who handle-jump
                (lambda (t live)
                  (let-values ([(etree tail) (md-handle-jump t)])
                    (values (unwrap etree '() (Tail live tail)) tail))))
              (define-who handle-effect-inline
                (lambda (effect-prim info new-effect* t* live)
                  (unwrap (apply (primitive-handler effect-prim) info t*) new-effect* live)))
              (define-who handle-pred-inline
                (lambda (pred-prim info t* live)
                  (let-values ([(etree pred) (apply (primitive-handler pred-prim) info t*)])
                    (values (unwrap etree '() (Pred live pred)) pred))))
              (define-who handle-value-inline
                (lambda (lvalue value-prim info new-effect* t* live)
                  (unwrap (apply (primitive-handler value-prim) info lvalue t*) new-effect* live))))
            (define compute-overage
              (lambda (max-fs@call)
                (if force-overflow?
                    (fxmax
                      (fx- (fx* max-fs@call (constant ptr-bytes)) 0)
                      (fx- (fx* (fx+ max-fv 1) (constant ptr-bytes)) (fx- (constant stack-slop) (fx* (constant stack-frame-limit) 2))))
                    (fxmax
                      (fx- (fx* max-fs@call (constant ptr-bytes)) (constant stack-frame-limit))
                      (fx- (fx* (fx+ max-fv 1) (constant ptr-bytes)) (fx- (constant stack-slop) (constant stack-frame-limit)))))))
            (define overage (compute-overage max-fs@call))
            (define handle-overflow-check
              (lambda (reg info new-effect* live)
                (let-values ([(xnew-effect* pred) (handle-pred-inline %u< null-info
                                                    (list
                                                      reg
                                                      (meta-cond
                                                        [(real-register? '%esp) %esp]
                                                        [else (with-output-language (L15c Triv)
                                                                `(mref ,%tc ,%zero ,(tc-disp %esp)))]))
                                                    live)])
                  (append xnew-effect*
                    (cons (with-output-language (L15d Effect)
                            `(overflow-check ,pred
                               ,(handle-effect-inline %asmlibcall! info '() '() live)
                               ...))
                      new-effect*)))))
            (define maybe-incr-instr-count
              (lambda (block e*)
                (define checks-cc? ; copied from instrument
                  (lambda (block)
                    (and (if-block? block)
                         (null? (block-effect* block))
                         (nanopass-case (L15c Pred) (if-block-pred block)
                           [(inline ,live-info ,info ,pred-prim ,t* ...) (eq? pred-prim %condition-code)]
                           [else #f]))))
                (define count
                  (lambda (n e)
                    ; overflow-check counts as one instruction...close enough, since it rarely fails
                    (nanopass-case (L15d Effect) e
                      [(rp-header ,mrvl ,fs ,lpm) n]
                      [(move-related ,x1 ,x2) n]
                      [else (fx+ n 1)])))
                (if (generate-instruction-counts)
                    (let* ([n (fold-left count (if (goto-block? block) 0 1) e*)]
                           [f (lambda (e*)
                                (handle-effect-inline %inc-cc-counter null-info e*
                                  (list %tc
                                    (with-output-language (L15c Triv) `(immediate ,(constant tc-instr-counter-disp)))
                                    (with-output-language (L15c Triv) `(immediate ,n)))
                                  (block-live-in block)))])
                      (if (and (not (null? e*))
                               (nanopass-case (L15d Effect) (car e*)
                                 [(rp-header ,mrvl ,fs ,lpm) #t]
                                 [else #f]))
                          (cons (car e*) (f (cdr e*)))
                          (begin
                            (assert (not (checks-cc? block)))
                            (f e*))))
                    e*))))
          (Rhs : Rhs (ir lvalue new-effect* live) -> * (new-effect*)
            [(inline ,info ,value-prim ,t* ...)
             (handle-value-inline lvalue value-prim info new-effect* t* live)]
            [else (handle-value-inline lvalue %move null-info new-effect* (list ir) live)])
          (Tail : Tail (ir) -> Tail ()
            [(jump ,live-info ,t) (handle-jump t (live-info-live live-info))]
            [(goto ,l) (values '() `(goto ,l))]
            [(asm-return) (values '() `(asm-return))]
            [(asm-c-return ,info) (values '() `(asm-c-return ,info))])
          (Effect : Effect (ir new-effect*) -> * (new-effect*)
            [(set! ,live-info ,lvalue ,rhs) (Rhs rhs lvalue new-effect* (live-info-live live-info))]
            [(inline ,live-info ,info ,effect-prim ,t* ...)
             (handle-effect-inline effect-prim info new-effect* t* (live-info-live live-info))]
            [(rp-header ,mrvl ,fs ,lpm)
             (cons (with-output-language (L15d Effect) `(rp-header ,mrvl ,fs ,lpm)) new-effect*)]
            [(overflow-check ,live-info)
             (if (fx> 1 overage (fx- (constant stack-frame-limit) (constant stack-slop)))
                 (handle-overflow-check %sfp (intrinsic-info-asmlib dooverflow #f) new-effect* (live-info-live live-info))
                 new-effect*)]
            [(overflood-check ,live-info)
             (if (fx> overage 0)
                 ; dooverflood protocol requires %xp be set where we need esp to be
                 (let ([uxp (make-precolored-unspillable 'uxp %xp)])
                   (handle-value-inline uxp %+ null-info
                     (handle-overflow-check uxp (intrinsic-info-asmlib dooverflood #f) new-effect* (live-info-live live-info))
                     (list %sfp (with-output-language (L15c Triv) `(immediate ,overage)))
                     (live-info-live live-info)))
                 new-effect*)]
            [(fcallable-overflow-check ,live-info)
             ; max-fs@call = 2: the return address and c-chain stored by C-call->XXX
             (if (fx> 1 (compute-overage 2) (fx- (constant stack-frame-limit) (constant stack-slop)))
                 (handle-overflow-check %sfp (intrinsic-info-asmlib dooverflow #f) new-effect* (live-info-live live-info))
                 new-effect*)])
          (Pred : Pred (ir) -> Pred ()
            [(inline ,live-info ,info ,pred-prim ,t* ...)
             (handle-pred-inline pred-prim info t* (live-info-live live-info))])
          (begin
            (for-each
              (lambda (block)
                (block-effect*-set! block
                  (maybe-incr-instr-count block
                    (fold-right Effect
                      (cond
                        [(or (goto-block? block) (joto-block? block) (newframe-block? block)) '()]
                        [(if-block? block)
                         (let-values ([(new-effect* pred) (Pred (if-block-pred block))])
                           (if-block-pred-set! block pred)
                           new-effect*)]
                        [(tail-block? block)
                         (let-values ([(new-effect* tail) (Tail (tail-block-tail block))])
                           (tail-block-tail-set! block tail)
                           new-effect*)]
                        [else (sorry! who "unrecognized block ~s" block)])
                      (block-effect* block)))))
              block*)
            `(dummy)))

        ; NB: try to reuse unspillables to reduce the number we create
        (architecture instructions)
      )

      (define-who do-unspillable-conflict!
        (lambda (kfv kspillable varvec live-size kunspillable unvarvec block*)
          (define remove-var (make-remove-var live-size))
          (define unspillable?
            (lambda (x)
              (and (uvar? x) (uvar-unspillable? x))))
          (define add-unspillable
            (lambda (unspillable* x)
              (if (and (unspillable? x) (not (uvar-seen? x)))
                  (begin
                    (uvar-seen! x #t)
                    (cons x unspillable*))
                  unspillable*)))
          (define add-move!
            (lambda (x1 x2)
              (when (var-index x2)
                ($add-move! x1 x2 2)
                ($add-move! x2 x1 2))))
          (define add-move-hint!
            (lambda (x1 x2)
              (when (var-index x2)
                ($add-move! x1 x2 1)
                ($add-move! x2 x1 1))))
          (define add-static-conflict!
            (lambda (u reg*)
              (let ([u-offset (var-index u)])
                (for-each
                  (lambda (reg) (conflict-bit-set! (var-unspillable-conflict* reg) u-offset))
                  reg*))))
          (define add-us->s-conflicts!
            (lambda (x out) ; x is an unspillable
              (let ([x-offset (var-index x)] [cset (var-spillable-conflict* x)])
                (tree-for-each out live-size 0 live-size
                  (lambda (y-offset)
                    (let* ([y (vector-ref varvec y-offset)] [y-cset (var-unspillable-conflict* y)])
                      (when y-cset
                        ; if y is a spillable, point the unspillable x at y
                        (when (fx< y-offset kspillable) (conflict-bit-set! cset y-offset))
                        ; point y at the unspillable x
                        (conflict-bit-set! y-cset x-offset))))))))
          (define add-us->us-conflicts!
            (lambda (x unspillable*) ; x is a unspillable
              (let ([x-offset (var-index x)] [cset (var-unspillable-conflict* x)])
                (for-each
                  (lambda (y)
                    (let ([y-offset (var-index y)])
                      (conflict-bit-set! cset y-offset)
                      (conflict-bit-set! (var-unspillable-conflict* y) x-offset)))
                  unspillable*))))
          (define add-s->us-conflicts!
            (lambda (x unspillable*) ; x is a spillable or register
              (let ([x-offset (var-index x)] [cset (var-unspillable-conflict* x)])
                (for-each
                  (lambda (y)
                    (let ([y-offset (var-index y)])
                      ; point x at unspillable y
                      (conflict-bit-set! cset y-offset)
                      ; if x is a spillable, point unspillable y at x
                      (when (fx< x-offset kspillable) (conflict-bit-set! (var-spillable-conflict* y) x-offset))))
                  unspillable*))))
          (define Triv
            (lambda (unspillable* t)
              (nanopass-case (L15d Triv) t
                [(mref ,x1 ,x2 ,imm) (add-unspillable (add-unspillable unspillable* x2) x1)]
                [,x (add-unspillable unspillable* x)]
                [else unspillable*])))
          (define Rhs
            (lambda (unspillable* rhs)
              (nanopass-case (L15d Rhs) rhs
                [(asm ,info ,proc ,t* ...) (fold-left Triv unspillable* t*)]
                [else (Triv unspillable* rhs)])))
          (define Pred
            (lambda (p)
              (nanopass-case (L15d Pred) p
                [(asm ,info ,proc ,t* ...) (fold-left Triv '() t*)]
                [else (sorry! who "unexpected pred ~s" p)])))
          (define Tail
            (lambda (tl)
              (nanopass-case (L15d Tail) tl
                [(jump ,t) (Triv '() t)]
                [else '()])))
          (define Effect*
            (lambda (e* unspillable*)
              (if (null? e*)
                  (safe-assert (null? unspillable*))
                  (Effect* (cdr e*)
                    (nanopass-case (L15d Effect) (car e*)
                      [(set! ,live-info ,x ,rhs)
                       (let ([spillable-live (live-info-live live-info)])
                         (if (unspillable? x)
                             (let ([unspillable* (remq x unspillable*)])
                               (safe-assert (uvar-seen? x))
                               (uvar-seen! x #f)
                               (if (and (var? rhs) (var-index rhs))
                                   (begin
                                     (if (unspillable? rhs)
                                         (begin
                                           (add-us->us-conflicts! x (remq rhs unspillable*))
                                           (add-us->s-conflicts! x spillable-live))
                                         (begin
                                           (add-us->us-conflicts! x unspillable*)
                                           (add-us->s-conflicts! x (remove-var spillable-live rhs))))
                                     (add-move! x rhs))
                                   (begin
                                     (add-us->us-conflicts! x unspillable*)
                                     (add-us->s-conflicts! x spillable-live)))
                               (Rhs unspillable* rhs))
                             (begin
                               (when (var-unspillable-conflict* x)
                                 (if (unspillable? rhs)
                                     (begin
                                       (add-s->us-conflicts! x (remq rhs unspillable*))
                                       (add-move! x rhs))
                                     (add-s->us-conflicts! x unspillable*)))
                               (Rhs unspillable* rhs))))]
                      [(set! ,live-info ,lvalue ,rhs) (Triv (Rhs unspillable* rhs) lvalue)]
                      [(asm ,info ,proc ,t* ...) (fold-left Triv unspillable* t*)]
                      [(move-related ,x1 ,x2) (add-move-hint! x1 x2) unspillable*]
                      [(overflow-check ,p ,e* ...) (Effect* (reverse e*) '()) (Pred p)]
                      [else unspillable*])))))
          (for-each (lambda (x) (var-spillable-conflict*-set! x (make-empty-cset kspillable))) unspillable*)
          (let ([f (lambda (x) (var-unspillable-conflict*-set! x (make-empty-cset kunspillable)))])
            (vector-for-each f regvec)
            (for-each f spillable*)
            (vector-for-each f unvarvec))
          (vector-for-each (lambda (x) (add-static-conflict! x (uvar-conflict* x))) unvarvec)
          (for-each
            (lambda (block)
              (Effect* (reverse (block-effect* block))
                (cond
                  [(or (goto-block? block) (joto-block? block) (newframe-block? block)) '()]
                  [(if-block? block) (Pred (if-block-pred block))]
                  [(tail-block? block) (Tail (tail-block-tail block))]
                  [else (sorry! who "unrecognized block ~s" block)])))
            block*)))

      (define-who assign-registers!
        (lambda (lambda-info varvec unvarvec)
          (define k (vector-length regvec))
          (define uvar-weight
            (lambda (x)
              (fx- (uvar-ref-weight x) (uvar-save-weight x))))
          ; could also be calculated when the conflict set is built, which would be more
          ; efficient for low-degree variables
          (define compute-degrees!
            (lambda (x*)
              ; account for uvar -> uvar conflicts
              (for-each
                (lambda (x)
                  (uvar-degree-set! x
                    (fx+
                      ; spills have been trimmed from the var-spillable-conflict* sets
                      (conflict-bit-count (var-spillable-conflict* x))
                      (conflict-bit-count (var-unspillable-conflict* x)))))
                x*)
              ; account for reg -> uvar conflicts
              (vector-for-each
                (lambda (reg)
                  (cset-for-each (var-spillable-conflict* reg)
                    (lambda (x-offset)
                      (let ([x (vector-ref varvec x-offset)])
                        (unless (uvar-location x)
                          (uvar-degree-set! x (fx+ (uvar-degree x) 1))))))
                  (cset-for-each (var-unspillable-conflict* reg)
                    (lambda (x-offset)
                      (let ([x (vector-ref unvarvec x-offset)])
                        (uvar-degree-set! x (fx+ (uvar-degree x) 1))))))
                regvec)))
          (define-who find-home!
            (lambda (x)
              (define conflict?
                (lambda (reg x)
                  (let ([cset (if (uvar-unspillable? x) (var-unspillable-conflict* reg) (var-spillable-conflict* reg))])
                    (conflict-bit-set? cset (var-index x)))))
              (define find-move-related-home
                (lambda (x0 succ fail)
                  (let f ([x x0] [work* '()] [clear-seen! void])
                    (if (uvar-seen? x)
                        (if (null? work*) (begin (clear-seen!) (fail)) (f (car work*) (cdr work*) clear-seen!))
                        (let ([clear-seen! (lambda () (uvar-seen! x #f) (clear-seen!))])
                          (uvar-seen! x #t)
                          (let loop ([move* (uvar-move* x)] [work* work*])
                            (if (null? move*)
                                (if (null? work*) (begin (clear-seen!) (fail)) (f (car work*) (cdr work*) clear-seen!))
                                (let ([var (caar move*)] [move* (cdr move*)])
                                  (define try-reg
                                    (lambda (reg)
                                      (if (conflict? reg x0)
                                          (loop move* work*)
                                          (begin (clear-seen!) (succ reg)))))
                                  (if (reg? var)
                                      (try-reg var)
                                      (if (uvar? var)
                                          (let ([reg (uvar-location var)])
                                            (if (reg? reg)
                                                (try-reg reg)
                                                (loop move* (cons var work*))))
                                          (loop move* work*)))))))))))
              (define set-home!
                (lambda (home)
                  (define update-conflict!
                    (lambda (reg x)
                      (cset-merge! (var-spillable-conflict* reg) (var-spillable-conflict* x))
                      (cset-merge! (var-unspillable-conflict* reg) (var-unspillable-conflict* x))))
                  (uvar-location-set! x home)
                  (update-conflict! home x)))
              (find-move-related-home x
                set-home!
                (lambda ()
                  (let f ([offset (fx- k 1)])
                    (cond
                      [(fx< offset 0)
                       (uvar-spilled! x #t)
                       (when (uvar-unspillable? x)
                         (sorry! who "spilled unspillable ~s" x))]
                      [(conflict? (vector-ref regvec offset) x) (f (fx- offset 1))]
                      [else (set-home! (vector-ref regvec offset))]))))))
          (define pick-victims
            (lambda (x*)
              (define low-degree? (lambda (x) (fx< (uvar-degree x) k)))
              (define pick-potential-spill
                ; x* is already sorted by weight, so this effectively picks uvar with
                ; the highest degree among those with the lowest weight
                (lambda (x*)
                  (let ([x (let f ([x* (cdr x*)] [max-degree (uvar-degree (car x*))] [max-x (car x*)])
                             (if (null? x*)
                                 max-x
                                 (let ([x (car x*)] [x* (cdr x*)])
                                   (if (or (uvar-unspillable? x) (fx> (uvar-weight x) (uvar-weight max-x)))
                                       max-x
                                       (let ([degree (uvar-degree x)])
                                         (if (fx> degree max-degree)
                                             (f x* degree x)
                                             (f x* max-degree max-x)))))))])
                    (values x (remq x x*)))))
              (define remove-victim!
                (lambda (victim)
                  (cset-for-each (var-spillable-conflict* victim)
                    (lambda (offset)
                      (let ([x (vector-ref varvec offset)])
                        (uvar-degree-set! x (fx- (uvar-degree x) 1)))))
                  (cset-for-each (var-unspillable-conflict* victim)
                    (lambda (offset)
                      (let ([x (vector-ref unvarvec offset)])
                        (uvar-degree-set! x (fx- (uvar-degree x) 1)))))))
              (define sort-victims
                ; NB: sorts based on likelihood of successfully assigning move-related vars to the same register
                ; NB: probably should sort based on value of assigning move-related vars to the same register,
                ; NB: i.e., taking into account the ref-weight
                (lambda (victim*)
                  (map car
                    (list-sort
                      (lambda (x y) (fx> (cdr x) (cdr y)))
                      (map (lambda (x)
                             (define relevant?
                               (lambda (x)
                                 (or (reg? x) (and (uvar? x) (not (uvar-spilled? x))))))
                             (do ([move* (uvar-move* x) (cdr move*)]
                                  [w 0 (let ([move (car move*)])
                                         (if (relevant? (car move))
                                             (fx+ w (cdr move))
                                             w))])
                               ((null? move*) (cons x w))))
                        victim*)))))
              (let-values ([(victim* keeper*) (partition low-degree? x*)])
                (if (null? victim*)
                    (let-values ([(victim keeper*) (pick-potential-spill x*)])
                      ; note: victim can be an unspillable if x* contains only precolored unspillables
                      (remove-victim! victim)
                      (values (list victim) keeper*))
                    (begin
                      (unless (null? keeper*)
                        ; tried creating a mask from victim*, logand with bv for each x, count the bits,
                        ; and subtract from x's uvar-degree-set!.  code in chaff.  didn't help at this point.
                        ; perhaps if fxbit-count were implemented better it would
                        (for-each remove-victim! victim*))
                      (values (sort-victims victim*) keeper*))))))
          (let ([x* (append (sort (lambda (x y) (fx< (uvar-weight x) (uvar-weight y))) spillable*) unspillable*)])
            (compute-degrees! x*)
            (let f ([x* x*])
              (unless (null? x*)
                (let-values ([(victim* x*) (pick-victims x*)])
                  (f x*)
                  (for-each find-home! victim*)))))))

      (define everybody-home?
        (lambda ()
          (safe-assert (andmap uvar-location unspillable*))
          (andmap uvar-location spillable*)))

      (define record-inspector-information!
        (lambda (info)
          (define get-closure-fv-names
            (lambda (info ctci)
              (define (get-name fv) (unannotate (uvar-source fv)))
              (or (ctci-closure-fv-names ctci)
                  (case (info-lambda-closure-rep info)
                    [(pair)
                     (let ([p (cons (get-name (car (info-lambda-fv* info)))
                                    (get-name (cadr (info-lambda-fv* info))))])
                       (ctci-closure-fv-names-set! ctci p)
                       p)]
                    [(vector)
                     (let ([v (list->vector (map get-name (info-lambda-fv* info)))])
                       (ctci-closure-fv-names-set! ctci v)
                       v)]
                    [else #f]))))
          (cond
            [(info-lambda-ctci info) =>
             (lambda (ctci)
               (ctci-live-set! ctci
                 (let f ([i 0] [spillable* spillable*])
                   (if (null? spillable*)
                       (make-vector i)
                       (let ([spillable (car spillable*)])
                         (cond
                           [(and (uvar-spilled? spillable) (uvar-source spillable)) =>
                            (lambda (source)
                              (if (eq? source (let () (include "types.ss") cpsymbol))
                                  (case (info-lambda-closure-rep info)
                                    [(singleton)
                                     (cond
                                       [(uvar-source (car (info-lambda-fv* info))) =>
                                        (lambda (source)
                                          (let ([v (f (fx+ i 1) (cdr spillable*))])
                                            (uvar-iii-set! spillable i)
                                            (vector-set! v i (cons (unannotate source) (fv-offset (uvar-location spillable))))
                                            v))]
                                       [else (f i (cdr spillable*))])]
                                    [(pair vector)
                                     (let ([v (f (fx+ i 1) (cdr spillable*))])
                                       (uvar-iii-set! spillable i)
                                       (vector-set! v i
                                         (cons (get-closure-fv-names info ctci)
                                           (fv-offset (uvar-location spillable))))
                                       v)]
                                    [(closure)
                                     (let ([v (f (fx+ i 1) (cdr spillable*))])
                                       (uvar-iii-set! spillable i)
                                       (vector-set! v i (cons (unannotate source) (fv-offset (uvar-location spillable))))
                                       v)]
                                    [else (f i (cdr spillable*))])
                                  (let ([v (f (fx+ i 1) (cdr spillable*))])
                                    (uvar-iii-set! spillable i)
                                    (vector-set! v i (cons (unannotate source) (fv-offset (uvar-location spillable))))
                                    v)))]
                           [else (f i (cdr spillable*))]))))))])))

      (define-pass finalize-register-locations! : (L15d Dummy) (ir block*) -> (L15e Dummy) ()
        (definitions
          (define var->loc
            (lambda (x)
              (if (uvar? x)
                  (or (uvar-location x) (sorry! who "no location assigned to uvar ~s" x))
                  x))))
        (Lvalue : Lvalue (ir) -> Lvalue ()
          [(mref ,x0 ,x1 ,imm) `(mref ,(var->loc x0) ,(var->loc x1) ,imm)]
          [,x (var->loc x)])
        (Pred : Pred (ir) -> Pred ())
        (Tail : Tail (ir) -> Tail ())
        (Effect : Effect (ir) -> Effect ()
          [(set! ,live-info ,[lvalue] ,[rhs]) `(set! ,lvalue ,rhs)])
        (foldable-Effect : Effect (ir new-effect*) -> * (new-effect*)
          [(move-related ,x1 ,x2) new-effect*]
          [(set! ,live-info ,x0 ,x1)
           (let ([x0 (var->loc x0)] [x1 (var->loc x1)])
             (if (eq? x0 x1)
                 new-effect*
                 (cons (Effect ir) new-effect*)))]
          [else (cons (Effect ir) new-effect*)])
        (begin
          (for-each
            (lambda (block)
              (block-effect*-set! block (fold-right foldable-Effect '() (block-effect* block)))
              (cond
                [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (void)]
                [(if-block? block) (if-block-pred-set! block (Pred (if-block-pred block)))]
                [(tail-block? block) (tail-block-tail-set! block (Tail (tail-block-tail block)))]
                [else (sorry! who "unrecognized block ~s" block)]))
            block*)
          `(dummy)))

      (define-pass expose-overflow-check-blocks! : (L15e Dummy) (ir entry-block0* block0*) -> (L16 Dummy) (entry-block* block*)
        (definitions
          (define block* block0*)
          (define entry-block* entry-block0*)
          (define-who redirect-link!
            (lambda (old new)
              (lambda (from)
                (cond
                  [(goto-block? from)
                   (cond
                     [(eq? (goto-block-next from) old) (goto-block-next-set! from new)]
                     [else (sorry! who "goto-block in-link not found")])]
                  [(joto-block? from)
                   (cond
                     [(eq? (joto-block-next from) old) (joto-block-next-set! from new)]
                     [else (sorry! who "joto-block in-link not found")])]
                  [(if-block? from)
                   (cond
                     [(eq? (if-block-true from) old) (if-block-true-set! from new)]
                     [(eq? (if-block-false from) old) (if-block-false-set! from new)]
                     [else (sorry! who "if-block in-link not found")])]
                  [(newframe-block? from)
                   (cond
                     [(eq? (newframe-block-next from) old) (newframe-block-next-set! from new)]
                     [(eq? (newframe-block-rp from) old) (newframe-block-rp-set! from new)]
                     [(memq old (newframe-block-rp* from)) (newframe-block-rp*-set! from (subst new old (newframe-block-rp* from)))]
                     [else (sorry! who "newframe-block in-link not found")])]
                  [else (sorry! who "unexpected block ~s" from)]))))
          (define insert-check!
            (lambda (block rebefore* p ehere* eafter*)
              (let ([libcall-block (make-goto-block)])
                (goto-block-next-set! libcall-block block)
                (block-pariah! libcall-block #t)
                (let ([check-block (make-if-block block libcall-block)])
                  (if-block-pred-set! check-block p)
                  (block-effect*-set! check-block (reverse rebefore*))
                  (block-effect*-set! libcall-block ehere*)
                  (set! entry-block* (subst check-block block entry-block*))
                  (let ([label (block-label block)])
                    (block-label-set! check-block label)
                    (local-label-block-set! label check-block))
                  (let ([label (make-local-label 'post-overflow-check)])
                    (block-label-set! block label)
                    (local-label-block-set! label block))
                  (let ([label (make-local-label 'overflowed)])
                    (block-label-set! libcall-block label)
                    (local-label-block-set! label libcall-block))
                  (for-each (redirect-link! block check-block) (block-in-link* block))
                  (block-in-link*-set! block (list check-block libcall-block))
                  (set! block* (cons* check-block libcall-block block*))
                  (Effect* block '() eafter*)))))
          (define Effect*
            (lambda (block rebefore* eafter*)
              (if (null? eafter*)
                  (block-effect*-set! block (reverse rebefore*))
                  (let ([e (car eafter*)] [eafter* (cdr eafter*)])
                    (nanopass-case (L15e Effect) e
                      [(overflow-check ,[Pred : p] ,[Effect : e*] ...) (insert-check! block rebefore* p e* eafter*)]
                      [else (Effect* block (cons (Effect e) rebefore*) eafter*)]))))))
        (Pred : Pred (ir) -> Pred ())
        (Tail : Tail (ir) -> Tail ())
        (Effect : Effect (ir) -> Effect ())
        ; NB: without the begin, seems to ignore all but the first subform below
        (begin
          (for-each
            (lambda (block)
              (Effect* block '() (block-effect* block))
              (cond
                [(or (goto-block? block) (joto-block? block) (newframe-block? block)) (void)]
                [(if-block? block) (if-block-pred-set! block (Pred (if-block-pred block)))]
                [(tail-block? block) (tail-block-tail-set! block (Tail (tail-block-tail block)))]
                [else (sorry! who "unrecognized block ~s" block)]))
            block0*)
          (values `(dummy) entry-block* block*)))

      (define-syntax with-live-info-record-writer
        (lambda (x)
          (syntax-case x ()
            [(_ live-size varvec e1 e2 ...)
             #'(parameterize ([(case-lambda
                                 [() (record-writer (record-type-descriptor live-info))]
                                 [(x) (record-writer (record-type-descriptor live-info) x)])
                               (lambda (x p wr)
                                 (when (live-info-useless x) (fprintf p "useless "))
                                 (fprintf p "<live:")
                                 (let ([live (live-info-live x)])
                                   (if (eq? live 'uninitialized)
                                       (fprintf p " uninitialized")
                                       (for-each (lambda (x) (fprintf p " ~s" x)) (get-live-vars live live-size varvec))))
                                 (fprintf p ">"))])
                 e1 e2 ...)])))

    (define-pass np-allocate-registers : L15a (ir) -> L16 ()
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info ,max-fv0 (,local* ...) (,entry-block* ...) (,block* ...))
         (let ()
           (define block-printer
             (lambda (unparser name block*)
               (p-dot-graph block* (current-output-port))
               (p-graph block* name (current-output-port) unparser)))
           (module (RApass)
             (define RAprinter
               (lambda (unparser)
                 (lambda (val*)
                   (block-printer unparser (info-lambda-name info) block*))))
             (define-syntax RApass
               (lambda (x)
                 (syntax-case x ()
                   [(_ ?unparser pass-name ?arg ...)
                    #'(xpass pass-name (RAprinter ?unparser) (list ?arg ...))]))))
           (safe-assert (andmap (lambda (x) (eq? (uvar-location x) #f)) local*))
           (let ([kspillable (length local*)] [kfv (fx+ max-fv0 1)] [kreg (vector-length regvec)])
             (fluid-let ([spillable* local*] [unspillable* '()] [max-fv max-fv0] [max-fs@call 0] [poison-cset (make-empty-cset kspillable)])
               (let* ([live-size (fx+ kfv kreg kspillable)] [varvec (make-vector live-size)])
                 ; set up var indices & varvec mapping from indices to vars
                 (fold-left (lambda (i x) (var-index-set! x i) (vector-set! varvec i x) (fx+ i 1)) 0 spillable*)
                 (do ([i 0 (fx+ i 1)]) ((fx= i kfv)) (let ([fv (get-fv i)] [i (fx+ i kspillable)]) (var-index-set! fv i) (vector-set! varvec i fv)))
                 (do ([i 0 (fx+ i 1)]) ((fx= i kreg)) (let ([reg (vector-ref regvec i)] [i (fx+ i kspillable kfv)]) (var-index-set! reg i) (vector-set! varvec i reg)))
                 (with-live-info-record-writer live-size varvec
                   ; run intra/inter-block live analysis
                   (RApass unparse-L15a do-live-analysis! live-size entry-block*)
                   ; this is worth enabling from time to time...
                   #;(check-entry-live! (info-lambda-name info) live-size varvec entry-block*)
                   ; rerun intra-block live analysis and record (fv v reg v spillable) x spillable conflicts
                   (RApass unparse-L15a record-call-live! block* varvec)
                   ;; NB: we could just use (vector-length varvec) to get live-size
                   (when (fx> kspillable 1000) ; NB: parameter?
                     (RApass unparse-L15a identify-poison! kspillable varvec live-size block*))
                   (RApass unparse-L15a do-spillable-conflict! kspillable kfv varvec live-size block*)
                   #;(show-conflicts (info-lambda-name info) varvec '#())
                   ; find frame homes for call-live variables; adds new fv x spillable conflicts
                   (RApass unparse-L15a assign-frame! (filter uvar-spilled? spillable*))
                   #;(show-homes)
                   (RApass unparse-L15a record-inspector-information! info)
                   ; determine frame sizes at nontail-call sites and assign homes to new-frame variables
                   ; adds new fv x spillable conflicts
                   (let ([dummy (RApass unparse-L15b assign-new-frame! (with-output-language (L15a Dummy) `(dummy)) info live-size varvec block*)])
                     ; record fp offset on entry to each block
                     (RApass unparse-L15b record-fp-offsets! entry-block*)
                     ; assign frame homes to poison variables
                     (let ([spill* (filter (lambda (x) (and (not (uvar-location x)) (uvar-poison? x))) spillable*)])
                       (unless (null? spill*)
                         (for-each (lambda (x) (uvar-spilled! x #t)) spill*)
                         (RApass unparse-L15b assign-frame! spill*)))
                     ; on entry to loop, have assigned call-live and new-frame variables to frame homes, determined frame sizes, and computed block-entry fp offsets
                     (let ([saved-reg-csets (vector-map (lambda (reg) (cset-copy (var-spillable-conflict* reg))) regvec)]
                           [bcache* (map cache-block-info block*)])
                       (let loop ()
                         (for-each
                           (lambda (spill)
                             ; remove each spill from each other spillable's spillable conflict set
                             (unless (uvar-poison? spill)
                               (let ([spill-index (var-index spill)])
                                 (cset-for-each (var-spillable-conflict* spill)
                                   (lambda (i)
                                     (let ([x (vector-ref varvec i)])
                                       (unless (uvar-location x)
                                         (conflict-bit-unset! (var-spillable-conflict* x) spill-index)))))))
                             ; release the spill's conflict* set
                             (var-spillable-conflict*-set! spill #f))
                           (filter uvar-location spillable*))
                         (set! spillable* (remp uvar-location spillable*))
                         (let ([saved-move* (map uvar-move* spillable*)])
                           #;(show-homes)
                           (let ([dummy (RApass unparse-L15c finalize-frame-locations! dummy block*)])
                             (let ([dummy (RApass unparse-L15d select-instructions! dummy block* live-size
                                            (let ([libspec (info-lambda-libspec info)])
                                              (and libspec (libspec-does-not-expect-headroom? libspec))))])
                               (vector-for-each (lambda (reg) (reg-precolored-set! reg #f)) regvec)
                               (let* ([kunspillable (length unspillable*)] [unvarvec (make-vector kunspillable)])
                                 ; set up var indices & unvarvec mapping from indices to unspillables
                                 (fold-left (lambda (i x) (var-index-set! x i) (vector-set! unvarvec i x) (fx+ i 1)) 0 unspillable*)
                                 ; rerun intra-block live analysis and record (reg v spillable v unspillable) x unspillable conflicts
                                 (RApass unparse-L15d do-unspillable-conflict! kfv kspillable varvec live-size kunspillable unvarvec block*)
                                 #;(show-conflicts (info-lambda-name info) varvec unvarvec)
                                 (RApass unparse-L15d assign-registers! info varvec unvarvec)
                                 ; release the unspillable conflict sets
                                 (for-each (lambda (x) (var-unspillable-conflict*-set! x #f)) spillable*)
                                 (vector-for-each (lambda (x) (var-unspillable-conflict*-set! x #f)) regvec)
                                 #;(show-homes unspillable*)
                                 (if (everybody-home?)
                                     (let ([dummy (RApass unparse-L15e finalize-register-locations! dummy block*)])
                                       ; release the spillable conflict sets
                                       (vector-for-each (lambda (reg) (var-spillable-conflict*-set! reg #f)) regvec)
                                       (do ([i max-fv (fx- i 1)]) ((fx< i 0)) (var-spillable-conflict*-set! (get-fv i) #f))
                                       (let-values ([(dummy entry-block* block*)
                                                     (xpass expose-overflow-check-blocks!
                                                       (lambda (val*)
                                                         (apply (lambda (dummy entry-block* block*)
                                                                  (block-printer unparse-L16 (info-lambda-name info) block*))
                                                           val*))
                                                       (list dummy entry-block* block*))])
                                         (safe-assert (andmap block-label (append entry-block* block*)))
                                         (safe-assert (lambda (b) (eq? (local-label-block (block-label b)) b)) (append entry-block* block*))
                                         `(lambda ,info (,entry-block* ...) (,block* ...))))
                                     (begin
                                       (for-each restore-block-info! block* bcache*)
                                       (vector-for-each var-spillable-conflict*-set! regvec saved-reg-csets)
                                       (for-each (lambda (x) (uvar-location-set! x #f)) spillable*)
                                       (for-each uvar-move*-set! spillable* saved-move*)
                                       (set! unspillable* '())
                                       (RApass unparse-L15b assign-frame! (filter uvar-spilled? spillable*))
                                       (loop)))))))))))))))])))

    ; NB: commonize with earlier
    (define-pass np-remove-repeater-blocks-again! : L16 (ir) -> L16 ()
      (definitions
        (define path-compress!
          (lambda (b)
            (cond
              [(block-repeater? b) (goto-block-next b)]
              ; NB: ignoring block-src* here, post-profiling
              [(and (goto-block? b) (null? (block-effect* b)))
               (block-repeater! b #t)
               (let ([end (path-compress! (goto-block-next b))])
                 (goto-block-next-set! b end)
                 end)]
              [else b])))
        (define resolve
          (lambda (b)
            (if (block-repeater? b)
                (goto-block-next b)
                b))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info (,entry-block* ...) (,block* ...))
         (for-each path-compress! block*)
         (for-each
           (lambda (from)
             (define resolve!
               (lambda (get put!)
                 (let ([to (get from)])
                   (when (block-repeater? to)
                     (put! from (goto-block-next to))))))
             (cond
               [(goto-block? from)
                (unless (block-repeater? from)
                  (resolve! goto-block-next goto-block-next-set!))]
               [(joto-block? from)
                (resolve! joto-block-next joto-block-next-set!)]
               [(if-block? from)
                (resolve! if-block-true if-block-true-set!)
                (resolve! if-block-false if-block-false-set!)]
               [(newframe-block? from)
                (resolve! newframe-block-next newframe-block-next-set!)
                (newframe-block-rp*-set! from (map resolve (newframe-block-rp* from)))
                (resolve! newframe-block-rp newframe-block-rp-set!)]
               [(tail-block? from) (void)]
               [else (sorry! who "unrecognized block ~s" from)]))
           block*)
         (for-each (lambda (dcl)
                     (let* ([b0 (local-label-block dcl)] [b (and b0 (resolve b0))])
                       (unless (eq? b b0)
                         (local-label-block-set! dcl b)
                         (block-label-set! b dcl))))
           (info-lambda-dcl* info))
         `(lambda ,info
            (,(map resolve entry-block*) ...)
            (,(filter (lambda (b) (or (not (block-repeater? b)) (eq? (goto-block-next b) b))) block*) ...))]))

    ; NB: might instead sort blocks in np-generate-code, which is in a better position
    ; NB: to deal with block ordering when branch displacement sizes are limited
    (define-pass np-optimize-block-order! : L16 (ir) -> L16 ()
      (definitions
        (define invertible?
          (lambda (pred)
            (nanopass-case (L16 Pred) pred
              [(asm ,info ,proc ,t* ...)
               (safe-assert (info-condition-code? info))
               (info-condition-code-invertible? info)])))
        (define block-likeliness
          (lambda (b)
            (or (block-weight b) 0)))
        (define block-in-degree
          (lambda (b)
            (fold-left (lambda (n b) (if (block-seen? b) n (fx+ n 1))) 0 (block-in-link* b)))))
      (CaseLambdaExpr : CaseLambdaExpr (ir) -> CaseLambdaExpr ()
        [(lambda ,info (,entry-block* ...) (,block* ...))
          (safe-assert (not (ormap block-seen? block*)))
          (safe-assert (not (null? entry-block*)))
          (let loop ([b (car entry-block*)] [w* '()] [pariah* (cdr entry-block*)] [rblock* '()])
            (define next-worklist-entry
              (lambda (w* pariah* rblock*)
                (if (null? w*)
                    (if (null? pariah*)
                        (begin
                          (safe-assert (andmap block-label (append entry-block* rblock*)))
                          (safe-assert (lambda (b) (eq? (local-label-block (block-label b)) b)) (append entry-block* rblock*))
                          (for-each (lambda (b) (block-seen! b #f)) block*)
                          `(lambda ,info (,entry-block* ...) (,(reverse rblock*) ...)))
                        (loop (car pariah*) '() (cdr pariah*) rblock*))
                    (loop (car w*) (cdr w*) pariah* rblock*))))
            (if (block-seen? b)
                (next-worklist-entry w* pariah* rblock*)
                (let ([rblock* (cons b rblock*)])
                  (block-seen! b #t)
                  (cond
                    [(goto-block? b) (loop (goto-block-next b) w* pariah* rblock*)]
                    [(joto-block? b) (loop (joto-block-next b) w* pariah* rblock*)]
                    [(if-block? b)
                      (let ([true (if-block-true b)] [false (if-block-false b)])
                        (if (block-seen? true)
                            (loop false w* pariah* rblock*)
                            (if (block-seen? false)
                                (loop true w* pariah* rblock*)
                                (if (invertible? (if-block-pred b))
                                    (let ([llntrue (block-likeliness true)] [llnfalse (block-likeliness false)])
                                      (if (or (and (fx= llnfalse llntrue)
                                                   (fx< (block-in-degree false) (block-in-degree true)))
                                              (fx< llntrue llnfalse))
                                          (if (fx< llntrue 0)
                                              (loop false w* (cons true pariah*) rblock*)
                                              (loop false (cons true w*) pariah* rblock*))
                                          (if (fx< llnfalse 0)
                                              (loop true w* (cons false pariah*) rblock*)
                                              (loop true (cons false w*) pariah* rblock*))))
                                    (if (fx< (block-likeliness false) 0)
                                        (loop true w* (cons false pariah*) rblock*)
                                        (loop true (cons false w*) pariah* rblock*))))))]
                    [(newframe-block? b)
                      (loop (newframe-block-next b)
                        (append (newframe-block-rp* b) (cons (newframe-block-rp b) w*))
                        pariah* rblock*)]
                    [(tail-block? b) (next-worklist-entry w* pariah* rblock*)]
                    [else (sorry! who "unrecognized block ~s" b)]))))]))

    (define (np-after-calling-conventions ir)
      (compose ir
        (pass np-expand-hand-coded unparse-L13.5)
        (pass np-expose-allocation-pointer unparse-L14)
        (pass np-expose-basic-blocks unparse-L15a)
        (pass np-remove-repeater-blocks! unparse-L15a)
        (lambda (ir)
          (if (and (or (eq? ($compile-profile) 'block) ($profile-block-data?)) ($sfd))
              ((pass np-add-block-source! unparse-L15a) ir)
              ir))
        (pass np-propagate-pariahty! unparse-L15a)
        (lambda (ir)
          (if (or (eq? ($compile-profile) 'source)
                  (and (eq? ($compile-profile) 'block) ($sfd)))
              ((pass np-insert-profiling unparse-L15a) ir)
              ir))
        (pass np-add-in-links! unparse-L15a)
        (pass np-compute-loop-depth! unparse-L15a)
        (pass np-weight-references! unparse-L15a)
        np-allocate-registers ; aggregate pass...don't use pass macro, or it will show up in timings
        (pass np-remove-repeater-blocks-again! unparse-L16)
        (pass np-optimize-block-order! unparse-L16)
        (pass np-generate-code)))

    (set! $np-compile
      (lambda (original-input-expression pt?)
        (with-initialized-registers
          (fluid-let ([frame-vars (make-vector 8 #f)]
                      [next-lambda-seqno 0]
                      [pass-time? pass-time?])
            (compose original-input-expression
              (pass cpnanopass unparse-L1)
              (pass np-recognize-let unparse-L2)
              (pass np-discover-names unparse-L3)
              #;(lambda (ir) (unless (eqv? (optimize-level) 3) ((pass np-check-flags) ir)) ir)
              (pass np-convert-assignments unparse-L4)
              (pass np-sanitize-bindings unparse-L4)
              (pass np-suppress-procedure-checks unparse-L4)
              (pass np-recognize-mrvs unparse-L4.5)
              (pass np-expand-foreign unparse-L4.75)
              (pass np-recognize-loops unparse-L4.875)
              (pass np-name-anonymous-lambda unparse-L5)
              (pass np-convert-closures unparse-L6)
              (pass np-optimize-direct-call unparse-L6)
              (pass np-identify-scc unparse-L6)
              (if ($optimize-closures)
                  (pass np-expand/optimize-closures unparse-L7)
                  (pass np-expand-closures unparse-L7))
              (lambda (ir)
                (if (fxzero? ($loop-unroll-limit))
                    ir
                    ((pass np-profile-unroll-loops unparse-L7) ir)))
              (pass np-simplify-if unparse-L7)
              (pass np-expand-primitives unparse-L9)
              (pass np-place-overflow-and-trap unparse-L9.5)
              (pass np-rebind-on-ruined-path unparse-L9.5)
              (pass np-finalize-loops unparse-L9.75)
              (pass np-optimize-pred-in-value unparse-L9.75)
              (pass np-remove-complex-opera* unparse-L10)
              (pass np-push-mrvs unparse-L10.5)
              (pass np-normalize-context unparse-L11)
              (pass np-insert-trap-check unparse-L11.5)
              (pass np-flatten-case-lambda unparse-L12)
              (pass np-impose-calling-conventions unparse-L13)
              np-after-calling-conventions)))))

    (set! $np-boot-code
      (lambda (which)
        (with-initialized-registers
          ($c-func-code-record
            (fluid-let ([frame-vars (make-vector 8 #f)]
                        [next-lambda-seqno 0]
                        [pass-time? #t])
              (parameterize ([generate-inspector-information #f] [$compile-profile #f])
                (np-after-calling-conventions
                  (with-output-language (L13 Program)
                    (let ([l (make-local-label 'Linvoke)])
                      `(labels ([,l (hand-coded ,which)]) ,l))))))))))
  )

  (set! $np-tracer tracer)

  (set! $np-last-pass last-pass)

  (set! $track-dynamic-closure-counts track-dynamic-closure-counts)

  (set! $track-static-closure-counts track-static-closure-counts)

  (set! $optimize-closures (make-parameter #t (lambda (x) (and x #t))))
)
