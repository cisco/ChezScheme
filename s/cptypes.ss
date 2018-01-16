"cptypes.ss"
;;; cptypes.ss
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
 - (cptypes ir ctxt types) -> (values ir ret types t-types f-types)
   + arguments
     ir: expression to be optimized
     ctxt: 'effect 'test 'value
     types: an immutable dictionary (currently an intmap).
            The dictionary connects the counter of a prelex with the types
            discovered previously.
            (fxmap ([prelex-counter x] . 'pair)
                   ([prelex-counter y] . 'vector)
                   ([prelex-counter z] . `(quote 0)))
   + results
     ir: the optimized expression
     ret: type of the result of the expression
     types: like the types in the argument, with addition of the type discover
            during the optimization of the expression
     t-types: types to be used in case the expression is not #f, to be used in
              the "then" branch of an if.
              If left as #f it will be automatically replaced with a copy of
              types by the wrapper.
              This is usually only filled in a text context.
     f-types: idem for the "else" branch. (if x (something) (here x is #f))


 - predicate: They may be:
              * a symbol to indicate the type, like 'vector 'pair 'number
                (there are a few fake values, in particular 'bottom is used to
                 signal that there is an error)
              * a nanopass-quoted value that is okay-to-copy?, like
                `(quote 0) `(quote 5) `(quote #t) `(quote '())
                (this doesn't includes `(quote <record-type-descriptor>))
              * a [normal] list ($record/rtd <rtd>) to signal that it's a
                record of type <rtd>
              * a [normal] list ($record/ref <ref>) to signal that it's a
                record of a type that is stored in the variable <ref>
                (these may collide with other records)
              * TODO?: add something to indicate that x is a procedure to
                       create/setter/getter/predicate of a record of that type

 - Primitives are marked as procedures, without distinction.
 - Most of the time I'm using eq? and eqv? as if they were equivalent.
   I assume that the differences are hidden by unspecified behavior.

|#


[define $cptypes
[let ()
  (import (nanopass))
  (include "base-lang.ss")
  (include "fxmap.ss")

  (with-output-language (Lsrc Expr)
    (define void-rec `(quote ,(void)))
    (define true-rec `(quote #t))
    (define false-rec `(quote #f))
    (define null-rec `(quote ()))
    (define empty-vector-rec `(quote #()))
    (define empty-string-rec `(quote ""))
    (define empty-bytevector-rec `(quote #vu8()))
    (define empty-fxvector-rec `(quote #vfx()))
    (define eof-rec `(quote #!eof))
    (define bwp-rec `(quote #!bwp))

    (define (simple? e) ; Simplified version copied from cp0. TODO: copy the rest.
      (nanopass-case (Lsrc Expr) e
        [(quote ,d) #t]
        [(ref ,maybe-src ,x) #t]
        [(case-lambda ,preinfo ,cl* ...) #t]
        [,pr #t]
        [(moi) #t]
        [(record-type ,rtd ,e) (simple? e)]
        [else #f]
        #;[else ($oops who "unrecognized record ~s" e)]))

    ; TODO: Remove discardable operations in e1. (vector (f) (g)) => (begin (f) (g))
    (define make-seq
      ; ensures that the right subtree of the output seq is not a seq if the
      ; second argument is similarly constrained, to facilitate result-exp
      (lambda (ctxt e1 e2)
        (if (simple? e1)
            e2
            (if (and (eq? ctxt 'effect) (simple? e2))
                e1
                (let ([e1 (nanopass-case (Lsrc Expr) e1
                            [(seq ,e11 ,e12)
                             (guard (simple? e12))
                             e11]
                            [else e1])])
                  (nanopass-case (Lsrc Expr) e2
                    [(seq ,e21 ,e22) `(seq (seq ,e1 ,e21) ,e22)]
                    [else `(seq ,e1 ,e2)]))))))

    #;(define make-seq* ; requires at least one operand
        (lambda (ctxt e*)
          (if (null? (cdr e*))
              (car e*)
              (make-seq ctxt (car e*) (make-seq* ctxt (cdr e*))))))
  )

  (module (pred-env-empty
           pred-env-add pred-env-remove/base pred-env-lookup
           pred-env-intersect/base pred-env-union/super-base
           pred-env-rebase
           pred-intersect pred-union)
    (import fxmap)

    (define pred-env-empty empty-fxmap)

    (define (pred-env-add/key types key pred)
      (cond
        [(and pred
              (not (eq? pred 'ptr))) ; filter 'ptr to reduce the size
         (let ([old (fxmap-ref types key #f)])
           (cond
             [(not old)
              (fxmap-set types key pred)]
             [else (let ([new (pred-intersect old pred)])
                     (if (eq? old new)
                         types
                        (fxmap-set types key new)))]))]
        [else
         types]))

    (define (pred-env-add types x pred)
      (cond
        [(and x (not (prelex-was-assigned x)))
         (pred-env-add/key types (prelex-counter x) pred)]
        [else types]))

    (define (pred-env-remove/base types x base)
      (fxmap-remove/base types (prelex-counter x) base))

    (define (pred-env-lookup types x)
      (and (not (prelex-was-assigned x))
           (fxmap-ref types (prelex-counter x) #f)))

    ; This is conceptually the intersection of the types in `types` and `from`
    ; but since 'ptr is not stored to save space and time, the implementation
    ; looks like an union of the fxmaps.
    ; [missing 'ptr] _and_ 'vector -> 'vector
    ; 'box _and_ 'vector -> 'bottom
    ; 'number _and_ 'exact-integer -> 'exact-integer
    (define (pred-env-intersect/base types from base)
      #;(display (list (fxmap-changes from) (fxmap-changes types)))
      (cond
        [(fx> (fxmap-changes from) (fxmap-changes types))
         (pred-env-intersect/base from types base)]
        [else
        (let ([ret types])
          (fxmap-for-each/diff (lambda (key x y)
                                (let ([z (fxmap-ref types key #f)])
                                  ;x-> from
                                  ;y-> base
                                  ;z-> types
                                  (set! ret (pred-env-add/key ret key (pred-intersect x z)))))
                               (lambda (key x)
                                (set! ret (pred-env-add/key ret key x)))
                               (lambda (key x) (error 'pred-env-intersect/base "") (void))
                               from
                               base)
           ret)]))

    (define (pred-intersect x y)
      (cond
        [(predicate-implies? x y) x]
        [(predicate-implies? y x) y]
        [(or (predicate-implies-not? x y)
             (predicate-implies-not? y x))
         'bottom]
        [(or (and (eq? x 'boolean) (eq? y 'true))
             (and (eq? y 'boolean) (eq? x 'true)))
         true-rec]
        [else (or x y)])) ; if there is no exact option, at least keep the old value

    ; This is conceptually the union of the types in `types` and `from`
    ; but since 'ptr is not stored to save space and time, the implementation
    ; looks like an intersection of the fxmaps.
    ; [missing 'ptr] _or_ 'vector -> [missing 'ptr]
    ; 'box _or_ 'boolean -> [missing 'ptr]
    ; 'number _or_ 'exact-integer -> 'number
    (define (pred-env-union/from from base types new-base)
      ; Calculate the union of types and from, and intersect it with new-base
      ; Iterate over the difference of from and base.
      (let ([ret new-base])
        (fxmap-for-each/diff (lambda (key x y)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> from
                                 ;y-> base
                                 ;z-> types
                                 (set! ret (pred-env-add/key ret key (pred-union x z)))))
                             (lambda (key x)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> from
                                 ;z-> types
                                 (set! ret (pred-env-add/key ret key (pred-union x z)))))
                             (lambda (key x) (error 'pred-env-union/base "") (void))
                             from
                             base)
          ret))

    (define (pred-env-union/super-base types types/b
                                       from from/b
                                       base
                                       new-base)
      ; Calculate the union of types and from, and intersect it with new-base
      ; Use the intermediate bases to minimize the amount of operations
      ; required. In particular, base should be the base of types/b and from/b.
      (let ([size-types (fx- (fxmap-changes types) (fxmap-changes base))]
            [size-from (fx- (fxmap-changes from) (fxmap-changes base))]
            [size-new (fx+ (fx- (fxmap-changes types) (fxmap-changes types/b))
                           (fx- (fxmap-changes from) (fxmap-changes from/b)))])
        (cond
          [(and (fx<= size-types size-from) (fx<= size-types size-new))
           (pred-env-union/from types base from new-base)]
          [(fx<= size-from size-new)
           (pred-env-union/from from base types new-base)]
          [else
           (let ([temp (pred-env-union/from from from/b types new-base)])
             (pred-env-union/from types types/b from temp))])))

    (define (pred-union x y)
      (cond
        [(predicate-implies? y x) x]
        [(predicate-implies? x y) y]
        [(find (lambda (t)
                 (and (predicate-implies? x t)
                      (predicate-implies? y t)))
               '(char null-or-pair $record
                 gensym symbol
                 fixnum exact-integer flonum real number
                 boolean true ptr))] ; ensure they are order from more restrictive to less restrictive
        [else #f]))

    (define (pred-env-rebase types base new-base)
      (let ([ret types])
        (fxmap-for-each/diff (lambda (key x y)
                              (let ([z (fxmap-ref types key #f)])
                                ;x-> new-base
                                ;y-> base
                                ;z-> types
                                (if (eq? x z)
                                    (set! ret (fxmap-reset/base ret key new-base))
                                    (set! ret (fxmap-advance/base ret key new-base)))))
                             (lambda (key x)
                              (let ([z (fxmap-ref types key #f)])
                                ;x-> new-base
                                ;z-> types
                                (if (eq? x z)
                                    (set! ret (fxmap-reset/base ret key new-base))
                                    (set! ret (fxmap-advance/base ret key new-base)))))
                             (lambda (key x) (error 'pred-env-rebase "") (void))
                             new-base
                             base)
        ret))
 )

  (define (pred-env-add/ref types r pred)
    (nanopass-case (Lsrc Expr) r
      [(ref ,maybe-src ,x)
       (pred-env-add types x pred)]
      [else types]))

  ;copied from cp0.ss
  (define (arity-okay? arity n)
    (or (not arity) ; presumably system routine w/no recorded arity
        (ormap
          (lambda (a)
            (or (fx= n a)
                (and (fx< a 0) (fx>= n (fx- -1 a)))))
          arity)))

  ;copied from cp0.ss
  (define okay-to-copy?
    (lambda (obj)
      ; okay to copy obj if (eq? (faslin (faslout x)) x) => #t or (in the case of numbers and characters)
      ; the value of (eq? x x) is unspecified
      (or (symbol? obj)
          (number? obj)
          (char? obj)
          (boolean? obj)
          (null? obj)
          (eqv? obj "")
          (eqv? obj '#())
          (eqv? obj '#vu8())
          (eqv? obj '#vfx())
          (eq? obj (void))
          (eof-object? obj)
          (bwp-object? obj)
          (eq? obj '#6=#6#)
          ($unbound-object? obj)
          (record-type-descriptor? obj)))) ;removed in datum->predicate

  (define (datum->predicate d ir)
    (cond
      [(#3%$record? d) '$record] ;check first to avoid double representation of rtd
      [(okay-to-copy? d) ir]
      [(and (integer? d) (exact? d)) 'exact-integer]
      [(pair? d) 'pair]
      [(box? d) 'box]
      [(vector? d) 'vector]
      [(string? d) 'string]
      [(bytevector? d) 'bytevector]
      [(fxvector? d) 'fxvector]
      [else #f]))

  (define (rtd->record-predicate rtd)
    (cond
      [(Lsrc? rtd)
       (nanopass-case (Lsrc Expr) rtd
         [(quote ,d)
          (guard (record-type-descriptor? d))
          (list '$record/rtd d)]
         [(ref ,maybe-src ,x)
          (guard (not (prelex-was-assigned x)))
          (list '$record/ref x)]
         [(record-type ,rtd ,e)
          (rtd->record-predicate e)]
         [else '$record])]
      [else '$record]))

  ; when extend is #f the result is a predicate that recognizes less values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #t and (something x) ==> (#3%something x)
  ; when extend is #t the result is a predicate that recognizes more values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #f and (something x) ==> <error>
  ; in case the non extended version is not #f, the extended version must be not #f
  (define (primref-name->predicate name extend?)
    (case name
      [pair? 'pair]
      [box? 'box]
      [$record? '$record]
      [fixnum? 'fixnum]
      [flonum? 'flonum]
      [real? 'real]
      [number? 'number]
      [vector? 'vector]
      [string? 'string]
      [bytevector? 'bytevector]
      [fxvector? 'fxvector]
      [gensym? 'gensym]
      [symbol? 'symbol]
      [char? 'char]
      [boolean? 'boolean]
      [procedure? 'procedure]
      [not false-rec]
      [null? null-rec]
      [eof-object? eof-rec]
      [bwp-object? bwp-rec]
      [list? (if (not extend?) null-rec 'null-or-pair)]
      [else ((if extend? cdr car);---------------------------------------------------
             (case name
               [(record? record-type-descriptor?) '(bottom . $record)]
               [(integer? rational?) '(exact-integer . real)]
               [(cflonum?) '(flonum . number)]
               [else '(#f . #f)]))])) ; this is used only to detect predicates.

  ; nqm: no question mark
  ; this is almost duplicated code, but with more cases
  ; it's also useful to avoid the allocation
  ; of the temporal strings to transform: vector -> vector?
  (define (primref-name/nqm->predicate name extend?)
    (case name
      [pair 'pair]
      [box 'box]
      [$record '$record]
      [fixnum 'fixnum]
      [flonum 'flonum]
      [real 'real]
      [number 'number]
      [vector 'vector]
      [string 'string]
      [bytevector 'bytevector]
      [fxvector 'fxvector]
      [gensym 'gensym]
      [symbol 'symbol]
      [char 'char]
      [bottom 'bottom] ;pseudo-predicate
      [ptr 'ptr] ;pseudo-predicate
      [boolean 'boolean]
      [procedure 'procedure]
      [exact-integer 'exact-integer] ;fake-predicate
      [void void-rec] ;fake-predicate
      [null null-rec]
      [eof-object eof-rec]
      [bwp-object bwp-rec]
      [list (if (not extend?) null-rec 'null-or-pair)] ;fake-predicate
      [else ((if extend? cdr car);---------------------------------------------------
             (case name
               [(record rtd) '(bottom . $record)]
               [(bit length ufixnum pfixnum) '(bottom . fixnum)]
               [(uint sub-uint) '(bottom . exact-integer)]
               [(sint) '(fixnum . exact-integer)]
               [(uinteger) '(bottom . real)]
               [(integer rational) '(exact-integer . real)]
               [(cflonum) '(flonum . number)]
               [else '(bottom . ptr)]))])) ; this is used only to analyze the signatures.

  (define (primref->predicate pr extend?)
    (primref-name->predicate (primref-name pr) extend?))

  (define (check-constant-is? x pred?)
    (nanopass-case (Lsrc Expr) x
      [(quote ,d) (pred? d)]
      [else #f]))

  ; strange properties of bottom here:
  ; (implies? x bottom): only for x=bottom
  ; (implies? bottom y): always
  ; (implies-not? x bottom): never
  ; (implies-not? bottom y): never
  ; check (implies? x bottom) before (implies? x something)
  (define (predicate-implies? x y)
    (and x
         y
         (or (eq? x y)
             (and (Lsrc? x)
                  (Lsrc? y)
                  (nanopass-case (Lsrc Expr) x
                    [(quote ,d1)
                     (nanopass-case (Lsrc Expr) y
                       [(quote ,d2) (eqv? d1 d2)] #;CHECK ;eq?/eqv?/equal?
                       [else #f])]
                    [else #f]))
             (and (pair? x) (pair? (cdr x)) (eq? (car x) '$record/rtd)
                  (pair? y) (pair? (cdr y)) (eq? (car y) '$record/rtd)
                  (cond
                    [(record-type-sealed? (cadr y))
                     (eqv? (cadr x) (cadr y))]
                    [else
                     (let loop ([x (cadr x)] [y (cadr y)])
                       (or (eqv? x y)
                           (let ([xp (record-type-parent x)])
                             (and xp (loop xp y)))))]))
             (and (pair? x) (pair? (cdr x)) (eq? (car x) '$record/ref)
                  (pair? y) (pair? (cdr y)) (eq? (car y) '$record/ref)
                  (eq? (cadr x) (cadr y)))
             (eq? x 'bottom)
             (case y
               [(null-or-pair) (or (check-constant-is? x null?)
                                   (eq? x 'pair))]
               [(fixnum) (check-constant-is? x target-fixnum?)]
               [(exact-integer)
                (or (eq? x 'fixnum)
                    (check-constant-is? x (lambda (x) (and (integer? x)
                                                           (exact? x)))))]
               [(flonum) (check-constant-is? x flonum?)]
               [(real) (or (eq? x 'fixnum)
                           (eq? x 'exact-integer)
                           (eq? x 'flonum)
                           (check-constant-is? x real?))]
               [(number) (or (eq? x 'fixnum)
                             (eq? x 'exact-integer)
                             (eq? x 'flonum)
                             (eq? x 'real)
                             (check-constant-is? x number?))]
               [(gensym) (check-constant-is? x gensym?)]
               [(symbol) (or (eq? x 'gensym)
                             (check-constant-is? x symbol?))]
               [(char) (check-constant-is? x char?)]
               [(boolean) (or (check-constant-is? x not)
                              (check-constant-is? x (lambda (x) (eq? x #t))))]
               [(true) (and (not (check-constant-is? x not))
                            (not (eq? x 'boolean))
                            (not (eq? x 'ptr)))] ; only false-rec, boolean and ptr may be `#f
               [($record) (or (check-constant-is? x #3%$record?)
                              (and (pair? x) (eq? (car x) '$record/rtd))
                              (and (pair? x) (eq? (car x) '$record/ref)))]
               [(vector) (check-constant-is? x vector?)] ; i.e. '#()
               [(string) (check-constant-is? x string?)] ; i.e. ""
               [(bytevector) (check-constant-is? x bytevector?)] ; i.e. '#vu8()
               [(fxvector) (check-constant-is? x fxvector?)] ; i.e. '#vfx()
               [(ptr) #t]
               [else #f]))))

  (define (predicate-implies-not? x y)
    (and x
         y
         ; a $record/ref may be any other kind or record
         (not (and (pair? x)
                   (eq? (car x) '$record/ref)
                   (predicate-implies? y '$record)))
         (not (and (pair? y)
                   (eq? (car y) '$record/ref)
                   (predicate-implies? x '$record)))
         ; boolean and true may be #f
         (not (and (eq? x 'boolean)
                   (eq? y 'true)))
         (not (and (eq? y 'boolean)
                   (eq? x 'true)))
         ; the other types are included or disjoint
         (not (predicate-implies? x y))
         (not (predicate-implies? y x))))

  (define (signature->result-predicate signature)
    (let ([results (cdr signature)])
      (and (fx= (length results) 1)
           (let ([result (car results)])
             (cond
               [(symbol? result)
                (primref-name/nqm->predicate result #t)]
               [(equal? result '(ptr . ptr))
                'pair]
               [(pair? result)
                'pair]
               [else
                'ptr])))))

  (define primref->result-predicate/cache (make-hashtable equal-hash equal?))

  (define (primref->result-predicate pr)
    (let ([key (primref-name pr)])
      (if (hashtable-contains? primref->result-predicate/cache key)
          (hashtable-ref primref->result-predicate/cache key #f)
          (let ([new (primref->result-predicate/no-cache pr)])
            (hashtable-set! primref->result-predicate/cache key new)
            new))))

  (define (primref->result-predicate/no-cache pr)
    (let ([pred/flags
           (let ([flags (primref-flags pr)])
             (cond
               [(all-set? (prim-mask abort-op) flags)
                'bottom]
               [(all-set? (prim-mask true) flags)
                'true]
               [(all-set? (prim-mask boolean-valued) flags)
                'boolean]
               [else
                #f]))]
          [pred/signatures
           (let ([signatures (primref-signatures pr)])
             (and (not (null? signatures))
                  (let ([results (map (lambda (s) (signature->result-predicate s)) signatures)])
                    (fold-left pred-union 'bottom results))))])
      (pred-intersect pred/flags pred/signatures)))

  (define (signature->argument-predicate signature pos extend?)
    (let* ([arguments (car signature)]
           [dots (memq '... arguments)])
      (cond
        [(and dots (null? (cdr dots)))
         (cond
           [(< pos (- (length arguments) 2))
            (primref-name/nqm->predicate (list-ref arguments pos) extend?)]
           [else
            (primref-name/nqm->predicate (list-ref arguments (- (length arguments) 2)) extend?)])]
         [dots #f] ; TODO: Extend to handle this case, perhaps knowing the argument count.
         [else
          (cond
            [(< pos (length arguments))
             (let ([argument (list-ref arguments pos)])
               (cond
                 [(equal? argument '(ptr . ptr))
                  'pair]
                 [(and extend? (pair? argument))
                  'pair]
                 [else
                  (primref-name/nqm->predicate argument extend?)]))]
            [else
             'bottom])])))

  (define primref->argument-predicate/cache (make-hashtable equal-hash equal?))

  (define (primref->argument-predicate pr pos extend?)
    (let ([key (list (primref-name pr) pos extend?)])
      (if (hashtable-contains? primref->argument-predicate/cache key)
          (hashtable-ref primref->argument-predicate/cache key #f)
          (let ([new (primref->argument-predicate/no-cache pr pos extend?)])
            (when (<= pos 10)
              (hashtable-set! primref->argument-predicate/cache key new))
            new))))

  (define (primref->argument-predicate/no-cache pr pos extend?)
    (let ([signatures (primref-signatures pr)])
      (and (>= (length signatures) 1)
           (let ([vals (map (lambda (signature)
                              (signature->argument-predicate signature pos extend?))
                            signatures)])
             (fold-left (if extend? pred-union pred-intersect) (car vals) (cdr vals))))))

  (define (primref->unsafe-primref pr)
    (lookup-primref 3 (primref-name pr)))

  [define-pass cptypes/raw : Lsrc (ir ctxt types) -> Lsrc (ret types t-types f-types)
    [Expr : Expr (ir ctxt types) -> Expr (ret types t-types f-types)
      [(quote ,d)
       (values ir (datum->predicate d ir) #f #f #f)]
      [(ref ,maybe-src ,x)
       (case ctxt
         [(test)
          (let ([t (pred-env-lookup types x)])
            (cond
              [(predicate-implies-not? t false-rec)
               (values true-rec true-rec #f #f #f)]
              [(predicate-implies? t false-rec)
               (values false-rec false-rec #f #f #f)]
              [else
               (values ir t
                      types
                      (pred-env-add/ref types ir 'true) ; don't confuse it with true-rec
                      (pred-env-add/ref types ir false-rec))]))]
         [else
           (let ([t (pred-env-lookup types x)])
            (cond
              [(Lsrc? t)
               (nanopass-case (Lsrc Expr) t
                 [(quote ,d)
                  (values t t #f #f #f)]
                 [else
                  (values ir t #f #f #f)])]
               [else
                (values ir t #f #f #f)]))])]
      [(seq ,e1 ,e2)
       (let-values ([(e1 ret1 types t-types f-types)
                     (cptypes e1 'effect types)])
         (cond
           [(predicate-implies? ret1 'bottom)
            (values e1 ret1 types #f #f)]
           [else
            (let-values ([(e2 ret types t-types f-types)
                          (cptypes e2 ctxt types)])
              (values (make-seq ctxt e1 e2) ret types t-types f-types))]))]
      [(if ,e1 ,e2 ,e3)
       (let-values ([(e1 ret1 types1 t-types1 f-types1)
                     (cptypes e1 'test types)])
         (cond
           [(predicate-implies? ret1 'bottom) ;check bottom first
            (values e1 ret1 types #f #f)]
           [(predicate-implies-not? ret1 false-rec)
            (let-values ([(e2 ret types t-types f-types)
                          (cptypes e2 ctxt types)])
              (values (make-seq ctxt e1 e2) ret types t-types f-types))]
           [(predicate-implies? ret1 false-rec)
            (let-values ([(e3 ret types t-types f-types)
                          (cptypes e3 ctxt types)])
              (values (make-seq ctxt e1 e3) ret types t-types f-types))]
           [else
            (let-values ([(e2 ret2 types2 t-types2 f-types2)
                           (cptypes e2 ctxt t-types1)]
                          [(e3 ret3 types3 t-types3 f-types3)
                           (cptypes e3 ctxt f-types1)])
              (let ([ir `(if ,e1 ,e2 ,e3)])
                (cond
                  [(predicate-implies? ret2 'bottom) ;check bottom first
                   (values ir ret3 types3 t-types3 f-types3)]
                  [(predicate-implies? ret3 'bottom) ;check bottom first
                   (values ir ret2 types2 t-types2 f-types2)]
                  [else
                   (let ([new-types (pred-env-union/super-base types2 t-types1
                                                               types3 f-types1
                                                               types1
                                                               types1)])
                     (values ir
                             (cond
                               [(and (eq? ctxt 'test)
                                     (predicate-implies-not? ret2 false-rec)
                                     (predicate-implies-not? ret3 false-rec))
                                true-rec]
                               [else
                                (pred-union ret2 ret3)])
                             new-types
                             (cond
                               [(not (eq? ctxt 'test))
                                #f] ; don't calculate t-types outside a test context
                               [(predicate-implies? ret2 false-rec)
                                (pred-env-rebase t-types3 types1 new-types)]
                               [(predicate-implies? ret3 false-rec)
                                (pred-env-rebase t-types2 types1 new-types)]
                               [(and (eq? types2 t-types2)
                                     (eq? types3 t-types3))
                                #f] ; don't calculate t-types when it will be equal to new-types
                               [else
                                (pred-env-union/super-base t-types2 t-types1
                                                           t-types3 f-types1
                                                           types1
                                                           new-types)])
                             (cond
                               [(not (eq? ctxt 'test))
                                #f] ; don't calculate f-types outside a test context
                               [(predicate-implies-not? ret2 false-rec)
                                (pred-env-rebase f-types3 types1 new-types)]
                               [(predicate-implies-not? ret3 false-rec)
                                (pred-env-rebase f-types2 types1 new-types)]
                               [(and (eq? types2 f-types2)
                                     (eq? types3 f-types3))
                                #f] ; don't calculate t-types when it will be equal to new-types
                               [else
                                (pred-env-union/super-base f-types2 t-types1
                                                           f-types3 f-types1
                                                           types1
                                                           new-types)])))])))]))]
      [(set! ,maybe-src ,x ,e)
       (let-values ([(e ret types t-types f-types)
                     (cptypes e 'value types)])
         (values `(set! ,maybe-src ,x ,e)
                 void-rec types #f #f))]
      [(call ,preinfo ,pr ,e* ...)
       (let* ([e/r/t* (map (lambda (e)
                             (let-values ([(e r t t-t f-t)
                                           (cptypes e 'value types)])
                               (list e r t)))
                           e*)]
              [e* (map car e/r/t*)]
              [r* (map cadr e/r/t*)]
              [t* (map caddr e/r/t*)]
              [t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)]
              [ret (primref->result-predicate pr)]
              [ir `(call ,preinfo ,pr ,e* ...)])
         (let-values ([(ret t)
                       (let loop ([e* e*] [r* r*] [n 0] [ret ret] [t t])
                         (if (null? e*)
                             (values ret t)
                             (let ([pred (primref->argument-predicate pr n #t)])
                               (loop (cdr e*)
                                     (cdr r*)
                                     (fx+ n 1)
                                     (if (predicate-implies-not? (car r*) pred)
                                         'bottom
                                         ret)
                                     (pred-env-add/ref t (car e*) pred)))))])
           (cond
             [(predicate-implies? ret 'bottom)
              (values ir ret t #f #f)]
             [(not (arity-okay? (primref-arity pr) (length e*)))
              (values ir 'bottom t #f #f)]
             [(and (fx= (length e*) 2)
                   (or (eq? (primref-name pr) 'eq?)
                       (eq? (primref-name pr) 'eqv?)))
                (let ([r1 (car r*)]
                      [r2 (cadr r*)]
                      [e1 (car e*)]
                      [e2 (cadr e*)])
                  (cond
                    [(or (predicate-implies-not? r1 r2)
                         (predicate-implies-not? r2 r1))
                     (values (make-seq ctxt (make-seq 'effect e1 e2) false-rec)
                             false-rec t #f #f)]
                    [else
                     (values ir ret types
                             (and (eq? ctxt 'test)
                                  (pred-env-add/ref
                                   (pred-env-add/ref t e1 r2)
                                   e2 r1))
                             #f)]))]
             [(and (fx= (length e*) 1)
                   (primref->predicate pr #t))
              (let ([var (car r*)]
                    [pred (primref->predicate pr #f)])
                (cond
                    [(predicate-implies? var pred)
                     (values (make-seq ctxt (car e*) true-rec)
                             true-rec t #f #f)]
                    [else
                     (let ([pred (primref->predicate pr #t)])
                       (cond
                         [(predicate-implies-not? var pred)
                          (values (make-seq ctxt (car e*) false-rec)
                                  false-rec t #f #f)]
                         [else
                          (values ir ret types
                                  (and (eq? ctxt 'test)
                                       (pred-env-add/ref t (car e*) pred))
                                  #f)]))]))]
             [(and (fx>= (length e*) 1)
                   (eq? (primref-name pr) '$record))
              (values ir (rtd->record-predicate (car e*)) t #f #f)]
             [(and (fx= (length e*) 2)
                   (or (eq? (primref-name pr) 'record?)
                       (eq? (primref-name pr) '$sealed-record?)))
              (let ([pred (rtd->record-predicate (cadr e*))]
                    [var (car r*)])
                (cond
                  [(predicate-implies-not? var pred)
                   (cond
                     [(or (all-set? (prim-mask unsafe) (primref-flags pr))
                          (nanopass-case (Lsrc Expr) (cadr e*) ; ensure that it is actually a rtd
                            [(quote ,d)
                             (record-type-descriptor? d)]
                            [(record-type ,rtd ,e) #t]
                            [else #f]))
                      (values (make-seq ctxt (make-seq 'effect (car e*) (cadr e*)) false-rec)
                              false-rec t #f #f)]
                     [else
                      (values (make-seq ctxt ir false-rec)
                              false-rec t #f #f)])]
                  [(and (not (eq? pred '$record)) ; assume that the only extension is '$record
                        (predicate-implies? var pred))
                   (values (make-seq ctxt (make-seq 'effect (car e*) (cadr e*)) true-rec)
                           true-rec t #f #f)]
                  [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                        (nanopass-case (Lsrc Expr) (cadr e*) ; check that it is a rtd
                          [(quote ,d)
                           (record-type-descriptor? d)]
                          [(record-type ,rtd ,e) #t]
                          [else #f]))
                   (let ([pr (primref->unsafe-primref pr)])
                     (values `(call ,preinfo ,pr ,e* ...)
                             ret types
                             (and (eq? ctxt 'test)
                                  (pred-env-add/ref types (car e*) pred))
                             #f))]
                  [else
                   (values ir ret types
                           (and (eq? ctxt 'test)
                                (pred-env-add/ref types (car e*) pred))
                           #f)]))]
             ; TODO: special case for call-with-values.
             [(and (fx= (length e*) 1)
                   (eq? (primref-name pr) 'exact?))
              (cond
                [(predicate-implies? (car r*) 'exact-integer)
                 (values (make-seq ctxt (car e*) true-rec)
                         true-rec t #f #f)]
                [(predicate-implies? (car r*) 'flonum)
                 (values (make-seq ctxt (car e*) false-rec)
                         false-rec t #f #f)]
                [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                      (predicate-implies? (car r*) 'number))
                 (let ([pr (primref->unsafe-primref pr)])
                   (values `(call ,preinfo ,pr ,e* ...)
                           ret t #f #f))]
                [else
                 (values ir ret t #f #f)])]
             [(and (fx= (length e*) 1)
                   (eq? (primref-name pr) 'inexact?))
              (cond
                [(predicate-implies? (car r*) 'exact-integer)
                 (values (make-seq ctxt (car e*) false-rec)
                         false-rec t #f #f)]
                [(predicate-implies? (car r*) 'flonum)
                 (values (make-seq ctxt (car e*) true-rec)
                         true-rec t #f #f)]
                [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                      (predicate-implies? (car r*) 'number))
                 (let ([pr (primref->unsafe-primref pr)])
                   (values `(call ,preinfo ,pr ,e* ...)
                           ret t #f #f))]
                [else
                 (values ir ret t #f #f)])]
             [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                   (all-set? (prim-mask safeongoodargs) (primref-flags pr))
                   (andmap (lambda (r n)
                             (predicate-implies? r
                                                 (primref->argument-predicate pr n #f)))
                           r* (enumerate r*)))
               (let ([pr (primref->unsafe-primref pr)])
                 (values `(call ,preinfo ,pr ,e* ...)
                         ret types #f #f))]
             [else
              (values ir ret t #f #f)])))]
      [(case-lambda ,preinfo ,cl* ...)
       (let ([cl* (map (lambda (cl)
                        (nanopass-case (Lsrc CaseLambdaClause) cl
                          [(clause (,x* ...) ,interface ,body)
                           (let-values ([(body ret types t-types f-types)
                                         (cptypes body 'value types)])
                             (with-output-language (Lsrc CaseLambdaClause)
                               `(clause (,x* ...) ,interface ,body)))]))
                       cl*)])
         (values `(case-lambda ,preinfo ,cl* ...) 'procedure #f #f #f))]
      [(call ,preinfo ,e0 ,e* ...)
       (let* ([e/r/t* (map (lambda (e)
                             (let-values ([(e r t t-t f-t)
                                           (cptypes e 'value types)])
                               (list e r t)))
                           e*)]
              [e* (map car e/r/t*)]
              [r* (map cadr e/r/t*)]
              [t* (map caddr e/r/t*)]
              [t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)])
         (nanopass-case (Lsrc Expr) e0
           [(case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body))
            ; We are sure that body will run and that it will be run after the evaluation of the arguments,
            ; so we can use the types discovered in the arguments and also use the ret and types from the body.
            (guard (fx= interface (length e*)))
            (let ([t (fold-left pred-env-add t x* r*)])
              (let-values ([(body ret n-types t-types f-types)
                            (cptypes body ctxt t)])
                (let* ([e0 `(case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body))]
                       [new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                       [t-types (and (eq? ctxt 'test)
                                     (not (eq? n-types t-types))
                                     (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                       [f-types (and (eq? ctxt 'test)
                                     (not (eq? n-types f-types))
                                     (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
                  (values `(call ,preinfo ,e0 ,e* ...)
                          ret new-types t-types f-types))))]
           [(case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body))
            ; We are sure that body will run and that it will be run after the evaluation of the arguments,
            ; but this will raise an error. TODO: change body to (void) because it will never run.
            (guard (not (fx= interface (length e*))))
            (let-values ([(body ret types t-types f-types)
                          (cptypes body 'value t)])
              (let ([e0 `(case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body))])
                (values `(call ,preinfo ,e0 ,e* ...)
                        'bottom #f #f #f)))]
           [(case-lambda ,preinfo2 ,cl* ...)
            ; We are sure that it will run after the arguments are evaluated,
            ; so we can effectively delay the evaluation of the lambda and use more types inside it.
            ; TODO: (difficult) Try to use the ret vales and discovered types.
            (let-values ([(e0 ret types t-types f-types)
                          (cptypes e0 'value t)])
              (values `(call ,preinfo ,e0 ,e* ...)
                      #f t #f #f))]
           [else
            ; It's difficult to be sure the order the code will run,
            ; so assume that the expression may be evaluated before the arguments.
            (let-values ([(e0 ret0 types0 t-types0 f-types0)
                          (cptypes e0 'value types)])
               (let* ([t (pred-env-intersect/base t types0 types)]
                      [t (pred-env-add/ref t e0 'procedure)])
                 (values `(call ,preinfo ,e0 ,e* ...)
                         #f t #f #f)))]))]
      [(letrec ((,x* ,e*) ...) ,body)
       (let* ([e/r/t* (map (lambda (e)
                             (let-values ([(e ret types t-types f-types)
                                           (cptypes e 'value types)])
                              (list e ret types)))
                            e*)]
              [e* (map car e/r/t*)]
              [r* (map cadr e/r/t*)]
              [t* (map caddr e/r/t*)]
              [t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)]
              [t (fold-left pred-env-add t x* r*)])
        (let-values ([(body ret n-types t-types f-types)
                     (cptypes body ctxt t)])
          (let* ([new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                 [t-types (and (eq? ctxt 'test)
                               (not (eq? n-types t-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                 [f-types (and (eq? ctxt 'test)
                               (not (eq? n-types f-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
            (values `(letrec ([,x* ,e*] ...) ,body)
                    ret new-types t-types f-types))))]
      [(letrec* ((,x* ,e*) ...) ,body)
       (let*-values ([(e* types)
                      (let loop ([x* x*] [e* e*] [types types] [rev-e* '()]) ; this is similar to an ordered-map
                        (if (null? x*)
                          (values (reverse rev-e*) types)
                          (let-values ([(e ret types t-types f-types)
                                        (cptypes (car e*) 'value types)])
                            (let ([types (pred-env-add types (car x*) ret)])
                              (loop (cdr x*) (cdr e*) types (cons e rev-e*))))))])
        (let-values ([(body ret n-types t-types f-types)
                      (cptypes body ctxt types)])
          (let* ([new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                 [t-types (and (eq? ctxt 'test)
                               (not (eq? n-types t-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                 [f-types (and (eq? ctxt 'test)
                               (not (eq? n-types f-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
            (values `(letrec* ([,x* ,e*] ...) ,body)
                    ret new-types t-types f-types))))]
      [,pr
       (values ir
               (and (all-set? (prim-mask proc) (primref-flags pr)) 'procedure)
               #f #f #f)]
      [(foreign ,conv ,name ,e (,arg-type* ...) ,result-type)
       (let-values ([(e ret types t-types f-types)
                     (cptypes e 'value types)])
         (values `(foreign ,conv ,name ,e (,arg-type* ...) ,result-type)
                 #f types #f #f))]
      [(fcallable ,conv ,e (,arg-type* ...) ,result-type)
       (let-values ([(e ret types t-types f-types)
                     (cptypes e 'value types)])
         (values `(fcallable ,conv ,e (,arg-type* ...) ,result-type)
                 #f types #f #f))]
      [(record ,rtd ,rtd-expr ,e* ...)
       (let-values ([(rtd-expr ret-re types-re t-types-re f-types-re)
                     (cptypes rtd-expr 'value types)])
         (let* ([e/r/t* (map (lambda (e)
                               (let-values ([(e ret types t-types f-types)
                                             (cptypes e 'value types)])
                                (list e ret types)))
                              e*)]
                [e* (map car e/r/t*)]
                #;[r* (map cadr e/r/t*)]
                [t* (map caddr e/r/t*)])
         (values `(record ,rtd ,rtd-expr ,e* ...)
                 (rtd->record-predicate rtd-expr)
                 (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)
                 #f #f)))]
      [(record-ref ,rtd ,type ,index ,e)
       (let-values ([(e ret types t-types f-types)
                     (cptypes e 'value types)])
         (values `(record-ref ,rtd ,type ,index ,e)
                 #f
                 (pred-env-add/ref types e '$record)
                 #f #f))]
      [(record-set! ,rtd ,type ,index ,e1 , e2) ;can they be reordered?
       (let-values ([(e1 ret1 types1 t-types1 f-types1)
                     (cptypes e1 'value types)]
                    [(e2 ret2 types2 t-types2 f-types2)
                     (cptypes e2 'value types)])
         (values `(record-set! ,rtd ,type ,index ,e1 ,e2)
                 void-rec
                 (pred-env-add/ref (pred-env-intersect/base types1 types2 types)
                                   e1 '$record)
                 #f #f))]
      [(record-type ,rtd ,[cptypes : e 'value types -> e ret types t-types f-types])
       (values `(record-type ,rtd ,e)
               #f types #f #f)]
      [(record-cd ,rcd ,rtd-expr ,[cptypes : e 'value types -> e ret types t-types f-types])
       (values `(record-cd ,rcd ,rtd-expr ,e)
               #f types #f #f)]
      [(immutable-list (,e* ...) ,e)
       (let ([e* (map (lambda (e)
                        (let-values ([(e ret types t-types f-types)
                                      (cptypes e 'value types)])
                          e))
                      e*)])
         (let-values ([(e ret types t-types f-types)
                       (cptypes e 'value types)])
           (values `(immutable-list (,e*  ...) ,e)
                    ret types #f #f)))] #;CHECK
      [(moi) (values ir #f #f #f #f)]
      [(pariah) (values ir void-rec #f #f #f)]
      [(cte-optimization-loc ,box ,e)
       (let-values ([(e ret types t-types f-types)
                     (cptypes e 'value types)])
         (values `(cte-optimization-loc ,box ,e)
                 ret types #f #f))] #;CHECK
      [(cpvalid-defer ,e) (sorry! who "cpvalid leaked a cpvalid-defer form ~s" ir)]
      [(profile ,src) (values ir #f #f #f #f)]
      #;[else (values ir #f #f #f #f)]
      [else ($oops who "unrecognized record ~s" ir)]]
    (Expr ir ctxt types)]

  (define (cptypes ir ctxt types)
    (let-values ([(ir ret r-types t-types f-types)
                  (cptypes/raw ir ctxt types)])
      (values ir
              ret
              (or r-types types)
              (or t-types r-types types)
              (or f-types r-types types))))
  (lambda (ir)
    (let-values ([(ir ret types t-types f-types)
                  (cptypes ir 'value pred-env-empty)])
      ir))
]]
