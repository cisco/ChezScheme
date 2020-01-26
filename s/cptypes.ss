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
     types: like the types in the argument, with the addition of the types
            discover during the optimization of the expression
     t-types: types to be used in case the expression is not #f, to be used in
              the "then" branch of an if.
              This is usually only filled in a text context.
              It may be #f, and in this case the `if` clause will use the value
              of types as a replacement.
              (Also the clauses for `let[rec/*]` handle the #f case specialy.)
     f-types: idem for the "else" branch. (if x (something) <here x is #f>)


 - predicate: They may be:
              * a symbol to indicate the type, like 'vector 'pair 'number
                (there are a few fake values, in particular 'bottom is used to
                 signal that there is an error)
              * a nanopass-quoted value that is okay-to-copy?, like
                `(quote 0) `(quote 5) `(quote #t) `(quote '())
                (this doesn't includes `(quote <record-type-descriptor>))
              * a record #[pred-$record/rtd <rtd>] to signal that it's a
                record of type <rtd>
              * a record #[pred-$record/ref <ref>] to signal that it's a
                record of a type that is stored in the variable <ref>
                (these may collide with other records)
              * TODO?: add something to indicate that x is a procedure to
                       create/setter/getter/predicate of a record of that type

 - Primitives are marked as procedures, without distinction.
 - Most of the time I'm using eq? and eqv? as if they were equivalent.
   I assume that the differences are hidden by unspecified behavior.

|#

  
(define $cptypes)
(let ()
  (import (nanopass))
  (include "base-lang.ss")
  (include "fxmap.ss")

  (define-pass cptypes : Lsrc (ir) -> Lsrc ()
    (definitions
  (define prelex-counter
    (let ()
      (define count 0)
      (lambda (x)
        (or (prelex-operand x)
            (let ([c count])
              (set! count (fx+ count 1))
              (prelex-operand-set! x c)
              c)))))

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

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-0})
    (sealed #t))

  (module (pred-env-empty pred-env-bottom
           pred-env-add pred-env-remove/base pred-env-lookup
           pred-env-intersect/base pred-env-union/super-base
           pred-env-rebase
           pred-intersect pred-union)
    (import fxmap)
    
    ; a fake fxmap that is full of 'bottom
    (define-record-type $bottom
      (nongenerative #{$bottom koj7zosgebxioicmj4lgopip0-0})
      (sealed #t))

    (define bottom-fxmap (make-$bottom))

    (define pred-env-empty empty-fxmap)

    (define pred-env-bottom bottom-fxmap)

    (define (pred-env-add/key types key pred)
      (cond
        [(and pred
              (not (eq? pred 'ptr)) ; filter 'ptr to reduce the size
              (not (eq? types bottom-fxmap)))
         (let ([old (fxmap-ref types key #f)])
           (cond
             [(not old)
              (fxmap-set types key pred)]
             [else (let ([new (pred-intersect old pred)])
                     (cond
                       [(eq? new old) types]
                       [(eq? new 'bottom) bottom-fxmap]
                       [else (fxmap-set types key new)]))]))]
        [else
         types]))

    (define (pred-env-add types x pred)
      (cond
        [(and x (not (prelex-assigned x)))
         (pred-env-add/key types (prelex-counter x) pred)]
        [else types]))

    ; When types is bottom-fxmap, the "association" is not removed
    (define (pred-env-remove/base types x base)
      (cond
        [(eq? types bottom-fxmap)
         bottom-fxmap]
        [else
         (fxmap-remove/base types (prelex-counter x) base)]))

    (define (pred-env-lookup types x)
      (cond
        [(eq? types bottom-fxmap)
         'bottom]
        [else
         (and (not (prelex-assigned x))
              (fxmap-ref types (prelex-counter x) #f))]))

    ; This is conceptually the intersection of the types in `types` and `from`
    ; but since 'ptr is not stored to save space and time, the implementation
    ; looks like an union of the fxmaps.
    ; [missing 'ptr] _and_ 'vector -> 'vector
    ; 'box _and_ 'vector -> 'bottom
    ; 'number _and_ 'exact-integer -> 'exact-integer
    (define (pred-env-intersect/base types from base)
      (cond
        [(or (eq? types bottom-fxmap)
             (eq? from bottom-fxmap))
         bottom-fxmap]
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
                               (lambda (key x)
                                 ($impoops 'pred-env-intersect/base "unexpected value ~s in base environment ~s" x base))
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
    ; *Internals auxilary function. Does not check bottom-fxmap.*
    (define ($pred-env-union/from from base types new-base)
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
                             (lambda (key x)
                               ($impoops 'pred-env-union/from "unexpected value ~s in base environment ~s" x base))
                             from
                             base)
          ret))

    (define (pred-env-union/super-base types types/b
                                       from from/b
                                       base
                                       new-base)
      ; Calculate the union of types and from, and intersect it with new-base
      ; Use the intermediate bases types/b and from/b to minimize the amount
      ; of operations required.
      ; In particular, base should be the base of types/b, from/b and new-base.
      (cond
        [(eq? new-base bottom-fxmap)
         bottom-fxmap]
        [(eq? types bottom-fxmap)
         (pred-env-rebase from base new-base)]
        [(eq? from bottom-fxmap)
         (pred-env-rebase types base new-base)]
        [else
         (let ([size-types (fx- (fxmap-changes types) (fxmap-changes base))]
               [size-from (fx- (fxmap-changes from) (fxmap-changes base))]
               [size-new (fx+ (fx- (fxmap-changes types) (fxmap-changes types/b))
                              (fx- (fxmap-changes from) (fxmap-changes from/b)))])
           (cond
             [(and (fx<= size-types size-from) (fx<= size-types size-new))
              ($pred-env-union/from types base from new-base)]
             [(fx<= size-from size-new)
              ($pred-env-union/from from base types new-base)]
             [else
              (let ([temp ($pred-env-union/from from from/b types new-base)])
                ; temp is never bottom-fxmap here
                ($pred-env-union/from types types/b from temp))]))]))

    (define (pred-union x y)
      (cond
        [(predicate-implies? y x) x]
        [(predicate-implies? x y) y]
        [(find (lambda (t)
                 (and (predicate-implies? x t)
                      (predicate-implies? y t)))
               '(char null-or-pair $record
                 gensym uninterned-symbol interned-symbol symbol
                 fixnum exact-integer flonum real number
                 boolean true ptr))] ; ensure they are order from more restrictive to less restrictive
        [else #f]))

    (define (pred-env-rebase types base new-base)
      (cond
        [(or (eq? types bottom-fxmap)
             (eq? new-base bottom-fxmap))
         bottom-fxmap]
        [else
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
                                (lambda (key x)
                                  ($impoops 'pred-env-rebase "unexpected value ~s in base environment ~s" x base))
                                new-base
                                base)
           ret)]))
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
      (or (and (symbol? obj) (not (uninterned-symbol? obj)))
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

  (define (rtd->record-predicate rtd extend?)
    (cond
      [(Lsrc? rtd)
       (nanopass-case (Lsrc Expr) rtd
         [(quote ,d)
          (guard (record-type-descriptor? d))
          (make-pred-$record/rtd d)]
         [(ref ,maybe-src ,x)
          (guard (not (prelex-assigned x)))
          (make-pred-$record/ref x)]
         [(record-type ,rtd ,e)
          (rtd->record-predicate e extend?)]
         [else (if (not extend?) 'bottom '$record)])]
      [else (if (not extend?) 'bottom '$record)]))

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
      [uninterned-symbol? 'uninterned-symbol]
      #;[interned-symbol? 'interned-symbol]
      [symbol? 'symbol]
      [char? 'char]
      [boolean? 'boolean]
      [procedure? 'procedure]
      [not false-rec]
      [null? null-rec]
      [eof-object? eof-rec]
      [bwp-object? bwp-rec]
      [list? (if (not extend?) null-rec 'null-or-pair)]
      [else ((if extend? cdr car)
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
      [uninterned-symbol 'uninterned-symbol]
      [interned-symbol 'interned-symbol]
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
      [else ((if extend? cdr car)
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
             (eq? x 'bottom)
             (cond
               [(Lsrc? y)
                (and (Lsrc? x)
                     (nanopass-case (Lsrc Expr) y
                       [(quote ,d1)
                        (nanopass-case (Lsrc Expr) x
                          [(quote ,d2) (eqv? d1 d2)]
                          [else #f])]
                       [else #f]))]
               [(pred-$record/rtd? y)
                (and (pred-$record/rtd? x)
                     (let ([x-rtd (pred-$record/rtd-rtd x)]
                           [y-rtd (pred-$record/rtd-rtd y)])
                       (cond
                         [(record-type-sealed? y-rtd)
                          (eqv? x-rtd y-rtd)]
                         [else
                          (let loop ([x-rtd x-rtd])
                            (or (eqv? x-rtd y-rtd)
                                (let ([xp-rtd (record-type-parent x-rtd)])
                                  (and xp-rtd (loop xp-rtd)))))])))]
               [(pred-$record/ref? y)
                (and (pred-$record/ref? x)
                     (eq? (pred-$record/ref-ref x)
                          (pred-$record/ref-ref y)))]
               [(case y
                  [(null-or-pair) (or (eq? x 'pair)
                                      (check-constant-is? x null?))]
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
                  [(uninterned-symbol) (check-constant-is? x uninterned-symbol?)]
                  [(interned-symbol) (check-constant-is? x (lambda (x)
                                                             (and (symbol? x)
                                                                  (not (gensym? x))
                                                                  (not (uninterned-symbol? x)))))]
                  [(symbol) (or (eq? x 'gensym)
                                (eq? x 'uninterned-symbol)
                                (eq? x 'interned-symbol)
                                (check-constant-is? x symbol?))]
                  [(char) (check-constant-is? x char?)]
                  [(boolean) (check-constant-is? x boolean?)]
                  [(true) (and (not (check-constant-is? x not))
                               (not (eq? x 'boolean))
                               (not (eq? x 'ptr)))] ; only false-rec, boolean and ptr may be `#f
                  [($record) (or (pred-$record/rtd? x)
                                 (pred-$record/ref? x)
                                 (check-constant-is? x #3%$record?))]
                  [(vector) (check-constant-is? x vector?)] ; i.e. '#()
                  [(string) (check-constant-is? x string?)] ; i.e. ""
                  [(bytevector) (check-constant-is? x bytevector?)] ; i.e. '#vu8()
                  [(fxvector) (check-constant-is? x fxvector?)] ; i.e. '#vfx()
                  [(ptr) #t]
                  [else #f])]
               [else #f]))))

  (define (predicate-implies-not? x y)
    (and x
         y
         ; a pred-$record/ref may be any other kind or record
         (not (and (pred-$record/ref? x)
                   (predicate-implies? y '$record)))
         (not (and (pred-$record/ref? y)
                   (predicate-implies? x '$record)))
         ; boolean and true may be a #t
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


  (module ()
    (with-output-language (Lsrc Expr)

      (define get-type-key)

      (define (expr-is-rtd? x types)
        (nanopass-case (Lsrc Expr) x ; ensure that it is actually a rtd
          [(quote ,d)
           (record-type-descriptor? d)]
          [(record-type ,rtd ,e) #t]
          ; TODO: extend the type system to include rtd
          [else #f]))

      ; Similar to the define-inline in other passes, but the result can't be #f.
      ; The arguments have already been analyzed, and the type of the result
      ; is available with the macro (get-type <arg>).
      ; A good default is (values `(call ,preinfo ,pr ,<args> ...) ret ntypes #f #f)
      ; In particular, ntypes has all the types discovered in the arguments and
      ; the types implied by the signatures. For the types before the arguments
      ; were analyzed, use oldtypes. (See exact? for an example.)
      ; Also, prim-name and level repeat the information available in pr.
      (define-syntax define-specialize
        (lambda (x)
          (define (make-get-type-name id)
            (datum->syntax-object id
              (gensym (string-append (symbol->string (syntax->datum id))
                                     "-ret-type"))))
          (syntax-case x ()
            [(_key lev prim clause ...)
             (identifier? #'prim)
             #'(_key lev (prim) clause ...)]
            [(_key lev (prim ...) clause ...)
             (andmap identifier? #'(prim ...))
             (with-implicit (_key level prim-name preinfo pr ret ctxt ntypes oldtypes)
               (with-syntax
                 ([key (case (datum lev)
                         [(2) #'cptypes2]
                         [(3) #'cptypes3]
                         [else ($oops #f "invalid inline level ~s" (datum lev))])]
                  [body
                    (let loop ([clauses #'(clause ...)])
                      (if (null? clauses)
                          #'(unhandled preinfo pr e* ret r* ctxt ntypes oldtypes)
                          (with-syntax ((rest (loop (cdr clauses))))
                            (syntax-case (car clauses) ()
                              [((x ...) b1 b2 ...)
                               #;guard: (andmap identifier? #'(x ...))
                               (with-syntax ([n (length #'(x ...))]
                                             [(x_r ...) (map make-get-type-name #'(x ...))])
                                 #'(if (eq? count n)
                                       (apply
                                        (apply (lambda (x ...)
                                                 (lambda (x_r ...)
                                                   (begin (define-property x get-type-key #'x_r) ...)
                                                   (begin b1 b2 ...))) e*) r*)
                                       rest))]
                              [(r b1 b2 ...)
                               #;guard: (identifier? #'r)
                               (with-syntax ([r_r (make-get-type-name #'r)])
                                 #'(apply
                                    (apply (lambda r
                                             (lambda r_r
                                               (define-property r get-type-key #'r_r)
                                               b1 b2 ...)) e*) r*))]
                              [((x ... . r) b1 b2 ...)
                               #;guard: (and (andmap identifier? #'(x ...)) (identifier? #'r))
                               (with-syntax ([n (length #'(x ...))]
                                             [(x_r ...) (map make-get-type-name #'(x ...))]
                                             [r_r (make-get-type-name #'r)])
                                 #'(if (fx>= count n)
                                       (apply 
                                         (apply (lambda (x ... . r)
                                                  (lambda (x_r ... . r_r)
                                                    (begin (define-property x get-type-key #'x_r) ...)
                                                    (define-property r get-type-key #'r_r)
                                                     b1 b2 ...)) e*) r*)
                                       rest))]))))])
                 (for-each
                   (lambda (sym-name)
                     (let ([sym-key (datum key)])
                       (if (getprop sym-name sym-key #f)
                           (warningf #f "duplicate ~s handler for ~s" sym-key sym-name)
                           (putprop sym-name sym-key #t))
                       (unless (all-set?
                                 (case (datum lev)
                                   [(2) (prim-mask cptypes2)]
                                   [(3) (prim-mask cptypes3)])
                                 ($sgetprop sym-name '*flags* 0))
                         (warningf #f "undeclared ~s handler for ~s~%" sym-key sym-name))))
                   (datum (prim ...)))
                 #'(begin
                     (let ([handler (lambda (preinfo pr e* ret r* ctxt ntypes oldtypes unhandled)
                                      (let ([level (if (all-set? (prim-mask unsafe) (primref-flags pr)) 3 2)]    
                                            [prim-name 'prim]
                                            [count (length e*)])
                                        body))])
                       ($sputprop 'prim 'key handler)) ...)))])))

      ; Similar to define-specialize, but the arguments are not analyzed yet,
      ; so it's necesary to use Expr, Expr/call or a similar function to analyze them.
      ; Also, the variables ret, ntypes and (get-type <arg>) are not available.
      (define-syntax define-specialize/unrestricted
        (lambda (x)
          (define (make-get-type-name id)
            (datum->syntax-object id
              (gensym (string-append (symbol->string (syntax->datum id))
                                     "-ret-type"))))
          (syntax-case x ()
            [(_key lev prim clause ...)
             (identifier? #'prim)
             #'(_key lev (prim) clause ...)]
            [(_key lev (prim ...) clause ...)
             (andmap identifier? #'(prim ...))
             (with-implicit (_key level prim-name preinfo pr ctxt oldtypes)
               (with-syntax
                 ([key (case (datum lev)
                         [(2) #'cptypes2x]
                         [(3) #'cptypes3x]
                         [else ($oops #f "invalid inline level ~s" (datum lev))])]
                  [body
                    (let loop ([clauses #'(clause ...)])
                      (if (null? clauses)
                          #'(unhandled preinfo pr e* ctxt oldtypes)
                          (with-syntax ((rest (loop (cdr clauses))))
                            (syntax-case (car clauses) ()
                              [((x ...) b1 b2 ...)
                               #;guard: (andmap identifier? #'(x ...))
                               (with-syntax ([n (length #'(x ...))])
                                 #'(if (eq? count n)
                                       (apply (lambda (x ...)
                                                b1 b2 ...) e*)
                                       rest))]
                              [(r b1 b2 ...)
                               #;guard: (identifier? #'r)
                               #'(apply
                                  (lambda r
                                    b1 b2 ...) e*)]
                              [((x ... . r) b1 b2 ...)
                               #;guard: (and (andmap identifier? #'(x ...)) (identifier? #'r))
                               (with-syntax ([n (length #'(x ...))])
                                 #'(if (fx>= count n)
                                       (apply 
                                         (lambda (x ... . r)
                                           b1 b2 ...) e*)
                                       rest))]))))])
                 (for-each
                   (lambda (sym-name)
                     (let ([sym-key (datum key)])
                       (if (getprop sym-name sym-key #f)
                           (warningf #f "duplicate ~s handler for ~s" sym-key sym-name)
                           (putprop sym-name sym-key #t))
                       (unless (all-set?
                                 (case (datum lev)
                                   [(2) (prim-mask cptypes2x)]
                                   [(3) (prim-mask cptypes3x)])
                                 ($sgetprop sym-name '*flags* 0))
                         (warningf #f "undeclared ~s handler for ~s~%" sym-key sym-name))))
                   (datum (prim ...)))
                 #'(begin
                     (let ([handler (lambda (preinfo pr e* ctxt oldtypes unhandled)
                                      (let ([level (if (all-set? (prim-mask unsafe) (primref-flags pr)) 3 2)]    
                                            [prim-name 'prim]
                                            [count (length e*)])
                                        body))])
                       ($sputprop 'prim 'key handler)) ...)))])))

      (define-syntax (get-type stx)
        (lambda (lookup)
          (syntax-case stx ()
            [(_ id) (or (lookup #'id #'get-type-key)
                        ($oops 'get-type "invalid identifier ~s" #'id))])))

      (define-specialize 2 (eq? eqv?)
        [(e1 e2) (let ([r1 (get-type e1)]
                       [r2 (get-type e2)])
                    (cond
                      [(or (predicate-implies-not? r1 r2)
                           (predicate-implies-not? r2 r1))
                       (values (make-seq ctxt (make-seq 'effect e1 e2) false-rec)
                               false-rec ntypes #f #f)]
                      [else
                       (values `(call ,preinfo ,pr ,e1 ,e2)
                               ret
                               ntypes
                               (and (eq? ctxt 'test)
                                    (pred-env-add/ref
                                     (pred-env-add/ref ntypes e1 r2)
                                     e2 r1))
                               #f)]))])

      (define-specialize 2 list
        [() (values null-rec null-rec ntypes #f #f)] ; should have been reduced by cp0
        [e* (values `(call ,preinfo ,pr ,e* ...) 'pair ntypes #f #f)])

      (define-specialize 2 $record
        [(rtd . e*) (values `(call ,preinfo ,pr ,rtd ,e* ...) (rtd->record-predicate rtd #t) ntypes #f #f)])

      (define-specialize 2 (record? $sealed-record?)
        [(val rtd) (let* ([val-type (get-type val)]
                          [to-unsafe (and (fx= level 2) 
                                          (expr-is-rtd? rtd oldtypes))] ; use the old types
                          [level (if to-unsafe 3 level)]
                          [pr (if to-unsafe
                                  (primref->unsafe-primref pr)
                                  pr)])
                     (cond
                       [(predicate-implies? val-type (rtd->record-predicate rtd #f))
                        (values (make-seq ctxt (make-seq 'effect val rtd) true-rec)
                                true-rec ntypes #f #f)]
                       [(predicate-implies-not? val-type (rtd->record-predicate rtd #t))
                        (cond
                          [(fx= level 3)
                           (values (make-seq ctxt (make-seq 'effect val rtd) false-rec)
                                   false-rec ntypes #f #f)]
                          [else
                           (values (make-seq ctxt `(call ,preinfo ,pr ,val ,rtd) false-rec)
                                   false-rec ntypes #f #f)])]
                       [else
                        (values `(call ,preinfo ,pr ,val ,rtd)
                                  ret
                                  ntypes
                                  (and (eq? ctxt 'test)
                                       (pred-env-add/ref ntypes val (rtd->record-predicate rtd #t)))
                                  #f)]))])

      (define-specialize 2 exact?
        [(n) (let ([r (get-type n)])
               (cond
                 [(predicate-implies? r 'exact-integer)
                  (values (make-seq ctxt n true-rec)
                          true-rec ntypes #f #f)]
                 [(predicate-implies? r 'flonum)
                  (values (make-seq ctxt n false-rec)
                          false-rec ntypes #f #f)]
                 [else
                  (values `(call ,preinfo ,pr ,n) ret ntypes #f #f)]))])

      (define-specialize 2 inexact?
        [(n) (let ([r (get-type n)])
               (cond
                 [(predicate-implies? r 'exact-integer)
                  (values (make-seq ctxt n false-rec)
                          false-rec ntypes #f #f)]
                 [(predicate-implies? r 'flonum)
                  (values (make-seq ctxt n true-rec)
                          true-rec ntypes #f #f)]
                 [else
                  (values `(call ,preinfo ,pr ,n) ret ntypes #f #f)]))])

      (define-specialize/unrestricted 2 call-with-values
        [(e1 e2) (let-values ([(e1 ret1 types1 t-types1 f-types1)
                               (Expr/call e1 'value oldtypes oldtypes)])
                   (let-values ([(e2 ret2 types2 t-types2 f-types2)
                                 (Expr/call e2 ctxt types1 oldtypes)])
                     (values `(call ,preinfo ,pr ,e1 ,e2)
                             (if (predicate-implies? ret1 'bottom) ; check if necesary
                                 'bottom
                                 ret2)
                             types2 t-types2 f-types2)))])

      (define-specialize/unrestricted 2 apply
        [(proc . e*) (let-values ([(e* r* t* t-t* f-t*)
                                   (map-values 5 (lambda (e) (Expr e 'value oldtypes)) e*)])
                     (let ([mtypes (fold-left (lambda (f t) (pred-env-intersect/base f t oldtypes)) oldtypes t*)])
                       (let-values ([(proc retproc typesproc t-typesproc f-typesproc)
                                     (Expr/call proc ctxt mtypes oldtypes)])
                         (values `(call ,preinfo ,pr ,proc ,e* ...)
                                 retproc typesproc t-typesproc f-typesproc))))])

      (define-specialize/unrestricted 2 $apply
        [(proc n args) (let*-values ([(n rn tn t-tn f-tn)
                                      (Expr n 'value oldtypes)]
                                     [(args rargs targs t-targs f-targs)
                                      (Expr args 'value oldtypes)])
                         (let* ([predn (primref->argument-predicate pr 1 #t)]
                                [tn (if (predicate-implies-not? rn predn)
                                        'bottom
                                        tn)]
                                [tn (pred-env-add/ref tn n predn)]
                                [predargs (primref->argument-predicate pr 2 #t)]
                                [targs (if (predicate-implies-not? rargs predargs)
                                        'bottom
                                        targs)]
                                [targs (pred-env-add/ref targs args predargs)]
                                [mtypes (pred-env-intersect/base tn targs oldtypes)])
                           (let-values ([(proc retproc typesproc t-typesproc f-typesproc)
                                         (Expr/call proc ctxt mtypes oldtypes)])
                             (values `(call ,preinfo ,pr ,proc ,n ,args)
                                     retproc typesproc t-typesproc f-typesproc))))])

      (let ()
        (define (handle-dynamic-wind critical? in body out ctxt oldtypes)
          (let*-values ([(critical? rcritical? tcritical? t-tcritical? f-tcritical?)
                         (if critical?
                             (Expr critical? 'value oldtypes)
                             (values #f #f oldtypes #f #f))]
                        [(ìn rin tin t-tin f-tin)
                         (Expr/call in 'value tcritical? oldtypes)]
                        [(body rbody tbody t-tbody f-tbody)
                         (Expr/call body 'value tin oldtypes)] ; it's almost possible to use ctxt instead of 'value here
                        [(out rout tout t-tout f-tout)
                         (Expr/call out 'value tin oldtypes)]) ; use tin instead of tbody in case of error or jump.
            (let* ([n-types (pred-env-intersect/base tbody tout tin)]
                   [t-types (and (eq? ctxt 'test)
                                 t-tbody
                                 (pred-env-rebase t-tbody tin n-types))]
                   [f-types (and (eq? ctxt 'test)
                                 f-tbody
                                 (pred-env-rebase f-tbody tin n-types))])
              (values critical? in body out rbody n-types t-types f-types))))
        
        (define-specialize/unrestricted 2 r6rs:dynamic-wind
          [(in body out) (let-values ([(critical? in body out ret n-types t-types f-types)
                                       (handle-dynamic-wind #f in body out ctxt oldtypes)])
                           (values `(call ,preinfo ,pr ,in ,body ,out)
                                   ret n-types t-types f-types))])
        (define-specialize/unrestricted 2 dynamic-wind
          [(in body out) (let-values ([(critical? in body out ret n-types t-types f-types)
                                       (handle-dynamic-wind #f in body out ctxt oldtypes)])
                           (values `(call ,preinfo ,pr ,in ,body ,out)
                                   ret n-types t-types f-types))]
          [(critical? in body out) (let-values ([(critical? in body out ret n-types t-types f-types)
                                                 (handle-dynamic-wind critical? in body out ctxt oldtypes)])
                                     (values `(call ,preinfo ,pr ,critical? ,in ,body ,out)
                                             ret n-types t-types f-types))])
      )

  ))

  (with-output-language (Lsrc Expr)
  
  (define (fold-predicate preinfo pr e* ret r* ctxt ntypes oldtypes)
    ; assume they never raise an error
    ; TODO?: Move to a define-specialize
    (let ([val (car e*)]
          [val-type (car r*)])
      (cond
        [(predicate-implies? val-type (primref->predicate pr #f))
         (values (make-seq ctxt val true-rec)
                 true-rec ntypes #f #f)]
        [(predicate-implies-not? val-type (primref->predicate pr #t))
         (values (make-seq ctxt val false-rec)
                 false-rec ntypes #f #f)]
        [else
         (values `(call ,preinfo ,pr ,val)
                 ret
                 ntypes
                 (and (eq? ctxt 'test)
                      (pred-env-add/ref ntypes val (primref->predicate pr #t)))
                 #f)])))

  (define (fold-call/primref preinfo pr e* ctxt oldtypes)
    (fold-primref/unrestricted preinfo pr e* ctxt oldtypes))

  (define (fold-primref/unrestricted preinfo pr e* ctxt oldtypes)
    (let* ([flags (primref-flags pr)]
           [prim-name (primref-name pr)]
           [handler (or (and (all-set? (prim-mask unsafe) flags)
                             (all-set? (prim-mask cptypes3x) flags)
                             ($sgetprop prim-name 'cptypes3x #f))
                        (and (all-set? (prim-mask cptypes2x) flags)
                             ($sgetprop prim-name 'cptypes2x #f)))])
      (if handler
          (call-with-values
            (lambda () (handler preinfo pr e* ctxt oldtypes fold-primref/next))
            (case-lambda
              [(ir2 ret2 types2 t-types2 f-types2)
               (values ir2 ret2 types2 t-types2 f-types2)]
              [else ($oops 'fold-primref "result of inline handler can't be #f")]))
          (fold-primref/next preinfo pr e* ctxt oldtypes))))

  (define (fold-primref/next preinfo pr e* ctxt oldtypes)
    (let-values ([(t e* r* t* t-t* f-t*)
                  (map-Expr/delayed e* oldtypes)])
      (let ([ret (primref->result-predicate pr)])
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
            [(or (predicate-implies? ret 'bottom)
                 (not (arity-okay? (primref-arity pr) (length e*))))
            (fold-primref/default preinfo pr e* 'bottom r* ctxt pred-env-bottom oldtypes)]
            [else
             (let* ([to-unsafe (and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                                    (all-set? (prim-mask safeongoodargs) (primref-flags pr))
                                    (andmap (lambda (r n)
                                              (predicate-implies? r
                                                                  (primref->argument-predicate pr n #f)))
                                            r* (enumerate r*)))]
                    [pr (if to-unsafe
                           (primref->unsafe-primref pr)
                           pr)])
               (fold-primref/normal preinfo pr e* ret r* ctxt t oldtypes))])))))

  (define (fold-primref/normal preinfo pr e* ret r* ctxt ntypes oldtypes)
    (cond
      [(and (fx= (length e*) 1) (primref->predicate pr #t))
       (fold-predicate preinfo pr e* ret r* ctxt ntypes oldtypes)]
      [else
       (let* ([flags (primref-flags pr)]
              [prim-name (primref-name pr)]
              [handler (or (and (all-set? (prim-mask unsafe) flags)
                                (all-set? (prim-mask cptypes3) flags)
                                ($sgetprop prim-name 'cptypes3 #f))
                           (and (all-set? (prim-mask cptypes2) flags)
                                ($sgetprop prim-name 'cptypes2 #f)))])
         (if handler
             (call-with-values
               (lambda () (handler preinfo pr e* ret r* ctxt ntypes oldtypes fold-primref/default))
               (case-lambda
                 [(ir2 ret2 types2 t-types2 f-types2)
                  (values ir2 ret2 types2 t-types2 f-types2)]
                 [else ($oops 'fold-primref "result of inline handler can't be #f")]))
             (fold-primref/default preinfo pr e* ret r* ctxt ntypes oldtypes)))]))

  (define (fold-primref/default preinfo pr e* ret r* ctxt ntypes oldtypes)
    (values `(call ,preinfo ,pr ,e* ...) ret ntypes #f #f))

  (define (fold-call/lambda preinfo e0 e* ctxt oldtypes)
    (define (finish preinfo preinfo2 x* interface body e* r* ntypes)
       (let ([ntypes/x (fold-left pred-env-add ntypes x* r*)])
         (let*-values ([(body ret n-types/x t-types/x f-types/x)
                       (Expr body ctxt ntypes/x)]
                       [(n-types t-types f-types)
                        (pred-env-triple-filter/base n-types/x t-types/x f-types/x x* ctxt ntypes)])
          (values `(call ,preinfo (case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body)) ,e* ...)
                  ret n-types t-types f-types))))
    (define (bad-arity preinfo e0 e* ctxt ntypes)
      (let*-values ([(e0 ret0 n-types0 t-types0 f-types0)
                     (Expr e0 'value ntypes)])
        (values `(call ,preinfo ,e0 ,e* ...)
               'bottom pred-env-bottom #f #f)))
    (define (cut-r* r* n)
      (let loop ([i n] [r* r*])
        (if (fx= i 0)
            (list (if (null? r*) null-rec 'pair))
            (cons (car r*) (loop (fx- i 1) (cdr r*))))))
    (let*-values ([(ntypes e* r* t* t-t* f-t*)
                   (map-Expr/delayed e* oldtypes)])
      (nanopass-case (Lsrc Expr) e0
        [(case-lambda ,preinfo2 (clause (,x** ...) ,interface* ,body*) ...)
         (let ([len (length e*)])
           (let loop ([x** x**] [interface* interface*] [body* body*])
             (cond
               [(null? interface*)
                (bad-arity preinfo e0 e* ctxt ntypes)]
               [else
                (let ([interface (car interface*)])
                  (cond
                    [(fx< interface 0)
                     (let ([nfixed (fxlognot interface)])
                       (if (fx>= len nfixed)
                           (let ([r* (cut-r* r* nfixed)])
                             (finish preinfo preinfo2 (car x**) interface (car body*) e* r* ntypes))
                           (loop (cdr x**) (cdr interface*) (cdr body*))))]
                    [else
                     (if (fx= interface len)
                         (finish preinfo preinfo2 (car x**) interface (car body*) e* r* ntypes)
                         (loop (cdr x**) (cdr interface*) (cdr body*)))]))])))])))

   (define (pred-env-triple-filter/base ntypes ttypes ftypes x* ctxt base)
     (let* ([ttypes (and (not (eq? ntypes ttypes)) ttypes)]
            [ntypes (and (not (eq? ntypes ttypes)) ntypes)] 
            [ntypes (fold-left (lambda (f x) (pred-env-remove/base f x base)) ntypes x*)]
            [ttypes (and (eq? ctxt 'test)
                          ttypes
                          (fold-left (lambda (f x) (pred-env-remove/base f x ntypes)) ttypes x*))]
            [ftypes (and (eq? ctxt 'test)
                          ftypes
                          (fold-left (lambda (f x) (pred-env-remove/base f x ntypes)) ftypes x*))])
       (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
       (values ntypes ttypes ftypes)))

  (define (fold-call/other preinfo e0 e* ctxt oldtypes)
    (let*-values ([(ntypes e* r* t* t-t* f-t*)
                   (map-Expr/delayed e* oldtypes)]
                  [(e0 ret0 types0 t-types0 f-types0)
                   (Expr/call e0 'value ntypes oldtypes)])
      (values `(call ,preinfo ,e0 ,e* ...)
              ret0 types0 t-types0 f-types0)))

  (define (map-Expr/delayed e* oldtypes)
    (define first-pass* (map (lambda (e)
                               (nanopass-case (Lsrc Expr) e
                                 [(case-lambda ,preinfo ,cl* ...)
                                  (cons 'delayed e)]
                                 [else
                                   (cons 'ready
                                         (call-with-values
                                           (lambda () (Expr e 'value oldtypes))
                                           list))]))
                             e*))
    (define fp-types (fold-left (lambda (t x)
                                  (if (eq? (car x) 'ready)
                                      (pred-env-intersect/base t (caddr (cdr x)) oldtypes)
                                      t))
                                oldtypes
                                first-pass*))
    (define second-pass* (map (lambda (e)
                                (cond
                                  [(eq? (car e) 'delayed)
                                   (call-with-values
                                            (lambda () (Expr (cdr e) 'value fp-types))
                                            list)]
                                  [else
                                   (cdr e)]))
                              first-pass*))
    (define sp-types fp-types) ; since they are only lambdas, they add no new info.
    (define untransposed (if (null? second-pass*)
                             '(() () () () ())
                             (apply map list second-pass*)))
    (apply values sp-types untransposed))

  (define (map-values l f v*)
    ; `l` is the default lenght, in case `v*` is null. 
    (if (null? v*)
      (apply values (make-list l '()))
      (let ()
        (define transposed (map (lambda (x)
                                  (call-with-values
                                    (lambda () (f x))
                                    list))
                                v*))
        (define good (apply map list transposed))
        (apply values good))))

  (define (Expr/fix-tf-types ir ctxt types)
    (let-values ([(ir ret types t-types f-types)
                  (Expr ir ctxt types)])
      (values ir ret
              types
              (if (predicate-implies? ret false-rec)
                  pred-env-bottom
                  (or t-types types))
              (if (predicate-implies? ret 'true)
                  pred-env-bottom
                  (or f-types types)))))

  (define (Expr/call ir ctxt types outtypes)
    (nanopass-case (Lsrc Expr) ir
      [,pr (values pr (primref->result-predicate pr) types #f #f)]
      [(case-lambda ,preinfo ,cl* ...)
       (let loop ([cl* cl*]
                  [rev-rcl* '()]
                  [rret 'bottom]
                  [rtypes types]
                  [rt-types (and (eq? ctxt 'test) types)]
                  [rf-types (and (eq? ctxt 'test) types)])
         (cond
           [(null? cl*)
            (let ([retcl* (reverse rev-rcl*)]) 
              (values `(case-lambda ,preinfo ,retcl* ...)
                      rret rtypes rt-types rf-types))] 
           [else
            (nanopass-case (Lsrc CaseLambdaClause) (car cl*)
              [(clause (,x* ...) ,interface ,body)
               (let-values ([(body ret2 types2 t-types2 f-types2)
                             (Expr body ctxt types)])
                 (let* ([cl2 (with-output-language (Lsrc CaseLambdaClause)
                                 `(clause (,x* ...) ,interface ,body))]
                        [t-types2 (or t-types2 types2)]
                        [f-types2 (or f-types2 types2)])
                   (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
                   (cond
                     [(predicate-implies? ret2 'bottom)
                      (loop (cdr cl*) (cons cl2 rev-rcl*)
                            rret rtypes rt-types rf-types)]
                     [(predicate-implies? rret 'bottom)
                      (loop (cdr cl*) (cons cl2 rev-rcl*)
                            ret2 types2 t-types2 f-types2)]
                     [else
                      (let ([ntypes (pred-env-union/super-base rtypes types
                                                               types2 types
                                                               types
                                                               types)])
                        (loop (cdr cl*)
                              (cons cl2 rev-rcl*)
                              (pred-union rret ret2)
                              ntypes 
                              (cond
                                [(not (eq? ctxt 'test))
                                 #f] ; don't calculate nt-types outside a test context
                                [(and (eq? rtypes rt-types)
                                      (eq? types2 t-types2))
                                 ntypes] ; don't calculate nt-types when it will be equal to ntypes
                                [else
                                 (pred-env-union/super-base rt-types rtypes
                                                            t-types2 types2
                                                            types
                                                            ntypes)])
                              (cond
                                [(not (eq? ctxt 'test))
                                 #f] ; don't calculate nt-types outside a test context
                                [(and (eq? rtypes rf-types)
                                      (eq? types2 f-types2))
                                 ntypes] ; don't calculate nt-types when it will be equal to ntypes
                                [else
                                 (pred-env-union/super-base rf-types rtypes
                                                            f-types2 types2
                                                            types
                                                            ntypes)])))])))])]))]
      [else
       (let-values ([(ir ret n-types t-types f-types)
                     (Expr ir 'value outtypes)])
         (values ir
                (if (predicate-implies-not? ret 'procedure)
                    'bottom
                    #f)
                (pred-env-add/ref (pred-env-intersect/base n-types types outtypes)
                                  ir 'procedure)
                #f #f))]))
  )
)
    (Expr : Expr (ir ctxt types) -> Expr (ret types t-types f-types)
      [(quote ,d)
       (values ir (datum->predicate d ir) types #f #f)]
      [(ref ,maybe-src ,x)
       (case ctxt
         [(test)
          (let ([t (pred-env-lookup types x)])
            (cond
              [(predicate-implies? t 'true)
               (values true-rec true-rec types #f #f)]
              [(predicate-implies? t false-rec)
               (values false-rec false-rec types #f #f)]
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
                  (values t t types #f #f)]
                 [else
                  (values ir t types #f #f)])]
               [else
                (values ir t types #f #f)]))])]
      [(seq ,[e1 'effect types -> e1 ret1 types t-types f-types] ,e2)
       (cond
         [(predicate-implies? ret1 'bottom)
          (values e1 'bottom pred-env-bottom #f #f)]
         [else
          (let-values ([(e2 ret types t-types f-types)
                        (Expr e2 ctxt types)])
            (values (make-seq ctxt e1 e2) ret types t-types f-types))])]
      [(if ,[Expr/fix-tf-types : e1 'test types -> e1 ret1 types1 t-types1 f-types1] ,e2 ,e3)
       (cond
         [(predicate-implies? ret1 'bottom) ;check bottom first
          (values e1 'bottom pred-env-bottom #f #f)]
         [(predicate-implies? ret1 'true)
          (let-values ([(e2 ret types t-types f-types)
                        (Expr e2 ctxt types1)])
            (values (make-seq ctxt e1 e2) ret types t-types f-types))]
         [(predicate-implies? ret1 false-rec)
          (let-values ([(e3 ret types t-types f-types)
                        (Expr e3 ctxt types1)])
            (values (make-seq ctxt e1 e3) ret types t-types f-types))]
         [else
          (let-values ([(e2 ret2 types2 t-types2 f-types2)
                        (Expr/fix-tf-types e2 ctxt t-types1)]
                       [(e3 ret3 types3 t-types3 f-types3)
                        (Expr/fix-tf-types e3 ctxt f-types1)])
            (let ([ir `(if ,e1 ,e2 ,e3)])
              (cond
                [(and (predicate-implies? ret2 'bottom)  ;check bottom first
                      (predicate-implies? ret3 'bottom)) ;check bottom first
                 (values ir 'bottom pred-env-bottom #f #f)]
                [(predicate-implies? ret2 'bottom) ;check bottom first
                 (values (make-seq ctxt `(if ,e1 ,e2 ,void-rec) e3)
                         ret3 types3 t-types3 f-types3)]
                [(predicate-implies? ret3 'bottom) ;check bottom first
                 (values (make-seq ctxt `(if ,e1 ,void-rec ,e3) e2)
                         ret2 types2 t-types2 f-types2)]
                [else
                 (let ([new-types (pred-env-union/super-base types2 t-types1
                                                             types3 f-types1
                                                             types1
                                                             types1)])
                   (values ir
                           (pred-union ret2 ret3)
                           new-types
                           (cond
                             [(not (eq? ctxt 'test))
                              #f] ; don't calculate t-types outside a test context
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
                             [(and (eq? types2 f-types2)
                                   (eq? types3 f-types3))
                              #f] ; don't calculate t-types when it will be equal to new-types
                             [else
                              (pred-env-union/super-base f-types2 t-types1
                                                         f-types3 f-types1
                                                         types1
                                                         new-types)])))])))])]
      [(set! ,maybe-src ,x ,[e 'value types -> e ret types t-types f-types])
       (values `(set! ,maybe-src ,x ,e) void-rec types #f #f)]
      [(call ,preinfo ,pr ,e* ...)
       (fold-call/primref preinfo pr e* ctxt types)]
      [(case-lambda ,preinfo ,cl* ...)
       (let ([cl* (map (lambda (cl)
                        (nanopass-case (Lsrc CaseLambdaClause) cl
                          [(clause (,x* ...) ,interface ,body)
                           (let-values ([(body ret types t-types f-types)
                                         (Expr body 'value types)])
                             (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
                             (with-output-language (Lsrc CaseLambdaClause)
                               `(clause (,x* ...) ,interface ,body)))]))
                       cl*)])
         (values `(case-lambda ,preinfo ,cl* ...) 'procedure types #f #f))]
      [(call ,preinfo (case-lambda ,preinfo2 ,cl* ...) ,e*  ...)
       (fold-call/lambda preinfo `(case-lambda ,preinfo2 ,cl* ...) e* ctxt types)]
      [(call ,preinfo ,e0 ,e*  ...)
       (fold-call/other preinfo e0 e* ctxt types)]
      [(letrec ((,x* ,e*) ...) ,body)
       (let-values ([(ntypes e* r* t* t-t* f-t*)
                     (map-Expr/delayed e* types)])
         (let ([ntypes/x (fold-left pred-env-add ntypes x* r*)])
           (let*-values ([(body ret n-types/x t-types/x f-types/x)
                         (Expr body ctxt ntypes/x)]
                         [(n-types t-types f-types)
                          (pred-env-triple-filter/base n-types/x t-types/x f-types/x x* ctxt ntypes)])
               (values `(letrec ([,x* ,e*] ...) ,body)
                       ret n-types t-types f-types))))]
      [(letrec* ((,x* ,e*) ...) ,body)
       (let*-values ([(e* ntypes/x)
                      (let loop ([x* x*] [e* e*] [types types] [rev-e* '()]) ; this is similar to an ordered-map
                        (if (null? x*)
                          (values (reverse rev-e*) types)
                          (let-values ([(e ret types t-types f-types)
                                        (Expr (car e*) 'value types)])
                            (let ([types (pred-env-add types (car x*) ret)])
                              (loop (cdr x*) (cdr e*) types (cons e rev-e*))))))]
                     [(body ret n-types/x t-types/x f-types/x)
                      (Expr body ctxt ntypes/x)]
                     [(n-types t-types f-types)
                      (pred-env-triple-filter/base n-types/x t-types/x f-types/x x* ctxt types)])
         (values `(letrec* ([,x* ,e*] ...) ,body)
                 ret n-types t-types f-types))]
      [,pr
       (values ir
               (and (all-set? (prim-mask proc) (primref-flags pr)) 'procedure)
               types #f #f)]
      [(foreign (,conv* ...) ,name ,[e 'value types -> e ret types t-types f-types] (,arg-type* ...) ,result-type)
       (values `(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type)
               #f types #f #f)]
      [(fcallable (,conv* ...) ,[e 'value types -> e ret types t-types f-types] (,arg-type* ...) ,result-type)
       (values `(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type)
               #f types #f #f)]
      [(record ,rtd ,[rtd-expr 'value types -> rtd-expr ret-re types-re t-types-re f-types-re]
               ,[e* 'value types -> e* r* t* t-t* f-t*] ...)
       (values `(record ,rtd ,rtd-expr ,e* ...)
               (rtd->record-predicate rtd-expr #t)
               (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)
               #f #f)]
      [(record-ref ,rtd ,type ,index ,[e 'value types -> e ret types t-types f-types])
       (values `(record-ref ,rtd ,type ,index ,e)
               #f
               (pred-env-add/ref types e '$record)
               #f #f)]
      [(record-set! ,rtd ,type ,index ,[e1 'value types -> e1 ret1 types1 t-types1 f-types1]
                    ,[e2 'value types -> e2 ret2 types2 t-types2 f-types2])
       (values `(record-set! ,rtd ,type ,index ,e1 ,e2)
               void-rec
               (pred-env-add/ref (pred-env-intersect/base types1 types2 types)
                                 e1 '$record)
               #f #f)]
      [(record-type ,rtd ,[e 'value types -> e ret types t-types f-types])
       (values `(record-type ,rtd ,e)
               #f types #f #f)]
      [(record-cd ,rcd ,rtd-expr ,[e 'value types -> e ret types t-types f-types])
       (values `(record-cd ,rcd ,rtd-expr ,e)
               #f types #f #f)]
      [(immutable-list (,[e* 'value types -> e* r* t* t-t* f-t*] ...)
                       ,[e 'value types -> e ret types t-types f-types])
       (values `(immutable-list (,e*  ...) ,e)
               ret types #f #f)]
      [(moi) (values ir #f types #f #f)]
      [(pariah) (values ir void-rec types #f #f)]
      [(cte-optimization-loc ,box ,[e 'value types -> e ret types t-types f-types] ,exts)
       (values `(cte-optimization-loc ,box ,e ,exts)
               ret types #f #f)]
      [(cpvalid-defer ,e) (sorry! who "cpvalid leaked a cpvalid-defer form ~s" ir)]
      [(profile ,src) (values ir #f types #f #f)]
      [else ($oops who "unrecognized record ~s" ir)])
    (let-values ([(ir ret types t-types f-types)
                  (Expr ir 'value pred-env-empty)])
      ir))

  (set! $cptypes cptypes)

)

; check to make sure all required handlers were seen, after expansion of the
; expression above has been completed
(let ()
  (define-syntax (test-handlers sxt)
    (for-each
      (lambda (sym)
        (let ([flags ($sgetprop sym '*flags* 0)])
          (when (all-set? (prim-mask cptypes2) flags)
            ; currently all the flags use the same bit
            (let ([used (map (lambda (key) (and (getprop sym key #f)
                                                (begin (remprop sym 'cp02) #t)))
                             '(cptypes2 cptypes3 cptypes2x cptypes3x))])
              (when (andmap not used)
                ($oops 'çptypes "no cptypes handler for ~s" sym))))))
      (oblist))
    #'(void))
  (test-handlers)
)
