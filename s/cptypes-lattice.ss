;;; cptypes-lattice.ss
;;; Copyright 1984-2020 Cisco Systems, Inc.
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

; bottom -> empty set / the expression raised an error
; <something> -> some other set
; ptr -> all single values expressions
; #f -> a result that may be single or multiple valued.

; bottom => <something> => ptr => #f

; properties of bottom:
; (implies? x bottom): only for x=bottom
; (implies? bottom y): always
; (disjoint? x bottom): always
; (disjoint? bottom y): always
; remember to check (implies? x bottom) before (implies? x something)

(module cptypes-lattice
  (predicate-implies?
   predicate-disjoint?
   predicate-intersect
   predicate-union
   make-pred-$record/rtd
   make-pred-$record/ref)

  (include "base-lang.ss")
  (with-output-language (Lsrc Expr)
    (define true-rec `(quote #t)))

  ; don't use rtd-* as defined in record.ss in case we're building a patch
  ; file for cross compilation, because the offsets may be incorrect
  (define rtd-ancestors (csv7:record-field-accessor #!base-rtd 'ancestors))
  (define (rtd-parent x) (vector-ref (rtd-ancestors x) 0))
  ;(define (rtd-ancestry x) ($object-ref 'scheme-object x (constant record-type-ancestry-disp)))
  ;(define (rtd-parent x) (vector-ref (rtd-ancestry x) 0))

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-0})
    (sealed #t))


  (define (check-constant-is? x pred?)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (pred? d)]
           [else #f])))

  (define (check-constant-eqv? x v)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (eqv? d v)]
           [else #f])))

  ;only false-rec, boolean and ptr may be '#f
  ;use when the other argument is truthy bur not exactly '#t
  (define (union/true x)
     (cond
       [(or (eq? x 'boolean)
            (check-constant-eqv? x #f))
        'ptr]
       [else
        'true]))

  (define (union/simple x pred? y)
     (cond
       [(or (check-constant-is? x pred?)
            (eq? x y))
        y]
       [else
        (union/true x)]))

  (define (union/symbol x pred? y)
     (cond
       [(or (check-constant-is? x pred?)
            (eq? x y))
        y]
       [(or (eq? x 'gensym)
		    (eq? x 'interned-symbol)
		    (eq? x 'uninterned-symbol)
	    	(eq? x 'symbol)
		    (check-constant-is? x symbol?))
		'symbol]
       [else
        (union/true x)]))

  (define (union/record x)
     (cond
       [(or (pred-$record/rtd? x)
            (pred-$record/ref? x)
	    	(eq? x '$record))
		'$record]
       [else
        (union/true x)]))

  (define (union/fixnum x)
    (cond 
 	  [(check-constant-is? x target-fixnum?)
 	   'fixnum]
	  [(or (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (check-constant-is? x exact-integer?))
	   'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
 	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/bignum x)
    (cond 
 	  [(check-constant-is? x target-bignum?)
	   'bignum]
	  [(or (eq? x 'fixnum)
	 	   (eq? x 'exact-integer)
	 	   (check-constant-is? x exact-integer?))
	   'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/exact-integer x)
    (cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (check-constant-is? x exact-integer?))
	  'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/flonum x)
    (cond 
 	  [(or (check-constant-is? x flonum?))
	   'flonum]
	  [(or (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/real x)
    (cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (eq? x 'flonum)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/number x)
	(cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  ;true when x is an ancestor of y
  ;includes the case when the are the same
  (define (rtd-ancestor*? x y)
    (or (eq? x y)
        (let ()
          (define ax (rtd-ancestors x))
          (define lx (vector-length ax))
          (define ay (rtd-ancestors y))
          (define ly (vector-length ay))
          (let ([pos (fx- ly lx 1)])
            (and (fx>= pos 0)
                 (eq? x (vector-ref ay pos)))))))

  ;includes the case when the are the same
  ;or when one is the ancester of the other
  (define (rdt-last-common-ancestor* x y)
    (cond 
      [(eq? x y) x]
      [else
       (let ()
         (define ax (rtd-ancestors x))
         (define lx (vector-length ax))
         (define ay (rtd-ancestors y))
         (define ly (vector-length ay))
         (cond
           [(let ([pos (fx- ly lx 1)])
              (and (fx>= pos 0)
                   (eq? x (vector-ref ay pos))))
            x]
           [(let ([pos (fx- lx ly 1)])
              (and (fx>= pos 0)
                   (eq? y (vector-ref ax pos))))
            y]
           [(fx= lx 1) #f]
           [(fx= ly 1) #f]
           [else 
             (let ()
               (define offset (fx- lx ly))
               (define f (if (fx< lx ly) (fx- offset) 0))
               (define l (fx- ly 1))
               (cond
                 [(eq? (vector-ref ay f)
                       (vector-ref ax (fx+ f offset)))
                  (vector-ref ay f)]
                 [else
                  (let loop ([f f] [l l])
                    (cond
                      [(fx= (fx- l f) 1) (vector-ref ay l)]
                      [else
                       (let ()
                         (define m (fxquotient (fx+ f l) 2))
                         (if (eq? (vector-ref ay m)
                                  (vector-ref ax (fx+ m offset)))
                          (loop f m)
                          (loop m l)))]))]))]))]))

  (define (exact-integer? x)
    (and (integer? x) (exact? x)))

  (define (interned-symbol? x)
    (and (symbol? x)
         (not (gensym? x))
         (not (uninterned-symbol? x))))

  ;If x and y are equivalent, they result must be eq? to y
  ;so it's easy to test in predicate-implies?.
  ;The result may be bigger than the actual union. 
  (define (predicate-union x y)
    (cond
      [(eq? x y) y]
      [(not x) #f] ;possible a multivalued expression
      [(not y) #f] ;possible a multivalued expression
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y 'ptr) y]
      [(eq? x 'ptr) x]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             y]
            [(not dy)
             (cond 
               [(or (eq? x 'boolean)
                    (check-constant-eqv? x #t))
                'boolean]
               [else
                'ptr])]
            [(eq? dy #t)
             (cond 
               [(or (eq? x 'boolean)
                    (check-constant-eqv? x #f))
                'boolean]
               [else
                'true])]
            [(null? dy)
             (cond 
               [(or (eq? x 'null-or-pair)
                    (eq? x 'pair))
                'null-or-pair]
               [else
                (union/true x)])]
			[(fixnum? dy)
  			 (union/fixnum x)]
			[(bignum? dy)
  			 (union/bignum x)]
		  	[(exact-integer? dy)
  			 (union/exact-integer x)]
		  	[(flonum? dy)
  			 (union/flonum x)]
			[(real? dy)
  			 (union/real x)]
			[(number? dy)
  			 (union/number x)]
            [(gensym? dy) (union/symbol x gensym? 'gensym)]
            [(uninterned-symbol? dy) (union/symbol x uninterned-symbol? 'uninterned-symbol)]
            [(interned-symbol? dy) (union/symbol x interned-symbol? 'interned-symbol)]
            [(char? dy) (union/simple x char? 'char)]
            [(vector? dy) (union/simple x vector? 'vector)]; i.e. #()
            [(string? dy) (union/simple x string? 'string)]; i.e. ""
            [(bytevector? dy) (union/simple x bytevector? 'bytevector)] ; i.e. '#vu8()
            [(fxvector? dy) (union/simple x fxvector? 'fxvector)] ; i.e. '#vfx()
            [(flvector? dy) (union/simple x flvector? 'flvector)] ; i.e. '#vfl()
            [else
             (union/true x)])])]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               y]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor*? y-rtd x-rtd) y '$record)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor*? x-rtd y-rtd) x '$record)]
              [else
               (let ([lca-rtd (rdt-last-common-ancestor* x-rtd y-rtd)])
                 (cond
                   [(not lca-rtd) '$record]
                   [(eqv? lca-rtd y-rtd) y]
                   [(eqv? lca-rtd x-rtd) x]
                   [else (make-pred-$record/rtd lca-rtd)]))]))]
         [else (union/record x)])]
      [(pred-$record/ref? y)
       (cond
         [(pred-$record/ref? x)
          (if (eq? (pred-$record/ref-ref x)
                   (pred-$record/ref-ref y))
             y
             '$record)]
         [else (union/record x)])]
      [else
       (case y
         [($record)
          (union/record x)] ; y must be the symbol '$record
         [(null-or-pair)
          (cond 
            [(or (eq? x 'pair)
                 (check-constant-eqv? x '()))
             y]
            [else (union/true x)])]
         [(pair)
          (cond 
            [(or (eq? x 'null-or-pair)
                 (check-constant-eqv? x '()))
             'null-or-pair]
            [else
             (union/true x)])]
		 [(fixnum)
  		  (union/fixnum x)]
		 [(bignum)
  		  (union/bignum x)]
		 [(exact-integer)
  		  (union/exact-integer x)]
		 [(flonum)
  		  (union/flonum x)]
		 [(real)
  		  (union/real x)]
		 [(number)
  		  (union/number x)]
         [(gensym)
          (union/symbol x gensym? 'gensym)]
         [(uninterned-symbol)
          (union/symbol x uninterned-symbol? 'uninterned-symbol)]
         [(interned-symbol)
          (union/symbol x interned-symbol? 'interned-symbol)]
         [(symbol)
          (union/symbol x symbol? 'symbol)]
         [(boolean)
          (cond 
            [(check-constant-is? x boolean?)
             y]
            [else
             'ptr])]
         [(char) (union/simple x char? y)]
         [(vector) (union/simple x vector? y)]; i.e. #()
         [(string) (union/simple x string? y)]; i.e. ""
         [(bytevector) (union/simple x bytevector? y)] ; i.e. '#vu8()
         [(fxvector) (union/simple x fxvector? y)] ; i.e. '#vfx()
         [(flvector) (union/simple x flvector? y)] ; i.e. '#vfl()
         [else (union/true x)])]))

  (define (intersect/simple x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(or (eq? x qpred)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/record x y)
    (cond
      [(or (pred-$record/ref? x)
           (pred-$record/rtd? x))
       x]
      [(or (eq? x '$record)
           (eq? x 'true))
       y]
      [else
       'bottom]))

  (define (intersect/symbol x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(or (eq? x qpred)
            (eq? x 'symbol)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/fixnum x check? y)
     (cond
       [(and check? (check-constant-is? x fixnum?))
        x]
       [(or (eq? x 'fixnum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/bignum x check? y)
     (cond
       [(and check? (check-constant-is? x bignum?))
        x]
       [(or (eq? x 'bignum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/exact-integer x check? y)
     (cond
       [(and check? (or (check-constant-is? x exact-integer?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)))
        x]
       [(or (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/flonum x check? y)
     (cond
       [(and check? (check-constant-is? x flonum?))
        x]
       [(or (eq? x 'flonum)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/real x check? y)
     (cond
       [(and check? (or (check-constant-is? x real?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)
                        (eq? x 'exact-integer)
                        (eq? x 'flonum)))
        x]
       [(or (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/number x check? y)
     (cond
       [(and check? (eq? x 'fixnum))
        x]
       [(and check? (or (check-constant-is? x number?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)
                        (eq? x 'exact-integer)
                        (eq? x 'flonum)
                        (eq? x 'real)))
        x]
       [(or (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  ;The result may be bigger than the actual intersection 
  ;if there is no exact result, it must be at least included in x
  ;so it's possible to make decreasing sequences.
  ;Anyway, for now the result is exact.
  (define (predicate-intersect x y)
    (cond
      [(eq? x y) x]
      [(not y) x]
      [(not x) y]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y 'ptr) x]
      [(eq? x 'ptr) y]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             x]
            [(not dy)
             (cond 
               [(eq? x 'boolean)
                y]
               [else
                'bottom])]
            [(eq? dy #t)
             (cond 
               [(or (eq? x 'boolean)
                    (eq? x 'true))
                y]
               [else
                'bottom])]
            [(null? dy)
             (cond 
               [(or (eq? x 'null-or-pair)
                    (eq? x 'true))
                y]
               [else
                'bottom])]
			[(fixnum? dy)
  			 (intersect/fixnum x #f y)]
			[(bignum? dy)
  			 (intersect/bignum x #f y)]
		  	[(exact-integer? dy)
  			 (intersect/exact-integer x #f y)]
		  	[(flonum? dy)
  			 (intersect/flonum x #f y)]
			[(real? dy)
  			 (intersect/real x #f y)]
			[(number? dy)
  			 (intersect/number x #f y)]
            [(gensym? dy) (intersect/symbol x #f 'gensym y)]
            [(uninterned-symbol? dy) (intersect/symbol x #f 'uninterned-symbol y)]
            [(interned-symbol? dy) (intersect/symbol x #f 'interned-symbol y)]
            [(char? dy) (intersect/simple x #f 'char y)]
            [(vector? dy) (intersect/simple x #f 'vector y)]; i.e. #()
            [(string? dy) (intersect/simple x #f 'string y)]; i.e. ""
            [(bytevector? dy) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
            [(fxvector? dy) (intersect/simple x #f 'fxvector y)] ; i.e. '#vfx()
            [(flvector? dy) (intersect/simple x #f 'flvector y)] ; i.e. '#vfl()
            [else
             (cond 
               [(eq? x 'true)
                y]
               [else
                'bottom])])])]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               x]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor*? y-rtd x-rtd) x 'bottom)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor*? x-rtd y-rtd) y 'bottom)]
              [else
               (cond
                 [(rtd-ancestor*? y-rtd x-rtd) x]
                 [(rtd-ancestor*? x-rtd y-rtd) y]
                 [else 'bottom])]))]
         [else
          (intersect/record x y)])]
      [(pred-$record/ref? y)
       (intersect/record x y)]
      [else
       (case y
         [($record)
          (intersect/record x y)]
         [(null-or-pair)
          (cond 
            [(eq? x 'pair)
             'pair]
            [(check-constant-eqv? x '())
             x]
            [(eq? x 'true)
             'null-or-pair]
            [else 'bottom])]
         [(pair)
          (cond 
            [(or (eq? x 'null-or-pair)
                 (eq? x 'true))
             'pair]
            [else
             'bottom])]
		 [(fixnum)
  		  (intersect/fixnum x #t y)]
		 [(bignum)
  		  (intersect/bignum x #t y)]
		 [(exact-integer)
  		  (intersect/exact-integer x #t y)]
		 [(flonum)
  		  (intersect/flonum x #t y)]
		 [(real)
  		  (intersect/real x #t y)]
		 [(number)
  		  (intersect/number x #t y)]
         [(gensym)
          (intersect/symbol x gensym? 'gensym y)]
         [(uninterned-symbol)
          (intersect/symbol x uninterned-symbol? 'uninterned-symbol y)]
         [(interned-symbol)
          (intersect/symbol x interned-symbol? 'interned-symbol y)]
         [(symbol)
          (cond 
            [(or (eq? x 'gensym)
                 (eq? x 'uninterned-symbol)
                 (eq? x 'interned-symbol)
                 (eq? x 'symbol)
                 (eq? x 'true)
                 (check-constant-is? x symbol?))
             x]
            [else
             'bottom])]
         [(boolean)
          (cond 
            [(eq? x 'true)
             true-rec]
            [(check-constant-is? x boolean?)
             x]
            [else
             'bottom])]
         [(true)
          (cond 
            [(eq? x 'boolean)
             true-rec]
            [(check-constant-eqv? x #f)
             'bottom]
            [else
             x])]
         [(char) (intersect/simple x char? 'char y)]
         [(vector) (intersect/simple x vector? 'vector y)]; i.e. #()
         [(string) (intersect/simple x string? 'string y)]; i.e. ""
         [(bytevector) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
         [(fxvector) (intersect/simple x fxvector? 'fxvector y)] ; i.e. '#vfx()
         [(flvector) (intersect/simple x flvector? 'flvector y)] ; i.e. '#vfl()
         [else
          (cond 
            [(eq? x 'true)
             y]
            [else
             'bottom])])]))



  (define (predicate-implies? x y)
    (eq? (predicate-union x y) y))

  (define (predicate-disjoint? x y)
    (eq? (predicate-intersect x y) 'bottom))
)
