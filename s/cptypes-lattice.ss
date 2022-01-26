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
  (primref-name/nqm->predicate
   ptr-pred
   char-pred
   eof/char-pred
   maybe-char-pred
   maybe-symbol-pred
   $fixmediate-pred
   $list-pred ; immutable lists
   boolean-pred
   true-pred ; anything that is not #f
   true-rec  ; only the #t object
   false-rec
   void-rec
   null-rec
   eof-rec
   bwp-rec
   predicate-is-ptr?
   predicate-implies?
   predicate-disjoint?
   predicate-intersect
   predicate-union
   predicate-substract
   make-pred-$record/rtd
   make-pred-$record/ref)

  (define-record-type pred-or
    (fields imm nor rec)
    (nongenerative #{pred-or nlomo7xtc1nguv2umpzwho0dt-0})
    (sealed #t))

  (define-record-type pred-immediate
    (fields mask)
    (nongenerative #{pred-or m4e7t2fuam2my9kt17zpmxnzc-0})
    (sealed #t))

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref maybe-rtd)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-1})
    (sealed #t))

  (define ($black-hole) '#6=#6#)
  (include "base-lang.ss")
  (with-output-language (Lsrc Expr)
    (define void-rec `(quote ,(void)))
    (define true-rec `(quote #t))
    (define false-rec `(quote #f))
    (define null-rec `(quote ()))
    (define eof-rec `(quote #!eof))
    (define bwp-rec `(quote #!bwp))
    (define black-hole-rec `(quote ,($black-hole)))
    (define unbound-rec `(quote ,($unbound-object))))

  (module (immediate-rec->mask
           build-pred-immediate
           immediate-pred-mask
           immediate-pred true-immediate-pred boolean-pred
           char-pred maybe-char-pred eof/char-pred)

    (define false-object-mask               #b0000000001)
    (define true-object-mask                #b0000000010)
    (define null-object-mask                #b0000000100)
    (define void-object-mask                #b0000001000)
    (define eof-object-mask                 #b0000010000)
    (define bwp-object-mask                 #b0000100000)
    (define black-hole-object-mask          #b0001000000)
    (define unbound-object-mask             #b0010000000)

    (define char-pred-mask                  #b0100000000)
    (define immediate-pred-mask             #b0111111111)

    (define boolean-pred-mask (fxior true-object-mask false-object-mask))
    (define maybe-char-pred-mask (fxior char-pred-mask false-object-mask))
    (define eof/char-pred-mask (fxior char-pred-mask eof-object-mask))
    (define true-immediate-pred-mask (fxand immediate-pred-mask (fxnot false-object-mask)))

    (define (immediate-rec->mask x extend)
      (cond
        [(Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d)
            (cond
              [(char? d) (if (not extend) 0 char-pred-mask)]
              [(not d) false-object-mask]
              [(eq? d #t) true-object-mask]
              [(null? d) null-object-mask]
              [(eq? d (void)) void-object-mask]
              [(eof-object? d) eof-object-mask]
              [(bwp-object? d) bwp-object-mask]
              [(eq? d ($black-hole)) black-hole-object-mask]
              [($unbound-object? d) unbound-object-mask]
              [else ($oops 'immediate-rec->mask "invalid immediate ~s" d)])]
           [else ($oops 'immediate-rec->mask "invalid expression ~s" x)])]
        [else ($oops 'immediate-rec->mask "invalid expression ~s" x)]))

    (define (mask->immediate-rec y)
      (cond
        [(fx= y char-pred-mask) char-pred]
        [(fx= y false-object-mask) false-rec]
        [(fx= y true-object-mask) true-rec]
        [(fx= y null-object-mask) null-rec]
        [(fx= y void-object-mask) void-rec]
        [(fx= y eof-object-mask) eof-rec]
        [(fx= y bwp-object-mask) bwp-rec]
        [(fx= y black-hole-object-mask) black-hole-rec]
        [(fx= y unbound-object-mask) unbound-rec]
        [else ($oops 'mask->immediate-rec "invalid mask number ~s" y)]))  

    (define (build-pred-immediate mask x y)
      (cond
        [(and x (fx= (pred-immediate-mask x) mask)) x]
        [(and y (fx= (pred-immediate-mask y) mask)) y]
        [else
         (case (fxbit-count mask)
           [(0) 'bottom]
           [(1) (mask->immediate-rec mask)]
           [else (make-pred-immediate mask)])]))

    (define maybe-char-pred (make-pred-immediate maybe-char-pred-mask))
    (define eof/char-pred (make-pred-immediate eof/char-pred-mask))
    (define char-pred (make-pred-immediate char-pred-mask))
    (define boolean-pred (make-pred-immediate boolean-pred-mask))
    (define immediate-pred (make-pred-immediate immediate-pred-mask))
    (define true-immediate-pred (make-pred-immediate true-immediate-pred-mask))
  )

  (define true-pred (make-pred-or true-immediate-pred 'normalptr '$record))
  (define ptr-pred (make-pred-or immediate-pred 'normalptr '$record))
  (define null-or-pair-pred (make-pred-or null-rec 'pair 'bottom))
  (define $list-pred (make-pred-or null-rec '$list-pair 'bottom))
  (define $fixmediate-pred (make-pred-or immediate-pred 'fixnum 'bottom))
  (define maybe-fixnum-pred (make-pred-or false-rec 'fixnum 'bottom))
  (define eof/fixnum-pred (make-pred-or eof-rec 'fixnum 'bottom))
  (define maybe-exact-integer-pred (make-pred-or false-rec 'exact-integer 'bottom))
  (define maybe-flonum-pred (make-pred-or false-rec 'flonum 'bottom))
  (define maybe-number-pred (make-pred-or false-rec 'number 'bottom))
  (define maybe-symbol-pred (make-pred-or false-rec 'symbol 'bottom))
  (define maybe-procedure-pred (make-pred-or false-rec 'procedure 'bottom))
  (define maybe-string-pred (make-pred-or false-rec 'string 'bottom))
  (define eof/string-pred (make-pred-or eof-rec 'string 'bottom))
  (define maybe-bytevector-pred (make-pred-or false-rec 'bytevector 'bottom))
  (define eof/bytevector-pred (make-pred-or eof-rec 'bytevector 'bottom))
  (define maybe-pair-pred (make-pred-or false-rec 'pair 'bottom))
  (define maybe-normalptr-pred (make-pred-or false-rec 'normalptr 'bottom))
  (define maybe-$record-pred (make-pred-or false-rec 'bottom '$record))

  ; This can be implemented with implies?
  ; but let's use the straightforward test.
  (define (predicate-is-ptr? x)
    (and (pred-or? x)
         (let ([i (pred-or-imm x)])
           (and (pred-immediate? i)
                (fx= (pred-immediate-mask i) immediate-pred-mask)))
         (eq? (pred-or-nor x) 'normalptr)
         (eq? (pred-or-rec x) '$record)))

  ; don't use rtd-* as defined in record.ss in case we're building a patch
  ; file for cross compilation, because the offsets may be incorrect
  (define rtd-ancestors (csv7:record-field-accessor #!base-rtd 'ancestors))
  (define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))

  ;could be a ctrtd
  (define (pred-$record-maybe-rtd x)
    (cond
     [(pred-$record/rtd? x) (pred-$record/rtd-rtd x)]
     [(pred-$record/ref? x) (pred-$record/ref-maybe-rtd x)]
     [else #f]))

  (define (rtd-obviously-incompatible? x y)
    (let ([x-flds (rtd-flds x)]
          [y-flds (rtd-flds y)])
      (or (and (fixnum? x-flds) (not (fixnum? y-flds)))
          (and (not (fixnum? x-flds)) (fixnum? y-flds)))))

  ;true when x is an ancestor of y
  ;includes the case when they are the same
  (define (rtd-ancestor*? x y)
    (or (eq? x y)
        (let ()
          (define ax (rtd-ancestors x))
          (define lx (vector-length ax))
          (define ay (rtd-ancestors y))
          (define ly (vector-length ay))
          (and (fx<= lx ly)
               (eq? x (vector-ref ay (fx- lx 1)))))))

  ;includes the case when they are the same
  ;or when one is the ancestor of the other
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
           [(and (fx<= lx ly)
                 (eq? x (vector-ref ay (fx- lx 1))))
            x]
           [(and (fx<= ly lx)
                 (eq? y (vector-ref ax (fx- ly 1))))
            y]
           [else
            ;; binary search to find a common prefix, given that
            ;; no elements are the same after a common prefix
            (let loop ([lo 0] [hi (fxmin lx ly)])
              (cond
                [(fx= lo hi) #f]
                [else (let* ([i (fxquotient (fx+ lo hi) 2)]
                             [v (vector-ref ax i)])
                        (cond
                          [(eq? v (vector-ref ay i))
                           (or (loop (fx+ i 1) hi)
                               v)]
                          [else
                           (loop lo i)]))]))]))]))

  ; nqm: no question mark
  ; Transform the types used in primdata.ss
  ; to the internal representation used here
  ; When extend is #f the result is a predicate that recognizes less values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #t and (something x) ==> (#3%something x)
  ; When extend is #t the result is a predicate that recognizes more values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #f and (something x) ==> <error>
  ; In case the non extended version is not #f, the extended version must be not #f
  (define (primref-name/nqm->predicate name extend?)
    (cond
      [(not name)
       #f]
      [(pair? name)
       (cond
         [(equal? name '(ptr . ptr))
          'pair]
         [else
          (if (not extend?) 'bottom 'pair)])]
      [else
       (let ([r (do-primref-name/nqm->predicate name extend?)])
         (cond
           [(pair? r)
            (if extend? (cdr r) (car r))]
           [else
            r]))]))

  (define (do-primref-name/nqm->predicate name extend?)
    (case name
      [bottom 'bottom]
      [ptr ptr-pred]
      [sub-ptr (cons 'bottom ptr-pred)]

      [char char-pred]
      [maybe-char maybe-char-pred]
      [eof/char eof/char-pred]
      [boolean boolean-pred]
      [true true-pred]
      [false false-rec]
      [void void-rec]
      [null null-rec]
      [eof-object eof-rec]
      [bwp-object bwp-rec]
      [$immediate immediate-pred]

      [pair 'pair]
      [maybe-pair maybe-pair-pred]
      [list (cons $list-pred null-or-pair-pred)]
      [list-assuming-immutable $list-pred]
      [box 'box]
      [vector 'vector]
      [string 'string]
      [sub-string '(bottom . string)]
      [maybe-string maybe-string-pred]
      [eof/string eof/string-pred]
      [bytevector 'bytevector]
      [maybe-bytevector maybe-bytevector-pred]
      [eof/bytevector eof/bytevector-pred]
      [fxvector 'fxvector]
      [flvector 'flvector]
      [pathname 'string]
      [maybe-pathname maybe-string-pred]
      [procedure 'procedure]
      [maybe-procedure maybe-procedure-pred]
      [maybe-who (cons maybe-symbol-pred maybe-normalptr-pred)] ;should be maybe-string/symbol

      [gensym 'gensym]
      [uninterned-symbol 'uninterned-symbol]
      [interned-symbol 'interned-symbol]
      [symbol 'symbol]
      [maybe-symbol maybe-symbol-pred]
      [sub-symbol '(bottom . symbol)]
      [maybe-sub-symbol (cons false-rec maybe-symbol-pred)]

      [fixnum 'fixnum]
      [(sub-fixnum bit length sub-length ufixnum sub-ufixnum pfixnum index sub-index u8 s8 u8/s8) '(bottom . fixnum)]
      [maybe-fixnum maybe-fixnum-pred]
      [maybe-ufixnum (cons false-rec maybe-fixnum-pred)]
      [(eof/length eof/u8) (cons eof-rec eof/fixnum-pred)]
      [bignum 'bignum]
      [(exact-integer sint) 'exact-integer]
      [(uint sub-uint nzuint exact-uinteger sub-sint) '(bottom . exact-integer)]
      [maybe-uint (cons false-rec maybe-exact-integer-pred)]
      [flonum 'flonum]
      [sub-flonum '(bottom . flonum)]
      [maybe-flonum maybe-flonum-pred]
      [real 'real]
      [(integer rational) '(exact-integer . real)]
      [(uinteger sub-integer) '(bottom . real)]
      [cflonum '(flonum . number)]
      [number 'number]
      [sub-number '(bottom . number)]
      [maybe-number maybe-number-pred]

      [$record '$record]
      [(record rtd) '(bottom . $record)] ; not sealed
      [(maybe-rtd) (cons false-rec maybe-$record-pred)]
      [(transcoder textual-input-port textual-output-port binary-input-port binary-output-port) '(bottom . $record)]  ; opaque
      [(maybe-transcoder maybe-textual-input-port maybe-textual-output-port maybe-binary-input-port maybe-binary-output-port input-port output-port) (cons false-rec maybe-$record-pred)]
      [(rcd sfd timeout) '(bottom . $record)] ; not opaque, sealed
      [(maybe-rcd maybe-sub-rcd maybe-sfd maybe-timeout) (cons false-rec maybe-$record-pred)]

      [else (cons 'bottom true-pred)])); for all other types that exclude #f

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

  (define (constant-value x)
    (if (Lsrc? x)
        (nanopass-case (Lsrc Expr) x
          [(quote ,d) d]
          [else ($oops 'constant-value "invalid expression ~s" x)])
        ($oops 'constant-value "invalid expression ~s" x)))

  (define (exact-integer? x)
    (and (integer? x) (exact? x)))

  (define (interned-symbol? x)
    (and (symbol? x)
         (not (gensym? x))
         (not (uninterned-symbol? x))))

  (define (predicate-union/immediate x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(Lsrc? x)
       (let ([mx (immediate-rec->mask x #t)])
         (cond
           [(Lsrc? y)
            (cond
              [(eqv? (constant-value x)
                     (constant-value y))
               y]
              [else
               (let ([my (immediate-rec->mask y #t)])
                 (build-pred-immediate (fxior mx my) #f #f))])]
           [(pred-immediate? y)
            (let ([my (pred-immediate-mask y)])
              (build-pred-immediate (fxior mx my) y #f))]
           [else 
            ($oops 'predicate-union/immediate "invalid expression ~s" y)]))]
      [(pred-immediate? x)
       (let ([mx (pred-immediate-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (immediate-rec->mask y #t)])
              (build-pred-immediate (fxior mx my) x #f))]
           [(pred-immediate? y)
            (let ([my (pred-immediate-mask y)])
              (build-pred-immediate (fxior mx my) y x))]
           [else
            ($oops 'predicate-union/immediate "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-union/immediate "invalid expression ~s" x)]))

  (define (union/simple x pred? y)
    (cond
      [(or (check-constant-is? x pred?)
           (eq? x y))
       y]
      [else
       'normalptr]))

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
        'normalptr]))

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
	   'normalptr]))

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
	   'normalptr]))

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
	   'normalptr]))

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
	   'normalptr]))

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
	   'normalptr]))

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
	   'normalptr]))

  (define (predicate-union/normal x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y 'normalptr) y]
      [(eq? x 'normalptr) x]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             y]
			[(target-fixnum? dy)
  			 (union/fixnum x)]
			[(target-bignum? dy)
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
            [(vector? dy) (union/simple x vector? 'vector)]; i.e. #()
            [(string? dy) (union/simple x string? 'string)]; i.e. ""
            [(bytevector? dy) (union/simple x bytevector? 'bytevector)] ; i.e. '#vu8()
            [(fxvector? dy) (union/simple x fxvector? 'fxvector)] ; i.e. '#vfx()
            [(flvector? dy) (union/simple x flvector? 'flvector)] ; i.e. '#vfl()
            [else
             'normalptr])])]
      [else
       (case y
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
         [(pair $list-pair)
          (cond 
	        [(or (eq? x 'pair)
		         (eq? x '$list-pair))
	         'pair]
	        [else
	         'normalptr])]
         [(vector) (union/simple x vector? y)]; i.e. #()
         [(string) (union/simple x string? y)]; i.e. ""
         [(bytevector) (union/simple x bytevector? y)] ; i.e. '#vu8()
         [(fxvector) (union/simple x fxvector? y)] ; i.e. '#vfx()
         [(flvector) (union/simple x flvector? y)] ; i.e. '#vfl()
         [else
          'normalptr])]))

  (define (predicate-union/record x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y '$record) y]
      [(eq? x '$record) x]
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
         [else
          '$record])]
      [(pred-$record/ref? y)
       (cond
         [(pred-$record/ref? x)
          (if (eq? (pred-$record/ref-ref x)
                   (pred-$record/ref-ref y))
             y
             '$record)]
         [else
          '$record])]
      [else
       '$record]))

  (define (predicate-intersect/immediate x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x) (constant-value y))
              x
              'bottom)]
         [(pred-immediate? y)
          (let ([mx (immediate-rec->mask x #t)]
                [my (pred-immediate-mask y)])
            (if (not (fx= (fxand mx my) 0))
                x
                'bottom))]
         [else
          ($oops 'predicate-intersect/immediate "invalid expression ~s" y)])]
      [(pred-immediate? x)
       (let ([mx (pred-immediate-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (immediate-rec->mask y #t)])
              (if (not (fx= (fxand mx my) 0))
                  y
                  'bottom))]
           [(pred-immediate? y)
            (let ([my (pred-immediate-mask y)])
              (build-pred-immediate (fxand mx my) x y))]
           [else
            ($oops 'predicate-intersect/immediate "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-intersect/immediate "invalid expression ~s" x)]))

  (define (intersect/simple x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(eq? x qpred)
        y]
       [else
        'bottom]))

  (define (intersect/symbol x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(or (eq? x qpred)
            (eq? x 'symbol))
        y]
       [else
        'bottom]))

  (define (intersect/fixnum x check? y)
     (cond
       [(and check? (check-constant-is? x target-fixnum?))
        x]
       [(or (eq? x 'fixnum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number))
        y]
       [else
        'bottom]))

  (define (intersect/bignum x check? y)
     (cond
       [(and check? (check-constant-is? x target-bignum?))
        x]
       [(or (eq? x 'bignum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number))
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
            (eq? x 'number))
        y]
       [else
        'bottom]))

  (define (intersect/flonum x check? y)
     (cond
       [(and check? (check-constant-is? x flonum?))
        x]
       [(or (eq? x 'flonum)
            (eq? x 'real)
            (eq? x 'number))
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
            (eq? x 'number))
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
       [(eq? x 'number)
        y]
       [else
        'bottom]))

  (define (predicate-intersect/normal x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y 'normalptr) x]
      [(eq? x 'normalptr) y]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             x]
			[(target-fixnum? dy)
  			 (intersect/fixnum x #f y)]
			[(target-bignum? dy)
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
            [(vector? dy) (intersect/simple x #f 'vector y)]; i.e. #()
            [(string? dy) (intersect/simple x #f 'string y)]; i.e. ""
            [(bytevector? dy) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
            [(fxvector? dy) (intersect/simple x #f 'fxvector y)] ; i.e. '#vfx()
            [(flvector? dy) (intersect/simple x #f 'flvector y)] ; i.e. '#vfl()
            [else
             'bottom])])]
      [else
       (case y
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
                 (check-constant-is? x symbol?))
             x]
            [else
             'bottom])]
         [(pair $list-pair)
          (cond
            [(or (eq? x 'pair)
                 (eq? x '$list-pair))
             '$list-pair]
            [else
             'bottom])]
         [(vector) (intersect/simple x vector? 'vector y)]; i.e. #()
         [(string) (intersect/simple x string? 'string y)]; i.e. ""
         [(bytevector) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
         [(fxvector) (intersect/simple x fxvector? 'fxvector y)] ; i.e. '#vfx()
         [(flvector) (intersect/simple x flvector? 'flvector y)] ; i.e. '#vfl()
         [else
          'bottom])]))

  (define (intersect/record x y)
    (cond
      [(or (pred-$record/ref? x)
           (pred-$record/rtd? x))
       x]
      [(eq? x '$record)
       y]
      [else
       'bottom]))

  (define (predicate-intersect/record x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y '$record) x]
      [(eq? x '$record) y]
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
         [(pred-$record/ref? x)
          (let ([x-rtd (pred-$record/ref-maybe-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (if (and x-rtd (rtd-obviously-incompatible? x-rtd y-rtd))
                'bottom
                (intersect/record x y)))]
         [else
          (intersect/record x y)])]
      [(pred-$record/ref? y)
       (let ([y-rtd (pred-$record/ref-maybe-rtd y)]
             [x-rtd (pred-$record-maybe-rtd x)])
         (if (and x-rtd y-rtd (rtd-obviously-incompatible? x-rtd y-rtd))
             'bottom
             (intersect/record x y)))]
      [else
       (case y
         [($record)
          (intersect/record x y)]
         [else
          'bottom])]))

  (define (predicate-substract/immediate x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? y 'bottom) x]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x)
                    (constant-value y))
              'bottom
              x)]
         [(pred-immediate? y)
          (let ([my (pred-immediate-mask y)]
                [mx (immediate-rec->mask x #t)])
            (if (not (fx= (fxand mx my) 0))
                'bottom
                x))]
         [else
          ($oops 'predicate-substract/immediate "invalid expression ~s" y)])]
      [(pred-immediate? x)
       (let ([mx (pred-immediate-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (immediate-rec->mask y #f)])
              (build-pred-immediate (fxand mx (fxnot my)) x #f))]
           [(pred-immediate? y)
            (let ([my (pred-immediate-mask y)])
              (build-pred-immediate (fxand mx (fxnot my)) x #f))]
           [else
            ($oops 'predicate-substract/immediate "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-substract/immediate "invalid expression ~s" x)]))

  (define (predicate-substract/normal x y)
    (cond
      [(eq? x y) 'bottom]
      [(predicate-implies?/normal x y) 'bottom]
      [(eq? x 'exact-integer)
       (case y
         [(fixnum) 'bignum]
         [(bignum) 'fixnum]
         [else x])]
      [else x]))

  (define (predicate-substract/record x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y '$record) 'bottom]
      [(pred-$record/rtd? x)
       (if (and (pred-$record/rtd? y)
                (rtd-ancestor*? (pred-$record/rtd-rtd y)
                                (pred-$record/rtd-rtd x)))
           'bottom
           x)]
      [(pred-$record/ref? x)
       (if (and (pred-$record/ref? y)
                (eq? (pred-$record/ref-ref x)
                     (pred-$record/ref-ref y)))
          'bottom
          x)]
      [else x]))

  (define (predicate-implies? x y)
    (eq? (predicate-union x y) y))

  (define (predicate-implies?/normal x y)
    (eq? (predicate-union/normal x y) y))

  (define (predicate-disjoint? x y)
    (eq? (predicate-intersect x y) 'bottom))

  (define (predicate->class x)
    (cond
      #;[(eq? x 'bottom) 'bottom]
      [(or (check-constant-is? x $immediate?)
           (pred-immediate? x))
       'immediateC]
      [(or (eq? x '$record)
           (pred-$record/rtd? x)
           (pred-$record/ref? x))
       '$record]
      [else
       'normalptr]))

  (define build-pred-or
    (case-lambda
      [(i n r)
       (build-pred-or i n r #f #f)]
      [(i n r x)
       (build-pred-or i n r x #f)]
      [(i n r x y)
       (cond
         [(and x
               (eq? (pred-or-imm x) i)
               (eq? (pred-or-nor x) n)
               (eq? (pred-or-rec x) r))
          x]
         [(and y
               (eq? (pred-or-imm y) i)
               (eq? (pred-or-nor y) n)
               (eq? (pred-or-rec y) r))
          y]
         [(eq? i 'bottom)
          (cond
            [(eq? n 'bottom) r]
            [(eq? r 'bottom) n]
            [else (make-pred-or i n r)])]
         [else
          (cond
            [(and (eq? n 'bottom) (eq? r 'bottom)) i]
            [else (make-pred-or i n r)])])]))
  
  ;If x and y are equivalent, they result must be eq? to y
  ;so it's easy to test in predicate-implies?.
  ;The result may be bigger than the actual union. 
  (define (predicate-union x y)
    (cond
      [(or (not x) (not y)) #f]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define i (predicate-union/immediate (pred-or-imm x) (pred-or-imm y)))
         (define n (predicate-union/normal (pred-or-nor x) (pred-or-nor y)))
         (define r (predicate-union/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or i n r y x))]
      [(pred-or? x)
       (case (predicate->class y)
         [(immediateC)
          (build-pred-or (predicate-union/immediate (pred-or-imm x) y)
                         (pred-or-nor x)
                         (pred-or-rec x)
                         x)]
         [(normalptr)
          (build-pred-or (pred-or-imm x)
                         (predicate-union/normal (pred-or-nor x) y)
                         (pred-or-rec x)
                         x)]
         [($record)
          (build-pred-or (pred-or-imm x)
                         (pred-or-nor x)
                         (predicate-union/record (pred-or-rec x) y)
                         x)])]
      [(pred-or? y)
       (case (predicate->class x)
         [(immediateC)
          (build-pred-or (predicate-union/immediate x (pred-or-imm y))
                         (pred-or-nor y)
                         (pred-or-rec y)
                         y)]
         [(normalptr)
          (build-pred-or (pred-or-imm y)
                         (predicate-union/normal x (pred-or-nor y))
                         (pred-or-rec y)
                         y)]
         [($record)
          (build-pred-or (pred-or-imm y)
                         (pred-or-nor y)
                         (predicate-union/record x (pred-or-rec y))
                         y)])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(eq? cx cy)
            (case cx
              [(immediateC)
               (predicate-union/immediate x y)]
              [(normalptr)
               (predicate-union/normal x y)]
              [($record)
               (predicate-union/record x y)])]
           [else
            (let ()
              (define i (cond
                          [(eq? cx 'immediateC) x]
                          [(eq? cy 'immediateC) y]
                          [else 'bottom]))
              (define n (cond
                          [(eq? cx 'normalptr) x]
                          [(eq? cy 'normalptr) y]
                          [else 'bottom]))
              (define r (cond
                          [(eq? cx '$record) x]
                          [(eq? cy '$record) y]
                          [else 'bottom]))
              (build-pred-or i n r))]))]))

  ;The result may be bigger than the actual intersection 
  ;if there is no exact result, it must be at least included in x
  ;so it's possible to make decreasing sequences.
  ;Anyway, for now the result is exact.
  (define (predicate-intersect x y)
    (cond
      [(not x) y]
      [(not y) x]
      [(or (eq? x 'bottom)
           (eq? y 'bottom))
       'bottom]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define i (predicate-intersect/immediate (pred-or-imm x) (pred-or-imm y)))
         (define n (predicate-intersect/normal (pred-or-nor x) (pred-or-nor y)))
         (define r (predicate-intersect/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or i n r x y))]
      [(pred-or? x)
       (case (predicate->class y)
         [(immediateC)
          (predicate-intersect/immediate (pred-or-imm x) y)]
         [(normalptr)
          (predicate-intersect/normal (pred-or-nor x) y)]
         [($record)
          (predicate-intersect/record (pred-or-rec x) y)])]
      [(pred-or? y)
       (case (predicate->class x)
         [(immediateC)
          (predicate-intersect/immediate x (pred-or-imm y))]
         [(normalptr)
          (predicate-intersect/normal x (pred-or-nor y))]
         [($record)
          (predicate-intersect/record x (pred-or-rec y))])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(not (eq? cx cy))
            'bottom]
           [else
            (case cx
              [(immediateC)
               (predicate-intersect/immediate x y)]
              [(normalptr)
               (predicate-intersect/normal x y)]
              [($record)
               (predicate-intersect/record x y)])]))]))

  ;The result may be bigger than the actual intersection 
  ;if there is no exact result.
  ;Anyway, it must be included in x, 
  ;and in many cases, for now the result is just x.
  (define (predicate-substract x y)
    (cond
      [(not x) x]
      [(not y) x]
      [(eq? x 'bottom)
       'bottom]
      [(eq? y 'bottom)
       x]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define i (predicate-substract/immediate (pred-or-imm x) (pred-or-imm y)))
         (define n (predicate-substract/normal (pred-or-nor x) (pred-or-nor y)))
         (define r (predicate-substract/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or i n r x))]
      [(pred-or? x)
       (let ([i (pred-or-imm x)]
             [n (pred-or-nor x)]
             [r (pred-or-rec x)])
         (case (predicate->class y)
           [(immediateC)
            (build-pred-or (predicate-substract/immediate i y) n r x)]
           [(normalptr)
            (build-pred-or i (predicate-substract/normal n y) r x)]
           [($record)
            (build-pred-or i n (predicate-substract/record r y) x)]))]
      [(pred-or? y)
       (case (predicate->class x)
         [(immediateC)
          (predicate-substract/immediate x (pred-or-imm y))]
         [(normalptr)
          (predicate-substract/normal x (pred-or-nor y))]
         [($record)
          (predicate-substract/record x (pred-or-rec y))])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(not (eq? cx cy))
            x]
           [else
            (case cx
              [(immediateC)
               (predicate-substract/immediate x y)]
              [(normalptr)
               (predicate-substract/normal x y)]
              [($record)
               (predicate-substract/record x y)])]))]))
)
