(define-datatype #{fasl striprur0zx3-fasl}
  (#{entry striprur0zx3-0} situation fasl)
  (#{header striprur0zx3-1} version machine dependencies)
  (#{pair striprur0zx3-2} vfasl)
  (#{tuple striprur0zx3-3} ty vfasl)
  (#{string striprur0zx3-4} ty string)
  (#{gensym striprur0zx30-5} pname uname)
  (#{vector striprur0zx3-6} ty vfasl)
  (#{fxvector striprur0zx3-7} viptr)
  (#{bytevector striprur0zx3-9} ty bv)
  (#{stencil-vector striprur0zx0-sv} mask vfasl sys?)
  (#{record  striprur0zx3-10} maybe-uid size nflds rtd pad-ty* fld*) ; maybe-uid => rtd
  (#{rtd-ref striprur0zx3-11} uid) ; field info not recorded
  (#{closure striprur0zx3-12} offset c)
  (#{flonum striprur0zx3-13} high low)
  (#{small-integer striprur0zx3-14} iptr)
  (#{large-integer striprur0zx3-15} sign vuptr)
  (#{eq-hashtable striprur0zx3-16} mutable? subtype minlen veclen vpfasl)
  (#{symbol-hashtable striprur0zx3-17} mutable? minlen equiv veclen vpfasl)
  (#{code striprur0zx3-18} flags free name arity-mask info pinfo* bytes m vreloc)
  (#{atom striprur0zx3-19} ty uptr)
  (#{reloc striprur0zx3-20} type-etc code-offset item-offset fasl)
  (#{indirect striprur0zx3-21} g i))

(define-datatype #{field stripfur0zx3-field}
  (#{ptr stripfur0zx3-0} fasl)
  (#{byte stripfur0zx3-1} n)
  (#{iptr stripfur0zx3-2} n)
  (#{single stripfur0zx3-3} n)
  (#{double stripfur0zx3-4} high low))


;; cooperates better with auto-indent than `fasl-case`:
(define-syntax (fasl-case* stx)
  (syntax-case stx (else)
    [(_ target [(op fld ...) body ...] ... [else e-body ...])
     #'(fasl-case target [op (fld ...) body ...] ... [else e-body ...])]
    [(_ target [(op fld ...) body ...] ...)
     #'(fasl-case target [op (fld ...) body ...] ...)]))

;; reverse quoting convention compared to `constant-case`:
(define-syntax (constant-case* stx)
  (syntax-case stx (else)
    [(_ target [(const ...) body ...] ... [else e-body ...])
     (with-syntax ([((val ...) ...)
                    (map (lambda (consts)
                           (map (lambda (const)
                                  (lookup-constant const))
                                consts))
                         (datum ((const ...) ...)))])
       #'(case target [(val ...) body ...] ... [else e-body ...]))]
    [(_ target [(const ...) body ...] ...)
     #'(constant-case* target [(const ...) body ...] ... [else ($oops 'constant-case* "no matching case ~s" 'target)])]))
