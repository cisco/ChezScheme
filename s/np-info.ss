;; Records used by "cpnanopass.ss" and "cpprim.ss"

(define-record-type ctci ; compile-time version of code-info
  (nongenerative #{ctci bcpkdd2y9yyv643zicd4jbe3y-0})
  (sealed #t)
  (fields (mutable live) (mutable rpi*) (mutable closure-fv-names))
  (protocol
    (lambda (new)
      (lambda ()
        (new #f '() #f)))))

(define-record-type ctrpi ; compile-time version of rp-info
  (nongenerative #{ctrpi bcpkdd2y9yyv643zicd4jbe3y-1})
  (sealed #t)
  (fields label src sexpr mask))

(define-record-type info-lambda
  (nongenerative #{info-lambda bcpkdd2y9yyv643zicd4jbe3y-2})
  (parent info)
  (sealed #t)
  (fields src sexpr libspec (mutable interface*) (mutable dcl*) (mutable flags) (mutable fv*) (mutable name)
    (mutable well-known?) (mutable closure-rep) ctci (mutable pinfo*) seqno)
  (protocol
    (lambda (pargs->new)
      (rec cons-info-lambda
        (case-lambda
          [(src sexpr libspec interface*) (cons-info-lambda src sexpr libspec interface* #f 0)]
          [(src sexpr libspec interface* name) (cons-info-lambda src sexpr libspec interface* name 0)]
          [(src sexpr libspec interface* name flags)
           ((pargs->new) src sexpr libspec interface*
            (map (lambda (iface) (make-direct-call-label 'dcl)) interface*)
            (if (eq? (subset-mode) 'system) (fxlogor flags (constant code-flag-system)) flags)
            '() name #f 'closure (and (generate-inspector-information) (make-ctci)) '() ($np-next-lambda-seqno))])))))

(define-record-type info-call
  (nongenerative #{info-call bcpkdd2y9yyv643zicd4jbe3y-3})
  (parent info)
  (sealed #t)
  (fields src sexpr (mutable check?) pariah? error? shift-attachment? shift-consumer-attachment?*)
  (protocol
    (lambda (pargs->new)
      (case-lambda
       [(src sexpr check? pariah? error? shift-attachment? shift-consumer-attachment?*)
        ((pargs->new) src sexpr check? pariah? error? shift-attachment? shift-consumer-attachment?*)]
       [(src sexpr check? pariah? error?)
        ((pargs->new) src sexpr check? pariah? error? #f '())]))))

(define-record-type info-newframe
  (nongenerative #{info-newframe bcpkdd2y9yyv643zicd4jbe3y-4})
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

(define-record-type info-kill*
  (nongenerative #{info-kill* bcpkdd2y9yyv643zicd4jbe3y-5})
  (parent info)
  (fields kill*))

(define-record-type info-kill*-live*
  (nongenerative #{info-kill*-live* bcpkdd2y9yyv643zicd4jbe3y-6})
  (parent info-kill*)
  (fields live*)
  (protocol
    (lambda (new)
      (case-lambda
        [(kill* live*)
          ((new kill*) live*)]
        [(kill*)
          ((new kill*) (reg-list))]))))

(define-record-type info-asmlib
  (nongenerative #{info-asmlib bcpkdd2y9yyv643zicd4jbe3y-7})
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

(module (intrinsic-info-asmlib intrinsic-return-live* intrinsic-entry-live* intrinsic-modify-reg* dorest-intrinsics)
  (define-record-type intrinsic
    (nongenerative #{intrinsic bcpkdd2y9yyv643zicd4jbe3y-A})
    (sealed #t)
    (fields libspec kill* live* rv*))
  (define intrinsic-info-asmlib
    (lambda (intrinsic save-ra?)
      (make-info-asmlib (intrinsic-kill* intrinsic)
        (intrinsic-libspec intrinsic)
        save-ra?
        (intrinsic-live* intrinsic))))
  (define intrinsic-return-live*
    ; used a handful of times, just while compiling library.ss...don't bother optimizing
    (lambda (intrinsic)
      (fold-left (lambda (live* kill) (remq kill live*))
        (vector->list regvec) (intrinsic-kill* intrinsic))))
  (define intrinsic-entry-live*
    ; used a handful of times, just while compiling library.ss...don't bother optimizing
    (lambda (intrinsic) ; return-live* - rv + live*
      (fold-left (lambda (live* live) (if (memq live live*) live* (cons live live*)))
        (fold-left (lambda (live* rv) (remq rv live*))
          (intrinsic-return-live* intrinsic)
          (intrinsic-rv* intrinsic))
        (intrinsic-live* intrinsic))))
  (define intrinsic-modify-reg*
    (lambda (intrinsic)
      (append (intrinsic-rv* intrinsic)
              (intrinsic-kill* intrinsic))))
  (define-syntax declare-intrinsic
    (syntax-rules (unquote)
      [(_ name entry-name (kill ...) (live ...) (rv ...))
       (begin
         (define name
           (make-intrinsic
             (lookup-libspec entry-name)
             (reg-list kill ...)
             (reg-list live ...)
             (reg-list rv ...)))
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
  (declare-intrinsic reify-1cc reify-1cc (%xp %ac0 %ts %reify1 %reify2) () (%td)) ; %reify1 & %reify2 are defined as needed per machine...
  (declare-intrinsic maybe-reify-cc maybe-reify-cc (%xp %ac0 %ts %reify1 %reify2) () (%td)) ; ... to have enough registers to allocate
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
                              (reg-list %ac0 %xp %ts %td)
                              (reg-cons* %ac0 (list-xtail arg-registers #,i))
                              (let ([ls (list-xtail arg-registers #,i)]) (if (null? ls) '() (list (car ls)))))
                      (f (fx+ i 1))))))))
      dorests)))

(define-record-type info-alloc
  (nongenerative #{info-alloc bcpkdd2y9yyv643zicd4jbe3y-9})
  (parent info)
  (sealed #t)
  (fields tag save-flrv? save-ra?))

(define-record-type info-foreign
  (nongenerative #{info-foreign bcpkdd2y9yyv643zicd4jbe3y-10})
  (parent info)
  (sealed #t)
  (fields conv* arg-type* result-type unboxed? (mutable name))
  (protocol
    (lambda (pargs->new)
      (lambda (conv* arg-type* result-type unboxed?)
        ((pargs->new) conv* arg-type* result-type unboxed? #f)))))

(define-record-type info-literal
  (nongenerative #{info-literal bcpkdd2y9yyv643zicd4jbe3y-11})
  (parent info)
  (sealed #t)
  (fields indirect? type addr offset))

(define-record-type info-lea
  (nongenerative #{info-lea bcpkdd2y9yyv643zicd4jbe3y-12})
  (parent info)
  (sealed #t)
  (fields offset))

(define-record-type info-load
  (nongenerative #{info-load bcpkdd2y9yyv643zicd4jbe3y-13})
  (parent info)
  (sealed #t)
  (fields type swapped?))

(define-record-type info-condition-code
  (nongenerative #{info-condition-code bcpkdd2y9yyv643zicd4jbe3y-14})
  (parent info)
  (sealed #t)
  (fields type reversed? invertible?))

(define-record-type info-c-simple-call
  (nongenerative #{info-c-simple-call bcpkdd2y9yyv643zicd4jbe3y-15})
  (parent info-kill*-live*)
  (sealed #t)
  (fields save-ra? entry)
  (protocol
   (lambda (new)
     (case-lambda
      [(save-ra? entry) ((new '() '()) save-ra? entry)]
      [(live* save-ra? entry) ((new '() live*) save-ra? entry)]))))

(define-record-type info-c-return
  (nongenerative #{info-c-return bcpkdd2y9yyv643zicd4jbe3y-16})
  (parent info)
  (sealed #t)
  (fields offset))

(define-record-type info-inline
  (nongenerative #{info-inline bcpkdd2y9yyv643zicd4jbe3y-17})
  (parent info)
  (sealed #t)
  (fields))

(define-record-type info-unboxed-args
  (nongenerative #{info-unboxed-args bcpkdd2y9yyv643zicd4jbe3y-18})
  (parent info)
  (fields unboxed?*))
