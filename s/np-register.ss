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

(define-syntax define-reserved-registers
  (lambda (x)
    (syntax-case x ()
      [(_ [regid alias ... callee-save? mdinfo type] ...)
       (syntax-case #'(regid ...) (%tc %sfp) [(%tc %sfp . others) #t] [_ #f])
       #'(begin
           (begin
             (define-once regid (make-reg 'regid 'mdinfo (tc-disp regid) callee-save? 'type))
             (module (alias ...) (define x regid) (define alias x) ...))
           ...)])))

(define-syntax define-register-aliases
  (syntax-rules ()
    [(_ regid reg-alias ...) (begin (define reg-alias regid) ...)]))

(define-syntax define-allocable-registers
  (lambda (x)
    (assert (fx<= (constant asm-arg-reg-cnt) (constant asm-arg-reg-max)))
    (syntax-case x ()
      [(_ regvec arg-registers extra-registers extra-fpregisters make-reg-spillinfo
          [regid reg-alias ... callee-save? mdinfo type] ...)
       (with-syntax ([((tc-disp ...) (arg-regid ...) (extra-regid ...) (extra-fpregid ...))
                      (syntax-case #'([regid type] ...) (%ac0 %xp %ts %td uptr)
                        [([%ac0 _] [%xp _] [%ts _] [%td _] [other other-type] ...)
                         (let f ([other* #'(other ...)]
                                 [other-type* #'(other-type ...)]
                                 [rtc-disp* '()]
                                 [arg-offset (constant tc-arg-regs-disp)]
                                 [fp-offset (constant tc-fpregs-disp)]
                                 [rextra* '()]
                                 [rfpextra* '()])
                           (if (null? other*)
                               (cond
                                 [(not (fx= (length rextra*) (constant asm-arg-reg-max)))
                                  (syntax-error x (format "asm-arg-reg-max extra registers are not specified ~s" (syntax->datum rextra*)))]
                                 [(not (fx= (length rfpextra*) (constant asm-fpreg-max)))
                                  (syntax-error x (format "asm-fpreg-max extra registers are not specified ~s" (syntax->datum rfpextra*)))]
                                 [else
                                   (let ([extra* (reverse rextra*)]
                                         [fpextra* (reverse rfpextra*)])
                                     (list
                                       (list*
                                         (constant tc-ac0-disp)
                                         (constant tc-xp-disp)
                                         (constant tc-ts-disp)
                                         (constant tc-td-disp)
                                         (reverse rtc-disp*))
                                       (list-head extra* (constant asm-arg-reg-cnt))
                                       (list-tail extra* (constant asm-arg-reg-cnt))
                                       fpextra*))])
                               (let ([other (car other*)])
                                 (if (memq (syntax->datum other) '(%ac1 %yp %cp %ret))
                                     (f (cdr other*) (cdr other-type*) (cons #`(tc-disp #,other) rtc-disp*)
                                        arg-offset fp-offset rextra* rfpextra*)
                                     (if (eq? (syntax->datum (car other-type*)) 'fp)
                                         (f (cdr other*) (cdr other-type*) (cons fp-offset rtc-disp*)
                                            arg-offset (fx+ fp-offset (constant double-bytes)) rextra* (cons other rfpextra*))
                                         (f (cdr other*) (cdr other-type*) (cons arg-offset rtc-disp*)
                                            (fx+ arg-offset (constant ptr-bytes)) fp-offset (cons other rextra*) rfpextra*))))))]
                        [_ (syntax-error x "missing or out-of-order required registers")])]
                     [(reg-spillinfo-index ...) (iota (length #'(regid ...)))])
         #'(begin
             (define-once regid (let ([r (make-reg 'regid 'mdinfo tc-disp callee-save? 'type)])
                                  (var-spillinfo-redirect! r reg-spillinfo-index)
                                  r))
             ...
             (define-register-aliases regid reg-alias ...) ...
             (define regvec (vector regid ...))
             (define arg-registers (list arg-regid ...))
             (define extra-registers (list extra-regid ...))
             (define extra-fpregisters (list extra-fpregid ...))
             (define (make-reg-spillinfo)
               (vector (make-redirect-var 'regid)
                       ...))))])))

(define-syntax define-machine-dependent-registers
  (lambda (x)
    (syntax-case x ()
      [(_ [regid alias ... callee-save? mdinfo type] ...)
       #'(begin
           (begin
             (define-once regid (make-reg 'regid 'mdinfo #f callee-save? 'type))
             (module (alias ...) (define x regid) (define alias x) ...))
           ...)])))

(define-syntax define-registers
  (lambda (x)
    (syntax-case x (reserved allocable machine-dependent)
      [(k (reserved [rreg rreg-alias ... rreg-callee-save? rreg-mdinfo rreg-type] ...)
          (allocable [areg areg-alias ... areg-callee-save? areg-mdinfo areg-type] ...)
          (machine-depdendent [mdreg mdreg-alias ... mdreg-callee-save? mdreg-mdinfo mdreg-type] ...))
       (with-implicit (k regvec arg-registers extra-registers extra-fpregisters real-register? make-reg-spillinfo)
         #`(begin
             (define-reserved-registers [rreg rreg-alias ... rreg-callee-save? rreg-mdinfo rreg-type] ...)
             (define-allocable-registers regvec arg-registers extra-registers extra-fpregisters make-reg-spillinfo
               [areg areg-alias ... areg-callee-save? areg-mdinfo areg-type] ...)
             (define-machine-dependent-registers [mdreg mdreg-alias ... mdreg-callee-save? mdreg-mdinfo mdreg-type] ...)
             (define-syntax real-register?
               (with-syntax ([real-reg* #''(rreg ... rreg-alias ... ... areg ... areg-alias ... ... mdreg ... mdreg-alias ... ...)])
                 (syntax-rules ()
                   [(_ e) (memq e real-reg*)])))))])))

(architecture registers)

; pseudo register used for mref's with no actual index
(define-once %zero (make-reg 'zero #f #f #f #f))

;; define %ref-ret to be sfp[0] on machines w/no ret register
;;
;; The ret register, if any, is used to pass a return address to a
;; function. All functions currently stash the ret register in
;; sfp[0] and return to sfp[0] instead of the ret register, so the
;; register doesn't have to be saved and restored for non-tail
;; calls --- so use sfp[0] instead of the ret registerr to refer
;; to the current call's return address. (A leaf procedure could
;; do better, but doesn't currently.)
(define-syntax %ref-ret
  (lambda (x)
    (meta-cond
      [(real-register? '%ret) #'%ret]
      [else (with-syntax ([%mref (datum->syntax x '%mref)])
              #'(%mref ,%sfp 0))])))

(define-syntax reg-cons*
  (lambda (x)
    (syntax-case x ()
      [(_ ?reg ... ?reg*)
       (fold-right
         (lambda (reg reg*)
           (cond
             [(real-register? (syntax->datum reg))
              #`(cons #,reg #,reg*)]
             [else reg*]))
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
