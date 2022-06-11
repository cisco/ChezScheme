;;; Copyright (c) 2002 Oscar Waddell and R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Last modified: October 2010

;;; Acknowledgements
;;;  - Michael Lenaghan of frogware, Inc., contributed to the define-class
;;;    interface handling code.

;;; Possible names:
;;;   Chez Scheme OOP System (Chez SOOP)

#|
define-class:
  definition -> (define-class (class-name class-formal*)
                              (base-name base-actual*)
                  clause*)
  clause -> (implements interface*)
          | (ivars ivar*)
          | (init init-expr*)
          | (methods method*)
          | (constructor id)
          | (predicate id)
          | (prefix string)

  ivar -> (modifier* ivar-name ivar-expression)
  modifier -> mutability | visibility
  mutability -> mutable | immutable
  visibility -> public | private
  method -> (method-name formals method-body+)
  formals -> (var*) | var | (var+ . var)

notes:
  - at most one of each kind of clause may be present
  - at most one of each kind of modifier may be present
  - multiple methods of the same name but different formals can be present

products:
  - class-name is bound to class information in the expand-time environment
  - make-class-name (or specified constructor name) is bound to creation procedure
  - class-name? (or specified predicate name) is bound to predicate procedure
  - new (not inherited) method names are bound to method-dispatch procedures
  - for each public ivar, <prefix>-ivar is bound to an accessor procedure,
    where <prefix> is the specified prefix or "class-name-"
  - for each public, mutable ivar, <prefix>-ivar-set! is bound to a mutator
    procedure, where <prefix> is the specified prefix or "class-name-"

define-interface:
  definition -> (define-interface interface-name method*)
              | (define-interface interface-name base-name method*)
  
  method -> (method-name formals)

products:
  - interface-name is bound to interface information in the expand-time environment
  - new (not inherited) method names are bound to method-dispatch procedures
|#

#| Todo:
  - try using record-constructor, record-accessor, and record-mutator!
    instead of $record, $object-ref, and $object-set!
  - add a nongenerative clause ala define-record-type
    - squawk if any methods or interfaces
    - default to generative
  - need more tests:
    - init-expr checks
    - profile to see if all paths are covered by test suite
  - consider: don't degenerate to record definition facility
    - flush implicit nongenerative option
    - flush visibility keywords
    - flush getter/setter
    => less complex but less useful
  - consider: alternative version
    - <class-name> and method definitions are only products
    - (make-class-name arg ...) replaced by (make <class-name> arg ...)
    - (class-name? arg) replaced by (isa? <class-name> arg)
    - accessors and mutators replaced by
      (open-instance <class-name> (ivar ...) expr expr ...)
    - upside: prettier, less namespace clutter
    - downside: can't use module system directly to control visibility of
      makers, predicates, accessors, and mutators separately
    - downside: maker, predicate, accessors, mutators aren't
      first-class procedures
  - consider: inherited ivars
    - (inherited-ivar ivar ...)
    - inheritable / noninheritable ivar modifiers
  - (frogware) Error messages for incorrect syntax should be better. (You often
    get a generic "invalid syntax" message and have to hunt to figure
    out what caused it.)
  - consider exposing query-interface somehow
    - (interface I x) => version of x to which I's methods are applicable
    - (interface->instance (interface I x)) => x
  - interfaces and inheritance
    - either require parent for define-interface or hide <root-interface>
    - consider allowing multiple inheritance (including zero parents) for interfaces
|#

#|
reaching into Chez Scheme's internals for:
  #!base-rtd
  $make-record-type
  $record-type-field-offsets
  $record-type-interfaces
  $record
  $object-ref
  $object-set!
|#

#!chezscheme

(library (oop helpers)
  (export
    $make-class $class?
    $class-formals $class-formal-bindings $class-ivar-bindings $class-minfos
    $class-vtable-rtd $class-ctrtd
    $class-vtable-expr $class-interfaces $class-init-proc

    $make-interface $interface?
    $interface-rtd $interface-minfos

    $instance $make-instance $instance?

    root-vtable-rtd

    make-minfo minfo-mname minfo-hidden-mname minfo-arity minfo-formals minfo-flat-formals

    construct-name parse-formals build-generic make-ivar-defn free-id-member
  )

  (import (chezscheme))

  (define-record-type ($class $make-class $class?)
    (nongenerative)
    (fields
      (immutable formals)         ; (formal ...)
      (immutable formal-bindings) ; (((base-formal base-arg) ...) ...)
      (immutable ivar-bindings)   ; (((ivar init) ...) ...)
      (immutable minfos)          ; ((mname hidden-mname arity) ...)
                                  ; same mname may appear more than once

      (immutable vtable-rtd)
      (immutable ctrtd)

      (immutable vtable-expr)
      (immutable interfaces)
      (immutable init-proc)))

  (define-record-type ($interface $make-interface $interface?)
    (nongenerative)
    (fields
      (immutable rtd)
      (immutable minfos)))

  (define-record-type ($instance $make-instance $instance?)
    (nongenerative))

 ;; minfos cannot be records since we insert minfos into the output of
 ;; define-class and define-interface and the identifiers contained within
 ;; these minfos must be marked/unmarked as appropriate by the expander,
 ;; which delves into vectors but not records
  (define make-minfo
    (lambda (mname hidden-name arity formals flat-formals)
      (vector mname hidden-name arity formals flat-formals)))
  (define minfo-mname (lambda (x) (vector-ref x 0)))
  (define minfo-hidden-mname (lambda (x) (vector-ref x 1)))
  (define minfo-arity (lambda (x) (vector-ref x 2)))
  (define minfo-formals (lambda (x) (vector-ref x 3)))
  (define minfo-flat-formals (lambda (x) (vector-ref x 4)))

  (define root-vtable-rtd
    (#%$make-record-type #!base-rtd #!base-rtd
      "root-vtable-rtd"
      '((immutable ptr interfaces))
      #f
      #f))

  (define construct-name
    (lambda (template-identifier . args)
      (datum->syntax template-identifier
        (string->symbol
          (apply string-append
                 (map (lambda (x)
                        (if (string? x)
                            x
                            (symbol->string (syntax->datum x))))
                      args))))))

  (define parse-formals
    (lambda (fmls)
      (let f ([ids fmls] [n 0])
        (syntax-case ids ()
          [(car . cdr)
           (if (identifier? #'car)
               (f #'cdr (fx+ n 1))
               (syntax-error fmls "invalid method formals"))]
          [() (values n fmls)]
          [else
           (if (identifier? #'ids)
               (values
                 (fx- -1 n)
                 (let f ([ids fmls])
                   (syntax-case ids ()
                     [(x . d) (cons #'x (f #'d))]
                     [x #'(x)])))
               (syntax-error #'fmls "invalid method formals"))]))))

  (define build-generic
    (lambda (minfos offsets)
      (define cull-method
        (lambda (mname minfos offsets)
          (if (null? minfos)
              (values '() '() '() '())
              (let ([offset (car offsets)] [minfo (car minfos)])
                (let-values ([(gminfos goffsets new-minfos new-offsets)
                              (cull-method mname (cdr minfos) (cdr offsets))])
                  (if (bound-identifier=? (minfo-mname minfo) mname)
                      (values
                        (cons minfo gminfos)
                        (cons offset goffsets)
                        new-minfos
                        new-offsets)
                      (values
                        gminfos
                        goffsets
                        (cons (car minfos) new-minfos)
                        (cons (car offsets) new-offsets))))))))
      (if (null? minfos)
          '()
          (let ([generic-name (minfo-mname (car minfos))])
            (let-values ([(gminfos goffsets minfos offsets)
                          (cull-method generic-name minfos offsets)])
              (cons `(,generic-name
                      ,@(map (lambda (minfo offset)
                               `(,(minfo-formals minfo) ,(minfo-flat-formals minfo) ,offset))
                             gminfos
                             goffsets))
                    (build-generic minfos offsets)))))))

  (define (make-ivar-defn ivar mutable? ivar-offset)
    (with-syntax ([ivar ivar] [ivar-offset ivar-offset])
      (if mutable?
          #'(define-syntax ivar
              (identifier-syntax
                [id (#3%$object-ref 'scheme-object ego ivar-offset)]
                [(set! var val) (#3%$object-set! 'scheme-object ego ivar-offset val)]))
          #'(define-syntax ivar
              (make-variable-transformer
                (lambda (x)
                  (syntax-case x (set!)
                    [id (identifier? #'id) #'(#3%$object-ref 'scheme-object ego ivar-offset)]
                    [(set! var val) (syntax-error x "invalid assignment of immutable ivar")])))))))

  (define free-id-member
    (lambda (id ls2)
      (and (not (null? ls2))
           (if (free-identifier=? (car ls2) id)
               ls2
               (free-id-member id (cdr ls2))))))

  (record-writer (type-descriptor $instance)
    (lambda (x p wr)
      (fprintf p "#<instance of ~a>"
        (let ([rtd (record-rtd x)])
          (if (eq? rtd (type-descriptor $instance))
              "<root>"
              (record-type-name rtd))))))

 ; these aren't evaluated if placed in (oop), since (oop) has no
 ; run-time (variable) exports
  (pretty-format 'define-class
    '(_ (fill 0 x ...) 13 (fill 0 x ...) #f clause ...))
  (pretty-format 'methods
    '(_ #f (bracket x (fill 0 x ...) #f e ...) ...))
  (pretty-format 'ivars
    '(_ (bracket fill #f x ...) 6 (bracket fill #f x ...) ...))
  (pretty-format 'define-interface
    '(alt (_ var var #f (bracket x x) ...)
          (_ var #f (bracket x x) ...)))
)

;;; supplies define-class, define-interface, <root>, and <root-interface>
;;; and aux keywords

(library (oop)
  (export <root> <root-interface> define-interface
    define-class ivars public private methods self implements init
    constructor predicate prefix)
  (import (chezscheme) (oop helpers))

  (define-syntax <root>
    (make-compile-time-value
      ($make-class '() '() '() '()                         ; formals formal-bindings ivar-bindings minfos
        root-vtable-rtd
        (type-descriptor $instance)                        ; ctrtd
        #'(type-descriptor $instance)                      ; vtable-expr
        '()                                                ; interfaces
        #'values)))                                        ; init-expr ...

  (define-syntax <root-interface>
    (make-compile-time-value
      ($make-interface (make-record-type "base interface rtd" '()) '())))

  (define-syntax define-interface
    (lambda (x)
      (define build-minfo
        (lambda (mname formals)
          (let-values ([(arity flat-formals) (parse-formals formals)])
           ; discard source information
            (make-minfo mname "ignored" arity formals flat-formals))))
      (syntax-case x ()
        [(_ iname [method-name method-formals] ...)
         (and (identifier? #'iname) (andmap identifier? #'(method-name ...)))
         #'(define-interface iname <root-interface> [method-name method-formals] ...)]
        [(_ iname base-iname [method-name method-formals] ...)
         (and (identifier? #'iname) (identifier? #'base-iname) (andmap identifier? #'(method-name ...)))
         (lambda (r)
           (let ([bi (r #'base-iname)])
             (unless ($interface? bi)
               (syntax-error #'base-iname
                 "define-interface: unrecognized base interface"))
             (let ([base-mnames (with-syntax ([(#(base-mname base-hidden-name base-arity base-formals base-flat-formals) ...) ($interface-minfos bi)])
                                  #'(base-mname ...))])
               (let f ([ls #'(method-name ...)])
                 (unless (null? ls)
                   (when (free-id-member (car ls) base-mnames)
                     (syntax-error (car ls) "conflict with inherited interface method"))
                   (f (cdr ls)))))
             (with-syntax ([(base-minfo ...) ($interface-minfos bi)]
                           [(minfo ...) (map build-minfo #'(method-name ...) #'(method-formals ...))])
               (with-syntax ([iface-rtd
                              (make-record-type ($interface-rtd bi)
                                (symbol->string (syntax->datum #'iname))
                                (syntax->datum (map minfo-mname #'(minfo ...))))])
                 (with-syntax ([((generic-name (generic-formals generic-flat-formals generic-offset) ...) ...)
                                (build-generic
                                  #'(minfo ...)
                                  (let ([ls (#%$record-type-field-offsets #'iface-rtd)])
                                    (list-tail ls (- (length ls) (length #'(minfo ...))))))]
                               [opt3 (= (optimize-level) 3)])
                   #`(begin
                       (define-syntax iname (make-compile-time-value ($make-interface 'iface-rtd #'(base-minfo ... minfo ...))))
                       (define (qi who ego)
                         (define get-interfaces (#3%record-accessor '#,root-vtable-rtd 0))
                         (or (and (or opt3 (#3%record? ego))
                                  (let ([rtd (#3%record-rtd ego)])
                                    (and (or opt3 (#3%record? rtd '#,root-vtable-rtd))
                                      (#3%ormap (lambda (i) (and (#3%record? i 'iface-rtd) i))
                                        (get-interfaces rtd)))))
                             (errorf who "not applicable to ~s" ego)))
                       (define generic-name
                         (let ([who 'generic-name]) ; can't ref generic-name pattern vble inside ... below
                           (case-lambda
                             [(ego . generic-formals)
                              ((#3%$object-ref 'scheme-object (qi who ego) generic-offset)
                               ego . generic-flat-formals)]
                             ...)))
                       ...))))))])))

  (define-syntax define-class
    (lambda (x)
      (define build-minfo
        (lambda (mname formals hidden)
          (let-values ([(arity flat-formals) (parse-formals formals)])
            (make-minfo mname hidden arity formals flat-formals))))
      (define build-method
        (lambda (minfo class-name body base-minfos)
          (define cull-super
            (lambda (mname minfos)
              (if (null? minfos)
                  '()
                  (let ([minfo (car minfos)])
                    (if (free-identifier=? (minfo-mname minfo) mname)
                        (cons minfo (cull-super mname (cdr minfos)))
                        (cull-super mname (cdr minfos)))))))
          (with-syntax ([(formal ...) (minfo-flat-formals minfo)]
                        [body body]
                        [super-definition
                         (with-syntax ([super (datum->syntax class-name 'super)])
                           (if (not (null? base-minfos))
                               (with-syntax ([(#(mname hidden-name arity formals flat-formals) ...)
                                              (cull-super (minfo-mname minfo) base-minfos)])
                                 #'(define super (case-lambda [formals (hidden-name ego . flat-formals)] ...)))
                               (with-syntax ([mname (minfo-mname minfo)] [class-name class-name])
                                 #'(define-syntax super
                                     (lambda (x)
                                       (syntax-error x
                                         (format "no inherited ~s method for ~s in" 'mname 'class-name)))))))])
            #'(lambda (i formal ...)
                (fluid-let-syntax ([ego (identifier-syntax i)])
                  super-definition
                  body)))))
      (define unwrap-minfos
        (lambda (minfos)
         ; need list/vector structure, with arity unwrapped
          (with-syntax ([(#(mname hidden-name arity formals flat-formals) ...) minfos])
            (with-syntax ([(arity ...) (syntax->datum #'(arity ...))])
              #'(#(mname hidden-name arity formals flat-formals) ...)))))
      (define build-interface-vtable
        (lambda (minfos)
          (lambda (iface)
            (with-syntax
              ([irtd ($interface-rtd iface)]
               [(hidden ...)
                (map (lambda (iminfo)
                       (let ([mname (minfo-mname iminfo)] [arity (minfo-arity iminfo)])
                         (let f ([minfos minfos])
                           (cond
                             [(null? minfos) (syntax-error (minfo-mname iminfo) "no suitable implementation for interface method")]
                             [(minfo-match? (car minfos) mname arity) (minfo-hidden-mname (car minfos))]
                             [else (f (cdr minfos))]))))
                     (unwrap-minfos ($interface-minfos iface)))])
              #'(#3%$record 'irtd hidden ...)))))
      (define minfo-match?
        (lambda (minfo mname arity)
          (and (= arity (minfo-arity minfo))
               (free-identifier=? mname (minfo-mname minfo)))))
      (define process-methods
        (lambda (class-name interfaces all-base-minfos minfos bodies)
          (let f ([minfos minfos])
            (unless (null? minfos)
              (let ([mname (minfo-mname (car minfos))] [arity (minfo-arity (car minfos))])
                (let f ([ls (cdr minfos)])
                  (unless (null? ls)
                    (if (minfo-match? (car ls) mname arity)
                        (syntax-error (minfo-mname (car ls)) "duplicate arity for method")
                        (f (cdr ls))))))
              (f (cdr minfos))))
          (let ([base-mnames (map minfo-mname all-base-minfos)])
            (let f ([base-minfos all-base-minfos] [minfos minfos] [bodies bodies] [mlambdas '()] [all-minfos '()])
              (if (null? base-minfos)
                  (let f ([minfos minfos] [bodies bodies] [mlambdas mlambdas] [all-minfos all-minfos] [generics '()])
                    (if (null? minfos)
                        (begin
                         ; We should never create a new generic method that matches the name of
                         ; an interface method. (If the generic method made it this far then we
                         ; know it didn't match the arity of the interface method. If it did it
                         ; would have been filtered out.)
                          (for-each
                            (lambda (generic)
                              (let ([mname (minfo-mname generic)])
                                (for-each
                                  (lambda (i)
                                    (for-each
                                      (lambda (m)
                                        (let ([interface-mname (minfo-mname m)])
                                          (when (free-identifier=? mname interface-mname)
                                            (syntax-error mname "arity not supported by interface method"))))
                                      (unwrap-minfos ($interface-minfos i))))
                                  interfaces)))
                              generics)
                          (list (reverse all-minfos)
                                (reverse mlambdas)
                                (reverse generics)
                               ; We need to build a list of minfos that will actually be included in the class vtable--
                               ; ie, minfos that appear in this class and/or any super-class (but not in any interface).
                               ; That list will match up with the vtable offsets.
                                (let f ([possibly-included-minfos all-minfos] [included-minfos '()])
                                  (if (null? possibly-included-minfos)
                                      included-minfos
                                      (let ([minfo (car possibly-included-minfos)])
                                        (let ([mname (minfo-mname minfo)] [arity (minfo-arity minfo)])
                                          (f (cdr possibly-included-minfos)
                                             (if (ormap
                                                   (lambda (i)
                                                     (ormap (lambda (m) (minfo-match? m mname arity))
                                                            (unwrap-minfos ($interface-minfos i))))
                                                   interfaces)
                                                 included-minfos
                                                 (cons minfo included-minfos)))))))))
                        (let ([minfo (car minfos)])
                          (let ([mname (minfo-mname minfo)] [arity (minfo-arity minfo)])
                            (when (ormap (lambda (base-mname) (free-identifier=? mname base-mname)) base-mnames)
                              (syntax-error mname "arity not supported by base class method"))
                            (f (cdr minfos)
                               (cdr bodies)
                               (cons
                                 (list (minfo-hidden-mname minfo)
                                       (build-method minfo class-name (car bodies) '()))
                                 mlambdas)
                               (cons minfo all-minfos)
                               (if (ormap
                                     (lambda (i)
                                       (ormap (lambda (m) (minfo-match? m mname arity))
                                         (unwrap-minfos ($interface-minfos i))))
                                     interfaces)
                                   generics
                                   (cons minfo generics)))))))
                  (let ([base-minfo (car base-minfos)])
                    (let ([base-mname (minfo-mname base-minfo)] [base-arity (syntax->datum (minfo-arity base-minfo))])
                      (let find-method ([xminfos minfos] [xbodies bodies])
                        (cond
                          [(null? xminfos)
                           (f (cdr base-minfos) minfos bodies mlambdas
                              (cons base-minfo all-minfos))]
                          [(and (= base-arity (minfo-arity (car xminfos)))
                                (free-identifier=? base-mname (minfo-mname (car xminfos))))
                           (f (cdr base-minfos)
                              (remq (car xminfos) minfos)
                              (remq (car xbodies) bodies)
                              (cons (list (minfo-hidden-mname (car xminfos))
                                          (build-method (car xminfos) class-name (car xbodies) all-base-minfos))
                                    mlambdas)
                              (cons (car xminfos) all-minfos))]
                          [else (find-method (cdr xminfos) (cdr xbodies))])))))))))
      (define free-id-union
        (lambda (ls1 ls2)
          (if (null? ls1)
              ls2
              (if (free-id-member (car ls1) ls2)
                  (free-id-union (cdr ls1) ls2)
                  (cons (car ls1) (free-id-union (cdr ls1) ls2))))))

      (module (parse-clauses)
        (define-syntax define-option-parser
          (lambda (x)
            (syntax-case x (else)
              [(_ name [desc (kwd ...) ([var default] ...) pattern guard expr] ...)
               (with-syntax ([(seen? ...) (generate-temporaries #'(desc ...))])
                 #'(begin
                     (define (option-parser x*)
                       (let f ([x* x*] [seen? #f] ... [var default] ... ...)
                          (if (null? x*)
                              (values var ... ...)
                              (syntax-case (car x*) (kwd ... ...)
                                [pattern
                                 guard
                                 (begin
                                   (when seen?
                                     (syntax-error (car x*)
                                       (format "extra ~a" desc)))
                                   (let ([seen? #t])
                                     (let-values ([(var ...) expr])
                                       (f (cdr x*) seen? ... var ... ...))))]
                                ...))))
                     (define-syntax name
                       (lambda (x)
                         (syntax-case x ()
                           [(k x* b1 b2 (... ...))
                            (with-implicit (k var ... ...)
                              #'(let-values ([(var ... ...) (option-parser x*)])
                                  b1 b2 (... ...)))])))))])))
        (define (valid-type? x) (eq? x 'scheme-object))
        (define-option-parser parse-modifiers
          ["ivar mutability modifier" () ([mutable? #t])
           x
           (or (literal-identifier=? #'x #'mutable) (literal-identifier=? #'x #'immutable))
           (literal-identifier=? #'x #'mutable)]
          ["ivar public/private modifier" () ([public? #f])
           x
           (or (literal-identifier=? #'x #'public) (literal-identifier=? #'x #'private))
           (literal-identifier=? #'x #'public)]
          ["ivar type" () ([type 'scheme-object])
           x
           (valid-type? (datum x))
           (datum x)]
          ["" () () x #t (syntax-error #'x "invalid ivar modifier")])
        (define-option-parser parse-clauses
          ["implements clause" (implements) ([iface* '()])
           (implements iface ...)
           (andmap identifier? (syntax->list #'(iface ...)))
           (syntax->list #'(iface ...))]
          ["ivars clause" (ivars) ([public?* '()] [mutable?* '()] [ivar* '()] [ivar-init* '()])
           (ivars (modifier ... ivar ivar-init) ...)
           (andmap identifier? (syntax->list #'(ivar ...)))
           (let-values ([(public?* mutable?*)
                         (let f ([modifier** (syntax->list #'((modifier ...) ...))])
                           (if (null? modifier**)
                               (values '() '())
                               (let-values ([(public?* mutable?*) (f (cdr modifier**))])
                                 (parse-modifiers (syntax->list (car modifier**))
                                   (values (cons public? public?*)
                                           (cons mutable? mutable?*))))))])
             (values public?* mutable?* (syntax->list #'(ivar ...)) (syntax->list #'(ivar-init ...))))]
          ["init clause" (init) ([init-expr* '()])
           (init init-expr ...)
           #t
           (syntax->list #'(init-expr ...))]
          ["methods clause" (methods) ([method-name* '()] [method-formals* '()] [method-body* '()])
           (methods method ...)
           #t
           (let f ([method* (syntax->list #'(method ...))])
             (if (null? method*)
                 (values  '() '() '())
                 (let-values ([(method) (car method*)]
                              [(method-name* method-formals* method-body*) (f (cdr method*))])
                   (syntax-case method ()
                     [(method-name method-formals method-b1 method-b2 ...)
                      (values
                        (cons #'method-name method-name*)
                        (cons #'method-formals method-formals*)
                        (cons #'(let () method-b1 method-b2 ...) method-body*))]
                     [method (syntax-error #'method "invalid method syntax")]))))]
          ["constructor clause" (constructor) ([constructor-id #f])
           (constructor id)
           (identifier? #'id)
           #'id]
          ["predicate clause" (predicate) ([predicate-id #f])
           (predicate id)
           (identifier? #'id)
           #'id]
          ["prefix clause" (prefix) ([prefix-string #f])
           (prefix str)
           (string? (datum str))
           (datum str)]
          ["" () () x #t (syntax-error #'x "invalid define-class clause")]))

      (syntax-case x ()
        [(_ (class-name class-formal ...) (base-name base-arg ...) clause ...)
         (parse-clauses (syntax->list #'(clause ...))
           (lambda (r)
             (let ([bc (r #'base-name)])
               (unless ($class? bc)
                 (syntax-error #'base-name
                   "define-class: unrecognized base class"))
               (let f ([ls ivar*])
                 (define bound-id-member?
                   (lambda (x ls)
                     (and (not (null? ls))
                          (or (bound-identifier=? (car ls) x)
                              (bound-id-member? x (cdr ls))))))
                 (unless (null? ls)
                   (if (bound-id-member? (car ls) (cdr ls))
                       (syntax-error (car ls) "duplicate instance variable")
                       (f (cdr ls)))))
               (with-syntax ([(iface-name ...) (free-id-union
                                                 iface*
                                                 (syntax->list ($class-interfaces bc)))])
                 (with-syntax ([(interface ...)
                                (map (lambda (x)
                                       (let ([iface (r x)])
                                         (unless ($interface? iface) (syntax-error x "unrecognized interface"))
                                         iface))
                                     #'(iface-name ...))])
                   (with-syntax ([(((base-ivar base-ivar-init) ...) ...) ($class-ivar-bindings bc)]
                                 [base-init-proc ($class-init-proc bc)]
                                 [(base-formal-binding ...)
                                  (with-syntax ([(base-formal ...) ($class-formals bc)]
                                                [(base-base-formal-binding ...) ($class-formal-bindings bc)])
                                    (unless (= (length #'(base-arg ...)) (length #'(base-formal ...)))
                                      (syntax-error #'base-name
                                        "incorrect number of arguments to base class"))
                                    #'(((base-formal base-arg) ...) base-base-formal-binding ...))]
                                 [((all-minfo ...) ((new-hidden-mname mlambda) ...) (generic ...) (included-minfo ...))
                                  (process-methods #'class-name #'(interface ...)
                                    (unwrap-minfos ($class-minfos bc))
                                    (map build-minfo
                                      method-name*
                                      method-formals*
                                      (generate-temporaries method-name*))
                                    method-body*)]
                                 [self (datum->syntax #'class-name 'self)])
                     (let ([name (let ([name (datum class-name)])
                                   (if (gensym? name)
                                       name
                                       (symbol->string name)))]
                           [nongenerative? (and (null? #'(all-minfo ...))
                                                (null? #'(interface ...)))]
                           [flds (map list
                                      (map (lambda (x) (if x 'mutable 'immutable)) mutable?*)
                                      (map (lambda (x) 'ptr) ivar*)
                                      (syntax->datum ivar*))])
                       (when (gensym? name)
                         (unless nongenerative?
                           (syntax-error #'class-name
                             "cannot specify gensym class-name with methods or interfaces")))
                       (with-syntax ([ctrtd (#%$make-record-type
                                              #!base-rtd
                                              ($class-ctrtd bc)
                                              name
                                              flds
                                              #f
                                              #f)]
                                     [vtable-rtd (#%$make-record-type
                                                   #!base-rtd
                                                   ($class-vtable-rtd bc)
                                                   "compile-time-vtable-rtd"
                                                   (syntax->datum (map minfo-mname #'(generic ...)))
                                                   #f
                                                   #f)])
                         (with-syntax ([(ivar ...) ivar*]
                                       [(ivar-init ...) ivar-init*]
                                       [(ivar-defn ...)
                                        (map make-ivar-defn
                                             ivar*
                                             mutable?*
                                             (let ([offsets (#%$record-type-field-offsets #'ctrtd)])
                                               (list-tail offsets (- (length offsets) (length ivar*)))))])
                           (with-syntax ([init-proc
                                          (with-syntax ([(init-expr ...) init-expr*])
                                            #'(lambda (ego)
                                                (base-init-proc ego)
                                                (let ()
                                                  ivar-defn
                                                  ...
                                                  (define-syntax self (identifier-syntax ego))
                                                  init-expr ...
                                                  ego)))]
                                         [vtable-expr
                                          (if nongenerative?
                                              #''ctrtd
                                              (with-syntax ([(vtable-init ...) (map minfo-hidden-mname #'(included-minfo ...))]
                                                            [parent-rtd ($class-vtable-expr bc)]
                                                            [name (datum->syntax #'class-name name)]
                                                            [flds (datum->syntax #'class-name flds)]
                                                            [(iface-elt ...)
                                                             (map (build-interface-vtable #'(all-minfo ...)) #'(interface ...))])
                                                #'(#%$make-record-type
                                                    'vtable-rtd
                                                    parent-rtd
                                                    name
                                                    'flds
                                                    #f
                                                    #f
                                                    (list iface-elt ...)
                                                    vtable-init ...)))]
                                         [((generic-name (generic-formals generic-flat-formals generic-offset) ...) ...)
                                          (build-generic
                                            #'(generic ...)
                                            (let ([offsets (#%$record-type-field-offsets #'vtable-rtd)] [minfos #'(included-minfo ...)])
                                              (let f ([offsets (list-tail offsets (- (length offsets) (length minfos)))]
                                                      [minfos minfos]
                                                      [generics #'(generic ...)])
                                                (if (null? generics)
                                                    '()
                                                    (if (eq? (car generics) (car minfos))
                                                        (cons (car offsets) (f (cdr offsets) (cdr minfos) (cdr generics)))
                                                        (f (cdr offsets) (cdr minfos) generics))))))]
                                         [opt3 (= (optimize-level) 3)])
                             (with-syntax ([maker-name (or constructor-id (construct-name #'class-name "make-" #'class-name))]
                                           [maker-body
                                            (let f ([ls #'(base-formal-binding ...)])
                                              (syntax-case ls ()
                                                [(((lhs rhs) ...))
                                                 #'(let ((lhs rhs) ...)
                                                     (init-proc (let* ([base-ivar base-ivar-init] ... ... [ivar ivar-init] ...)
                                                                  (#3%$record vtable base-ivar ... ... ivar ...))))]
                                                [(((lhs rhs) ...) m ...)
                                                 (with-syntax ([body (f #'(m ...))])
                                                   #'(let ((lhs rhs) ...) body))]))]
                                           [pred-name (or predicate-id (construct-name #'class-name #'class-name "?"))]
                                           [((accessor-name . accessor) ...)
                                            (let ([offset* (let ([offset* (#%$record-type-field-offsets #'ctrtd)])
                                                             (list-tail offset* (- (length offset*) (length ivar*))))]
                                                  [prefix-string (or prefix-string (format "~a-" (datum class-name)))])
                                              (let f ([ivar* ivar*] [offset* offset*] [public?* public?*])
                                                (if (null? ivar*)
                                                    '()
                                                    (with-syntax ([rest (f (cdr ivar*) (cdr offset*) (cdr public?*))])
                                                      (if (car public?*)
                                                          (with-syntax ([accessor-name (construct-name (car ivar*) prefix-string (car ivar*))]
                                                                        [offset (car offset*)])
                                                            #'((accessor-name .
                                                                (lambda (x)
                                                                  (unless (or opt3 (record? x vtable))
                                                                    (errorf 'accessor-name "not applicable to ~s" x))
                                                                  (#3%$object-ref 'scheme-object x offset)))
                                                               . rest))
                                                          #'rest)))))]
                                           [((mutator-name . mutator) ...)
                                            (let ([offset* (let ([offset* (#%$record-type-field-offsets #'ctrtd)])
                                                             (list-tail offset* (- (length offset*) (length ivar*))))]
                                                  [prefix-string (or prefix-string (format "~a-" (datum class-name)))])
                                              (let f ([ivar* ivar*] [offset* offset*] [public?* public?*] [mutable?* mutable?*])
                                                (if (null? ivar*)
                                                    '()
                                                    (with-syntax ([rest (f (cdr ivar*) (cdr offset*) (cdr public?*) (cdr mutable?*))])
                                                      (if (and (car public?*) (car mutable?*))
                                                          (with-syntax ([mutator-name (construct-name (car ivar*) prefix-string (car ivar*) "-set!")]
                                                                        [offset (car offset*)])
                                                            #'((mutator-name .
                                                                (lambda (x v)
                                                                  (unless (or opt3 (record? x vtable))
                                                                    (errorf 'mutator-name "not applicable to ~s" x))
                                                                  (#3%$object-set! 'scheme-object x offset v)))
                                                               . rest))
                                                          #'rest)))))])
                               #'(begin
                                  ; we use a module here (1) to attach necessary
                                  ; indirect exports to class-name in case class-name
                                  ; is exported from a top-level module and
                                  ; (2) so that we can define vtable after
                                  ; new-hidden-mname ... yet still have the introduced
                                  ; identifier vtable resolve to the correct binding
                                  ; in the method bodies.  we don't wrap the entire
                                  ; define-class output in a module form since we want
                                  ; the pred-name, maker-name, and the generic names
                                  ; to be ordinary top-level variables at top level.
                                   (module ((class-name vtable new-hidden-mname ...) vtable new-hidden-mname ...)
                                     (module (new-hidden-mname ...)
                                      ; local ego for fluid-let-syntax to whack
                                       (define-syntax ego values)
                                       ivar-defn
                                       ...
                                       (define-syntax self (identifier-syntax ego))
                                       (define new-hidden-mname mlambda) ...)
                                    ; counting on letrec* semantics below so that
                                    ; new-hidden-mname ... are defined before referenced
                                    ; in vtable-expr
                                     (define vtable vtable-expr)
                                     (define-syntax class-name
                                       (make-compile-time-value
                                         ($make-class
                                           #'(class-formal ...)
                                           #'(base-formal-binding ...)
                                           #'(([base-ivar base-ivar-init] ...) ... ([ivar ivar-init] ...))
                                           #'(all-minfo ...)
                                           'vtable-rtd
                                           'ctrtd
                                           #'vtable
                                           #'(iface-name ...)
                                           #'init-proc))))
                                   (define (pred-name x) (#3%record? x vtable))
                                   (define (maker-name class-formal ...) maker-body)
                                   (define accessor-name accessor) ...
                                   (define mutator-name mutator) ...
                                   (define generic-name
                                     (let ([who 'generic-name]) ; can't ref generic-name pattern vble inside ... below
                                       (case-lambda
                                         [(ego . generic-formals)
                                          (unless (or opt3 (#3%record? ego vtable))
                                            (errorf who "not applicable to ~s" ego))
                                          ((#3%$object-ref 'scheme-object (#3%record-rtd ego) generic-offset)
                                           ego . generic-flat-formals)]
                                         ...)))
                                   ...))))))))))))])))

  (define-syntax aux
    (syntax-rules ()
      [(_ kwd)
       (define-syntax kwd
         (lambda (x)
           (syntax-error x "misplaced aux keyword")))]))

  (aux ivars)
  (aux public)
  (aux private)
  (aux methods)
  (aux self)
  (aux implements)
  (aux init)
 ; constructor, predicate, and prefix are defined by (chezscheme)
  #;(aux constructor)
  #;(aux predicate)
  #;(aux prefix)
)
