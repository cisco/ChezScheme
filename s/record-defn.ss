;;; record-defn.ss
;;; Copyright 1984-2021 Cisco Systems, Inc.
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

;;; Much of this code originated in oop.ss, which is licensed as follows:

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

;;; Acknowledgements
;;;  - Michael Lenaghan of frogware, Inc., contributed to the
;;;    interface handling code.

#|
This version of define-record-type extends R6RS define-record-type
with support for object-oriented programming.  Specifically, it
allows methods to be declared (and possibly overridden by child
record types), and it allows interfaces to be defined, which supports
a form of multiple inheritance.  It also allows fields to be declared
private, i.e., visible only within the methods of the record type.

The following summarizes the extended syntax:

  definition -> (define-record-type <rtname> <clause> ...)
                (define-record-type (<rtname> <maker-name> <predicate-name>) <clause> ...)

  A clause can be one of the define-record-type clauses, an implements clause,
  or a methods clause.

  <clause> -> (fields <field-spec>*)
              (parent <rtname>)
              (nongenerative) | (nongenerative <uid>) | (nongenerative #f)
              (sealed #f) | (sealed #t)
              (opaque #f) | (opaque #t)
              (protocol <expression>)
              (parent-rtd <rtd-expression> <rcd-expression>)
              (implements <interface-name>*)
              (methods <method-spec>*)

  A field spec is similar to a define-record-type field spec, except that a
  define-record-type field spec supports public and private modifiers.  accessors
  and mutators are defined for public fields but not for private fields (even if
  accessors and/or mutators are explicitly specified in the field syntax).
  fields are public and immutable by default for compatibility with
  define-record-type.

  <field-spec> -> <field-name>
                  (<visibility> <field-name>)
                  (<visibility> <field-detail>)
                  (<field-detail>)
  <visibility> -> public | private
  <field-detail> -> immutable <field-name>
                    mutable <field-name>
                    immutable <field-name> <accessor-name>
                    mutable <field-name> <accessor-name> <mutator-name>

  <rtd-expression> -> <expression>
  <rcd-expression> -> <expression>

  <method-spec> -> (<method-name> <formals> <body>)

  <rtname> -> <identifier>
  <field-name> -> <identifier>
  <uid> -> <identifier>
  <interface-name> -> <identifier>
  <method-name> -> <identifier>
  <formals> -> (<identifier>*) | <identifier> | (<identifier>+ . <identifier>)

restrictions:
  - at most one of each kind of clause may be present
  - multiple methods of the same name but different arities can be present
  - a record type with methods (including inherited methods) or implementing
    interfaces cannot be nongenerative

products:
  - The specified rtname NAME is bound to record-type information in the expand-time environment
  - make-NAME (or other specified constructor name) is bound to a creation procedure
  - NAME? (or other specified predicate name) is bound to a predicate procedure
  - NAME-FIELD and, for mutable fields, NAME-FIELD-SET! (or other specified accessor and
    mutator names) are bound to accessor (and mutator) procedures for each new (not
    inherited) public field FIELD.
  - new (not inherited) method names are bound to method-dispatch procedures

define-interface:
  definition -> (define-interface <interface-name> <interface-clause> ...)
                (define-interface (<interface-name> <predicate-name>) <interface-clause> ...)

  <interface-clause> -> (parent <interface-name>)
                        (methods <interface-method-spec>*)

  <interface-method-spec> -> (<method-name> <formals>)

  <interface-name> -> <identifier>
  <predicate-name> -> <identifier>

products:
  - the specified interface-name NAME is bound to interface information in the expand-time environment
  - NAME? (or other specified predicate name) is bound to a predicate procedure
  - new (not inherited) method names are bound to method-dispatch procedures

open-interface:
  definition -> (open-interface <interface-name> (<method-binding> ...) <object-expression>)

  <method-binding> -> (<local-method-name> <interface-method-name>)

  <object-expression> -> <expression>

  <local-method-name> -> <identifier>
  <interface-method-name> -> <identifier>

products:
  - each specified local method name is bound to the method named by the corresponding
    interface-method-name in the specified interface, specialized to the object that
    results from evaluating the given object expression.  The specialized method takes
    the same arguments as the original method except for the leading object argument.
|#

(define require-nongenerative-clause
  ($make-thread-parameter #f
    (lambda (x) (and x #t))))

(let ()
 ;; vector-records are used where we cannot use actual records, i.e., for objects
 ;; inserted into macro output and containing identifiers that must be marked/unmarked
 ;; as appropriate by the expander, which delves into vectors but not records
  (define-syntax define-vector-record
    (lambda (x)
      (syntax-case x ()
        [(_ (maker pred) accessor ...)
         (let ([n (length #'(accessor ...))])
           (with-syntax ([(offset ...) (iota n)] [n n])
             #'(begin
                 (define maker (lambda (accessor ...) (vector accessor ...)))
                 (define pred (lambda (x) (and (vector? x) (eq? (vector-length x) n))))
                 (define accessor (lambda (x) (vector-ref x offset))) ...)))])))

  (module ($make-drtinfo $drtinfo? $drtinfo-minfos $drtinfo-base-rtd $drtinfo-maybe-ctrtd $drtinfo-maybe-rtd $drtinfo-maybe-rcd
           $drtinfo-rtd-expr $drtinfo-rcd-expr $drtinfo-interface-names $drtinfo-sealed?  $drtinfo-protocol?
           unwrap-drtinfo)
    (define-vector-record ($$make-drtinfo $$drtinfo?)
      $drtinfo-uid
      $drtinfo-base-rtd
      $drtinfo-maybe-ctrtd
      $drtinfo-maybe-rtd
      $drtinfo-maybe-rcd
      $drtinfo-rtd-expr
      $drtinfo-rcd-expr
      $drtinfo-interface-names
      $drtinfo-minfos
      $drtinfo-sealed?
      $drtinfo-protocol?)

    (module ($make-drtinfo $drtinfo?)
      (define the-drtinfo-uid '#{drtinfo cb2hekodjsht6om8o8t9g00vq-0})
      (define ($make-drtinfo base-rtd maybe-ctrtd maybe-rtd maybe-rcd rtd-expr rcd-expr interface-names minfos sealed? protocol?)
        ($$make-drtinfo (datum->syntax #'* the-drtinfo-uid) base-rtd maybe-ctrtd maybe-rtd maybe-rcd rtd-expr rcd-expr interface-names minfos sealed? protocol?))
      (define $drtinfo?
        (lambda (x)
          (and ($$drtinfo? x) (eq? (syntax->datum ($drtinfo-uid x)) the-drtinfo-uid)))))

    (define (unwrap-drtinfo x)
      (syntax-case x ()
        [#(uid base-rtd maybe-ctrtd maybe-rtd maybe-rcd rtd-expr rcd-expr (iface-name ...) (minfo ...) sealed? protocol?)
         #`#(uid
             #,(syntax->datum #'base-rtd)
             #,(syntax->datum #'maybe-ctrtd)
             #,(syntax->datum #'maybe-rtd)
             #,(syntax->datum #'maybe-rcd)
             rtd-expr
             rcd-expr
             (iface-name ...)
             (minfo ...)
             #,(syntax->datum #'sealed?)
             #,(syntax->datum #'protocol?))]
        [_ x])))

  (module ($make-diinfo $diinfo? $diinfo-rtd $diinfo-minfos unwrap-diinfo)
    (define-vector-record ($$make-diinfo $$diinfo?)
      $diinfo-uid
      $diinfo-rtd
      $diinfo-minfos)

    (module ($make-diinfo $diinfo?)
      (define the-diinfo-uid '#{diinfo cb2hekodjsht6om8o8t9g00vq-1})
      (define ($make-diinfo rtd minfos)
        ($$make-diinfo (datum->syntax #'* the-diinfo-uid) rtd minfos))
      (define $diinfo?
        (lambda (x)
          (and ($$diinfo? x) (eq? (syntax->datum ($diinfo-uid x)) the-diinfo-uid)))))

    (define (unwrap-diinfo x)
      (syntax-case x ()
        [#(uid rtd (minfo ...))
         #`#(uid
             #,(syntax->datum #'rtd)
             (minfo ...))]
        [_ x])))

  (module (make-minfo minfo?
           minfo-mname minfo-hidden-mname minfo-arity minfo-formals minfo-flat-formals
           unwrap-minfos)
    (define-vector-record (make-minfo minfo?)
      minfo-mname
      minfo-hidden-mname
      minfo-arity
      minfo-formals
      minfo-flat-formals)
    (define unwrap-minfos
      (lambda (minfos)
        ; need list/vector structure, with arity unwrapped
        (with-syntax ([(#(mname hidden-name arity formals flat-formals) ...) minfos])
          (with-syntax ([(arity ...) (syntax->datum #'(arity ...))])
            #'(#(mname hidden-name arity formals flat-formals) ...))))))

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
    (lambda (minfos indices)
      (define cull-method
        (lambda (mname minfos indices)
          (if (null? minfos)
              (values '() '() '() '())
              (let ([offset (car indices)] [minfo (car minfos)])
                (let-values ([(gminfos gindices new-minfos new-indices)
                              (cull-method mname (cdr minfos) (cdr indices))])
                  (if (bound-identifier=? (minfo-mname minfo) mname)
                      (values
                        (cons minfo gminfos)
                        (cons offset gindices)
                        new-minfos
                        new-indices)
                      (values
                        gminfos
                        gindices
                        (cons (car minfos) new-minfos)
                        (cons (car indices) new-indices))))))))
      (if (null? minfos)
          '()
          (let ([generic-name (minfo-mname (car minfos))])
            (let-values ([(gminfos gindices minfos indices)
                          (cull-method generic-name minfos indices)])
              (cons `(,generic-name
                      ,@(map (lambda (minfo offset)
                               `(,(minfo-formals minfo) ,(minfo-flat-formals minfo) ,offset))
                             gminfos
                             gindices))
                    (build-generic minfos indices)))))))

  (define free-id-member
    (lambda (id ls2)
      (and (not (null? ls2))
           (if (free-identifier=? (car ls2) id)
               ls2
               (free-id-member id (cdr ls2))))))

  (define free-id-union
    (lambda (ls1 ls2)
      (if (null? ls1)
          ls2
          (if (free-id-member (car ls1) ls2)
              (free-id-union (cdr ls1) ls2)
              (cons (car ls1) (free-id-union (cdr ls1) ls2))))))

  (set! $trans-define-interface
    (lambda (x)
      (define src x)
      (lambda (env)
        (define (do-define-interface iname pred-name clause*)
          (define-flags clause-key
            (parent        #b0000000010)
            (methods       #b0010000000))
          (define (parse-method x)
            (syntax-case x ()
              [(name formals)
               (let-values ([(arity flat-formals) (parse-formals #'formals)])
                 (make-minfo #'name "ignored" arity #'formals flat-formals))]))
          (define-syntactic-monad Mclause %minfos %parent)
          (define parse-clauses
            (Mclause lambda (keys-seen clause*)
              (if (null? clause*)
                  (Mclause values () keys-seen)
                  (syntax-case (car clause*) (methods parent)
                    [(methods method ...)
                     (begin
                       (when (any-set? keys-seen (clause-key methods))
                         (syntax-error src "interface definition has multiple methods clauses"))
                       (Mclause parse-clauses
                         ([%minfos (map parse-method #'(method ...))])
                         (set-flags keys-seen (clause-key methods))
                         (cdr clause*)))]
                    [(parent pname)
                     (identifier? #'pname)
                     (let ()
                       (when (any-set? keys-seen (clause-key parent))
                         (syntax-error src "interface definition has multiple parent clauses"))
                       (let ([x (unwrap-diinfo (env #'pname))])
                         (unless ($diinfo? x)
                           (syntax-error #'pname "define-interface: unrecognized parent interface"))
                         (Mclause parse-clauses ([%parent x])
                           (set-flags keys-seen (clause-key parent))
                           (cdr clause*))))]
                    [_ (syntax-error (car clause*) "invalid define-interface clause")]))))
          (call-with-values
            (lambda ()
              (Mclause parse-clauses
                ([%minfos '()]
                 [%parent #f])
                (clause-key (or))
                clause*))
            (Mclause lambda (keys-seen)
              (let ([parent-iface-rtd (and %parent ($diinfo-rtd %parent))]
                    [parent-minfos (if %parent ($diinfo-minfos %parent) '())])
                (for-each
                  (let ([parent-mnames (map minfo-mname parent-minfos)])
                    (lambda (minfo)
                      (when (free-id-member (minfo-mname minfo) parent-mnames)
                        (syntax-error (minfo-mname minfo) "conflict with inherited interface method"))))
                  %minfos)
                (let ([iface-rtd
                       ($make-record-type-descriptor
                         #!base-rtd
                         (datum iname)
                         parent-iface-rtd
                         #f #f #f
                         (vector-map
                           (lambda (x) `(immutable ,(syntax->datum (minfo-mname x))))
                           (list->vector %minfos))
                         'define-interface)])
                  (with-syntax ([((generic-name (generic-formals generic-flat-formals generic-index) ...) ...)
                                 (build-generic
                                   %minfos
                                   (let ([ls (enumerate (csv7:record-type-field-names iface-rtd))])
                                     (list-tail ls (- (length ls) (length %minfos)))))]
                                [opt3 (= (optimize-level) 3)])
                    (with-syntax ([((method-accessor ...) ...) (map generate-temporaries #'((generic-index ...) ...))])
                      #`(begin
                          (define-syntax #,iname
                            (make-compile-time-value
                              #'#,($make-diinfo
                                    iface-rtd
                                    #`(#,@parent-minfos #,@%minfos))))
                          (define (qi! who ego)
                            (or (and (or opt3 (record? ego)) (#3%$query-interface '#,iface-rtd ego))
                                (errorf who "not applicable to ~s" ego)))
                          (define #,pred-name
                            (lambda (x)
                              (and (record? x) (#3%$query-interface '#,iface-rtd x) #t)))
                          (define generic-name
                            (let ([who 'generic-name]) ; can't ref generic-name pattern vble inside ... below
                              (define method-accessor (#3%csv7:record-field-accessor '#,iface-rtd generic-index))
                              ...
                              (case-lambda
                                [(ego . generic-formals)
                                 ((method-accessor (qi! who ego)) ego . generic-flat-formals)]
                                ...)))
                          ...))))))))
      (syntax-case x ()
        [(_ (name pred-name) clause ...)
         (and (identifier? #'name) (identifier? #'pred-name))
         (do-define-interface
           #'name
           #'pred-name
           #'(clause ...))]
        [(_ name clause ...)
         (identifier? #'name)
         (do-define-interface
           #'name
           (construct-name #'name #'name "?")
           #'(clause ...))]))))

  (set! $query-interface
    (lambda (iface-rtd ego)
      (let ([rtd (#3%record-rtd ego)])
        (let* ([v ($record-type-interfaces rtd)] [n (vector-length v)])
          (let loop ([i 0])
            (and (fx< i n)
                 (let ([iface (vector-ref v i)])
                   (if (#3%record? iface iface-rtd)
                     iface
                     (loop (fx+ i 1))))))))))

  (set! $trans-open-interface
    (lambda (x)
      (syntax-case x ()
        [(_ iface-name ([local-mname iface-mname] ...) obj-expr)
         (lambda (env)
           (let ([local-mname* #'(local-mname ...)])
             (unless ($distinct-bound-ids? local-mname*)
               ($invalid-ids-error local-mname* x "local method name"))
             (let* ([diinfo (unwrap-diinfo (env #'iface-name))]
                    [iface-rtd ($diinfo-rtd diinfo)])
               (unless ($diinfo? diinfo) (syntax-error #'iface-name "unrecognized interface"))
               (with-syntax ([(((generic-formals generic-flat-formals generic-index) ...) ...)
                              (let ([x* (build-generic
                                          (unwrap-minfos ($diinfo-minfos diinfo))
                                          (enumerate (csv7:record-type-field-names iface-rtd)))])
                                (map (lambda (iface-mname)
                                       (let loop ([x* x*])
                                         (when (null? x*) (syntax-error iface-mname "unrecognized interface method name"))
                                         (syntax-case (car x*) ()
                                           [(generic-name . stuff) 
                                            (if (free-identifier=? #'generic-name iface-mname)
                                                #'stuff
                                                (loop (cdr x*)))])))
                                     #'(iface-mname ...)))]
                             [opt3 (= (optimize-level) 3)])
                 #`(begin
                     (define obj obj-expr)
                     (define iface 
                       (or (and (or opt3 (record? obj)) (#3%$query-interface '#,iface-rtd obj))
                           (errorf 'iface-name "not implemented by ~s" obj)))
                     (define local-mname
                       (let ()
                         (define method ((#3%csv7:record-field-accessor '#,iface-rtd generic-index) iface))
                         ...
                         (case-lambda
                           [generic-formals (method obj . generic-flat-formals)]
                           ...)))
                     ...)))))])))

  (set! $trans-define-record-type
    (lambda (x)
      (define src x)
      (define build-method
        (lambda (minfo rtname body base-minfos)
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
                         (with-syntax ([super (datum->syntax rtname 'super)])
                           (if (not (null? base-minfos))
                               (with-syntax ([(#(mname hidden-name arity formals flat-formals) ...)
                                              (cull-super (minfo-mname minfo) base-minfos)])
                                 #'(define super (case-lambda [formals (hidden-name ego . flat-formals)] ...)))
                               (with-syntax ([mname (minfo-mname minfo)] [rtname rtname])
                                 #'(define-syntax super
                                     (lambda (x)
                                       (syntax-error x
                                         (format "no inherited ~s method for ~s in" 'mname 'rtname)))))))])
            #'(lambda (i formal ...)
                ; NB: assuming i is a proper instance.  we should verify if we officially
                ; allow users to access and call methods directly out of the vtable
                (fluid-let-syntax ([ego (identifier-syntax i)])
                  super-definition
                  body)))))
      (define build-interface-vtable
        (lambda (minfos)
          (lambda (iface)
            (with-syntax
              ([irtd ($diinfo-rtd iface)]
               [(hidden ...)
                (map (lambda (iminfo)
                       (let ([mname (minfo-mname iminfo)] [arity (minfo-arity iminfo)])
                         (let f ([minfos minfos])
                           (cond
                             [(null? minfos) (syntax-error src
                                               (format
                                                 "no suitable implementation for interface method ~s"
                                                 (syntax->datum mname)))]
                             [(minfo-match? (car minfos) mname arity) (minfo-hidden-mname (car minfos))]
                             [else (f (cdr minfos))]))))
                     (unwrap-minfos ($diinfo-minfos iface)))])
              #'(#3%$record 'irtd hidden ...)))))
      (define minfo-match?
        (lambda (minfo mname arity)
          (and (= arity (minfo-arity minfo))
               (free-identifier=? mname (minfo-mname minfo)))))
      (define process-methods
        (lambda (rtname interfaces all-base-minfos minfos bodies)
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
                                      (unwrap-minfos ($diinfo-minfos i))))
                                  interfaces)))
                              generics)
                          (list (reverse all-minfos)
                                (reverse mlambdas)
                                (reverse generics)
                               ; build a list of minfos that will actually be included in the rtd/vtable
                               ; i.e., minfos that appear in this record-type and/or any parent record-type (but not in any interface).
                               ; That list will match up with the rtd/vtable offsets.
                                (let f ([possibly-included-minfos all-minfos] [included-minfos '()])
                                  (if (null? possibly-included-minfos)
                                      included-minfos
                                      (let ([minfo (car possibly-included-minfos)])
                                        (let ([mname (minfo-mname minfo)] [arity (minfo-arity minfo)])
                                          (f (cdr possibly-included-minfos)
                                             (if (ormap
                                                   (lambda (i)
                                                     (ormap (lambda (m) (minfo-match? m mname arity))
                                                            (unwrap-minfos ($diinfo-minfos i))))
                                                   interfaces)
                                                 included-minfos
                                                 (cons minfo included-minfos)))))))))
                        (let ([minfo (car minfos)])
                          (let ([mname (minfo-mname minfo)] [arity (minfo-arity minfo)])
                            (when (ormap (lambda (base-mname) (free-identifier=? mname base-mname)) base-mnames)
                              (syntax-error mname "arity not supported by base record-type method"))
                            (f (cdr minfos)
                               (cdr bodies)
                               (cons
                                 (list (minfo-hidden-mname minfo)
                                       (build-method minfo rtname (car bodies) '()))
                                 mlambdas)
                               (cons minfo all-minfos)
                               (if (ormap
                                     (lambda (i)
                                       (ormap (lambda (m) (minfo-match? m mname arity))
                                         (unwrap-minfos ($diinfo-minfos i))))
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
                                          (build-method (car xminfos) rtname (car xbodies) all-base-minfos))
                                    mlambdas)
                              (cons (car xminfos) all-minfos))]
                          [else (find-method (cdr xminfos) (cdr xbodies))])))))))))
      (lambda (env)
        (define (do-define-record-type rtname make-name pred-name clause*)
          (define-flags clause-key
            (fields        #b0000000001)
            (parent        #b0000000010)
            (protocol      #b0000000100)
            (sealed        #b0000001000)
            (opaque        #b0000010000)
            (nongenerative #b0000100000)
            (parent-rtd    #b0001000000)
            (methods       #b0010000000)
            (implements    #b0100000000))
          (define-record-type field-desc
            (fields (immutable name) (immutable index) (immutable spec) (immutable accessor) (immutable mutator) (immutable public?))
            (nongenerative)
            (sealed #t))
          (define-record-type method-desc
            (fields (immutable minfo) (immutable body))
            (nongenerative)
            (sealed #t))
          (define (parse-field x i)
            (define (err why) (syntax-error x why))
            (define (finish field-name public? mutable? accessor-name mutator-name)
              (make-field-desc field-name i
                #`(#,(if mutable? #'mutable #'immutable) #,field-name)
                (or accessor-name (construct-name rtname rtname "-" field-name))
                (or mutator-name (and mutable? (construct-name rtname rtname "-" field-name "-set!")))
                public?))
            (define (parse-field0 x)
              (syntax-case x (public private)
                [field-name
                 (identifier? #'field-name)
                 (finish #'field-name #t #f #f #f)]
                [(public . rest) (parse-field1 #'rest #t)]
                [(private . rest) (parse-field1 #'rest #f)]
                [else (parse-field2 x #t)]))
            (define (parse-field1 x public?)
              (syntax-case x ()
                [(field-name)
                 (identifier? #'field-name)
                 (finish #'field-name public? #f #f #f)]
                [else (parse-field2 x public?)]))
            (define (parse-field2 x public?)
              (syntax-case x (mutable immutable)
                [(mutable field-name)
                 (identifier? #'field-name)
                 (finish #'field-name public? #t #f #f)]
                [(immutable field-name)
                 (identifier? #'field-name)
                 (finish #'field-name public? #f #f #f)]
                [(mutable field-name accessor mutator)
                 (and (identifier? #'field-name) (identifier? #'accessor) (identifier? #'mutator))
                 (finish #'field-name public? #t #'accessor #'mutator)]
                [(immutable field-name accessor)
                 (and (identifier? #'field-name) (identifier? #'accessor))
                 (finish #'field-name public? #f #'accessor #f)]
                [else (err "invalid field specifier")]))
            (parse-field0 x))
          (define (parse-method x)
            (syntax-case x ()
              [(name formals e1 e2 ...)
               (let-values ([(arity flat-formals) (parse-formals #'formals)])
                 (let ([hidden (car (generate-temporaries (list #'name)))])
                   (make-method-desc
                     (make-minfo #'name hidden arity #'formals flat-formals)
                     #'(let () e1 e2 ...))))]))
          (define-syntactic-monad Mclause %fields %methods %interface-names %parent %protocol
            %sealed? %opaque? %uid %prtd-expr %prcd-expr)
          (define parse-clauses
            (Mclause lambda (keys-seen clause*)
              (if (null? clause*)
                  (Mclause values () keys-seen)
                  (syntax-case (car clause*) (fields methods implements parent protocol sealed opaque nongenerative parent-rtd)
                    [(fields field ...)
                     (begin
                       (when (any-set? keys-seen (clause-key fields))
                         (syntax-error src "record-type definition has multiple fields clauses"))
                       (let ([fields (let ([ls #'(field ...)])
                                       (map parse-field ls (enumerate ls)))])
                         (Mclause parse-clauses
                           ([%fields fields])
                           (set-flags keys-seen (clause-key fields))
                           (cdr clause*))))]
                    [(methods method ...)
                     (begin
                       (when (any-set? keys-seen (clause-key methods))
                         (syntax-error src "record-type definition has multiple methods clauses"))
                       (Mclause parse-clauses
                         ([%methods (map parse-method #'(method ...))])
                         (set-flags keys-seen (clause-key methods))
                         (cdr clause*)))]
                    [(implements iname ...)
                     (andmap identifier? #'(iname ...))
                     (begin
                       (when (any-set? keys-seen (clause-key implements))
                         (syntax-error src "record-type definition has multiple implements clauses"))
                       (Mclause parse-clauses
                         ([%interface-names #'(iname ...)])
                         (set-flags keys-seen (clause-key implements))
                         (cdr clause*)))]
                    [(parent pname)
                     (identifier? #'pname)
                     (let ()
                       (when (any-set? keys-seen (clause-key parent))
                         (syntax-error src "record-type definition has multiple parent clauses"))
                       (when (any-set? keys-seen (clause-key parent-rtd))
                         (syntax-error src "record-type definition has both parent and parent-rtd clauses"))
                       (let ([x (unwrap-drtinfo (env #'pname))])
                         (when (and (pair? x) (eq? (car x) '#{record val9xfsq6oa12q4-a}))
                           (syntax-error #'pname "cannot extend define-record parent"))
                         (unless ($drtinfo? x)
                           (syntax-error #'pname "define-record-type: unrecognized parent record type"))
                         (when ($drtinfo-sealed? x)
                           (syntax-error #'pname "parent record type is sealed"))
                         (Mclause parse-clauses ([%parent x])
                           (set-flags keys-seen (clause-key parent))
                           (cdr clause*))))]
                    [(protocol expr)
                     (begin
                       (when (any-set? keys-seen (clause-key protocol))
                         (syntax-error src "record-type definition has multiple protocol clauses"))
                       (Mclause parse-clauses ([%protocol #'expr])
                         (set-flags keys-seen (clause-key protocol))
                         (cdr clause*)))]
                    [(sealed expr)
                     (memq (datum expr) '(#t #f))
                     (begin
                       (when (any-set? keys-seen (clause-key sealed))
                         (syntax-error src "record-type definition has multiple sealed clauses"))
                       (Mclause parse-clauses ([%sealed? (datum expr)])
                         (set-flags keys-seen (clause-key sealed))
                         (cdr clause*)))]
                    [(opaque expr)
                     (memq (datum expr) '(#t #f))
                     (begin
                       (when (any-set? keys-seen (clause-key opaque))
                         (syntax-error src "record-type definition has multiple opaque clauses"))
                       (Mclause parse-clauses ([%opaque? (datum expr)])
                         (set-flags keys-seen (clause-key opaque))
                         (cdr clause*)))]
                    [(nongenerative)
                     (begin
                       (when (any-set? keys-seen (clause-key nongenerative))
                         (syntax-error src "record-type definition has multiple nongenerative clauses"))
                       (Mclause parse-clauses ([%uid (datum->syntax #'* (gensym (symbol->string (syntax->datum rtname))))])
                         (set-flags keys-seen (clause-key nongenerative))
                         (cdr clause*)))]
                    [(nongenerative id)
                     (identifier? #'id)
                     (begin
                       (when (any-set? keys-seen (clause-key nongenerative))
                         (syntax-error src "record-type definition has multiple nongenerative clauses"))
                       (Mclause parse-clauses ([%uid #'id])
                         (set-flags keys-seen (clause-key nongenerative))
                         (cdr clause*)))]
                    [(nongenerative #f)
                     (begin
                       (when (any-set? keys-seen (clause-key nongenerative))
                         (syntax-error src "record-type definition has multiple nongenerative clauses"))
                       (Mclause parse-clauses ()
                         (set-flags keys-seen (clause-key nongenerative))
                         (cdr clause*)))]
                    [(parent-rtd prtd-expr pcd-expr)
                     (begin
                       (when (any-set? keys-seen (clause-key parent-rtd))
                         (syntax-error src "record-type definition has multiple parent-rtd clauses"))
                       (when (any-set? keys-seen (clause-key parent))
                         (syntax-error src "record-type definition has both parent and parent-rtd clauses"))
                       (Mclause parse-clauses
                         ([%prtd-expr #'prtd-expr] [%prcd-expr #'pcd-expr])
                         (set-flags keys-seen (clause-key parent-rtd))
                         (cdr clause*)))]
                    [_ (syntax-error (car clause*) "invalid define-record-type clause")]))))
          (define (build-field-defn ctrtd)
            (lambda (field-desc)
              (let ([field-name (field-desc-name field-desc)]
                    [index (field-desc-index field-desc)])
                #`(module (#,field-name)
                    (define get ($record-accessor/proxy '#,ctrtd #,index))
                    #,@(if (field-desc-mutator field-desc)
                           #`((define set ($record-mutator/proxy '#,ctrtd #,index)))
                           '())
                    (define-syntax #,field-name
                      (make-variable-transformer
                        (lambda (x)
                          (syntax-case x (set!)
                            [id (identifier? #'id) #'(get ego)]
                            [(set! var val)
                             #,(if (field-desc-mutator field-desc)
                                   #'#'(set ego val)
                                   #'(syntax-error x "invalid assignment of immutable field"))]
                            [(id e (... ...)) (identifier? #'id) #'((get ego) e (... ...))]))))))))
          (call-with-values
            (lambda ()
              (Mclause parse-clauses
                ([%fields '()]
                 [%methods '()]
                 [%interface-names '()]
                 [%parent #f]
                 [%protocol #f]
                 [%sealed? #f]
                 [%opaque? #f]
                 [%uid #f]
                 [%prtd-expr #f]
                 [%prcd-expr #f])
                (clause-key (or))
                clause*))
            (Mclause lambda (keys-seen)
              (when (require-nongenerative-clause)
                (unless (any-set? keys-seen (clause-key nongenerative))
                  (syntax-error src "missing nongenerative clause and require-nongenerative-clause is #t")))
              (unless %protocol
                (when (and %parent ($drtinfo-protocol? %parent))
                  (syntax-error src "no protocol supplied, but parent protocol was supplied")))
              (let* ([%mutable-fields (filter field-desc-mutator %fields)]
                     [%all-interface-names (free-id-union %interface-names (if %parent (syntax->list ($drtinfo-interface-names %parent)) '()))]
                     [%interfaces (map (lambda (x)
                                         (let ([diinfo (unwrap-diinfo (env x))])
                                           (unless ($diinfo? diinfo) (syntax-error x "unrecognized interface"))
                                           diinfo))
                                        %all-interface-names)]
                     [%parent-minfos (if %parent (unwrap-minfos ($drtinfo-minfos %parent)) '())]
                     [maybe-rtd 
                       (and (and (null? %interfaces) (null? %methods) (null? %parent-minfos))
                            (or %uid ($syntax-top-level?))
                            (if %parent ($drtinfo-maybe-rtd %parent) (not %prtd-expr))
                            ($make-record-type-descriptor
                              #!base-rtd
                              (syntax->datum rtname)
                              (and %parent ($drtinfo-maybe-rtd %parent))
                              (syntax->datum %uid)
                              %sealed?
                              %opaque?
                              (list->vector (map syntax->datum (map field-desc-spec %fields)))
                              'define-record-type))]
                     [maybe-rcd
                       (and (and maybe-rtd (not %protocol))
                            ($make-record-constructor-descriptor maybe-rtd
                              (and %parent ($drtinfo-maybe-rcd %parent))
                              #f 'define-record-type))]
                     ; ctrtd is used as a proxy in the creation of field accessors and mutators to avoid unnecessary
                     ; checks accross library boundaries and to avoid creating unresolvable letrec* cycles among the
                     ; methods, vtable, and field accessors of a record with methods.
                     [maybe-ctrtd
                       (or maybe-rtd
                           (and (if %parent ($drtinfo-maybe-ctrtd %parent) (not %prtd-expr))
                                ($make-record-type-descriptor
                                  #!base-rtd
                                  (syntax->datum rtname)
                                  (and %parent ($drtinfo-maybe-ctrtd %parent))
                                  #f
                                  %sealed?
                                  %opaque?
                                  (list->vector (map syntax->datum (map field-desc-spec %fields)))
                                  'define-record-type)))])
                (with-syntax ([primlev (if (= (optimize-level) 3) 3 2)]
                              [((accessor-name accessor-index) ...)
                               (fold-right (lambda (field-desc ls)
                                             (if (field-desc-public? field-desc)
                                                 `((,(field-desc-accessor field-desc) ,(field-desc-index field-desc)) ,@ls)
                                                 ls))
                                 '() %fields)]
                              [((mutator-name mutator-index) ...)
                               (fold-right (lambda (field-desc ls)
                                             (if (field-desc-public? field-desc)
                                                 `((,(field-desc-mutator field-desc) ,(field-desc-index field-desc)) ,@ls)
                                                 ls))
                                 '() %mutable-fields)]
                              [rtd (if maybe-rtd #`'#,maybe-rtd #'rtd)]
                              [rcd (if maybe-rcd #`'#,maybe-rcd #'rcd)])
                  (if (and (null? %interfaces) (null? %methods) (null? %parent-minfos))
                      ; this just a record definition
                      ; construct plain record rtd at expand time iff:
                      ;  - uid is not #f or definition is at top level
                      ;  - %parent is #f or its rtd is not #f (implying it is a compile-time plain record), and
                      ;  - %prtd-expr is #f.
                      (begin
                        (unless ($distinct-bound-ids? `(,rtname ,make-name ,pred-name ,@#'(accessor-name ...) ,@#'(mutator-name ...)))
                          (syntax-error src "record-type definition would result in duplicates among the record, constructor, predicate, accessor, and mutator names"))
                        #`(begin
                            #,(if maybe-rtd
                                  #'(begin)
                                  #`(define rtd
                                      ($make-record-type-descriptor
                                        #!base-rtd
                                        '#,rtname
                                        #,(if %parent ($drtinfo-rtd-expr %parent) %prtd-expr)
                                        '#,%uid
                                        #,%sealed?
                                        #,%opaque?
                                        '#,(list->vector (map field-desc-spec %fields))
                                        'define-record-type)))
                            #,(if maybe-rcd
                                  #'(begin)
                                  #`(define rcd
                                      ($make-record-constructor-descriptor rtd
                                        #,(if %parent ($drtinfo-rcd-expr %parent) %prcd-expr)
                                        #,%protocol
                                        'define-record-type)))
                            (define-syntax #,rtname
                              (make-compile-time-value
                                #'#,($make-drtinfo
                                      #!base-rtd
                                      maybe-ctrtd
                                      maybe-rtd
                                      maybe-rcd
                                      #'rtd
                                      #'rcd
                                      #'() ; interface-names
                                      #'() ; minfos
                                      %sealed?
                                      (and %protocol #t))))
                            #,(if maybe-rtd #'(begin) #`(indirect-export #,rtname rtd))
                            #,(if maybe-rcd #'(begin) #`(indirect-export #,rtname rcd))
                            (define #,make-name (($primitive primlev r6rs:record-constructor) rcd))
                            (define #,pred-name (($primitive primlev record-predicate) rtd))
                            #,(let ([proxy (if maybe-ctrtd #`'#,maybe-ctrtd #'rtd)])
                                (if (= (optimize-level) 3)
                                    #`(begin
                                        (define accessor-name ($record-accessor/proxy #,proxy accessor-index)) ...
                                        (define mutator-name ($record-mutator/proxy #,proxy mutator-index)) ...)
                                    #`(begin
                                        (define accessor-name ($record-accessor/proxy #,proxy rtd accessor-index)) ...
                                        (define mutator-name ($record-mutator/proxy #,proxy rtd mutator-index)) ...)))
                            ))
                      (begin
                        (when %uid
                          (syntax-error src "a record type with methods or interfaces cannot be nongenerative"))
                        (unless maybe-ctrtd
                          (syntax-error src "all ancestors of a record type with methods or interfaces must be specified statically, i.e., with parent rather than parent-rtd"))
                        (unless ($distinct-bound-ids? (map field-desc-name %fields))
                          (syntax-error src "duplicates among field names would cause ambiguity for references within methods"))
                        (with-syntax ([(iface-name ...) %all-interface-names]
                                      [(field-defn ...) (map (build-field-defn maybe-ctrtd) %fields)]
                                      [((all-minfo ...) ((new-hidden-mname mlambda) ...) (generic ...) (included-minfo ...))
                                       (process-methods rtname %interfaces %parent-minfos
                                         (map method-desc-minfo %methods)
                                         (map method-desc-body %methods))]
                                      [self (datum->syntax rtname 'self)])
                          (let ([%vtable-rtd (let ([parent-vtable-rtd
                                                    (or (and %parent ($drtinfo-base-rtd %parent))
                                                        #!base-rtd)])
                                               (if (null? #'(generic ...))
                                                   parent-vtable-rtd
                                                   ($make-record-type-descriptor
                                                     #!base-rtd
                                                     'vtable-rtd
                                                     parent-vtable-rtd
                                                     (gensym "vtable-rtd")
                                                     #f
                                                     #f
                                                     (vector-map
                                                       (lambda (x) `(immutable ,(syntax->datum (minfo-mname x))))
                                                       #'#(generic ...))
                                                     'define-record-type)))])
                            (with-syntax ([((generic-name (generic-formals generic-flat-formals generic-index) ...) ...)
                                           (build-generic
                                             #'(generic ...)
                                             (let ([indices (enumerate (csv7:record-type-field-names %vtable-rtd))] [minfos #'(included-minfo ...)])
                                               (let f ([indices (list-tail indices (- (length indices) (length minfos)))]
                                                       [minfos minfos]
                                                       [generics #'(generic ...)])
                                                 (if (null? generics)
                                                     '()
                                                     (if (eq? (car generics) (car minfos))
                                                         (cons (car indices) (f (cdr indices) (cdr minfos) (cdr generics)))
                                                         (f (cdr indices) (cdr minfos) generics))))))])
                              (unless ($distinct-bound-ids? `(,rtname ,make-name ,pred-name ,@#'(accessor-name ... mutator-name ...) ,@#'(generic-name ...)))
                                (syntax-error src "record-type definition would result in duplicates among the record-type, constructor, predicate, accessor, mutator, and generic names"))
                              (with-syntax ([((method-accessor ...) ...) (map generate-temporaries #'((generic-index ...) ...))])
                                #`(begin
                                    (module (#,rtname rtd/vtable rcd new-hidden-mname ...)
                                      (define-syntax ego values)
                                      (module (new-hidden-mname ...)
                                        field-defn ...
                                        (define-syntax self (identifier-syntax ego))
                                        (define new-hidden-mname mlambda) ...)
                                      (define rtd/vtable
                                        (#3%$make-record-type-descriptor/interfaces
                                          '#,%vtable-rtd
                                          '#,rtname
                                          #,(and %parent ($drtinfo-rtd-expr %parent))
                                          #f
                                          #,%sealed?
                                          #,%opaque?
                                          '#,(list->vector (map field-desc-spec %fields))
                                          (immutable-vector #,@(map (build-interface-vtable #'(all-minfo ...)) %interfaces))
                                          'define-record-type
                                          #,@(map minfo-hidden-mname #'(included-minfo ...))))
                                      (define rcd
                                        (#3%$make-record-constructor-descriptor rtd/vtable
                                          #,(and %parent ($drtinfo-rcd-expr %parent))
                                          #,%protocol
                                          'define-record-type))
                                      (define-syntax #,rtname
                                        (make-compile-time-value
                                          #'#,($make-drtinfo
                                                %vtable-rtd
                                                maybe-ctrtd
                                                #f
                                                #f
                                                #'rtd/vtable
                                                #'rcd
                                                #'(iface-name ...)
                                                #'(all-minfo ...)
                                                %sealed?
                                                (and %protocol #t))))
                                      (indirect-export #,rtname rtd/vtable rcd new-hidden-mname ...))
                                    (define #,make-name (($primitive primlev r6rs:record-constructor) rcd))
                                    (define #,pred-name (($primitive primlev record-predicate) rtd/vtable))
                                    #,(let ([proxy (if maybe-ctrtd #`'#,maybe-ctrtd #'rtd/vtable)])
                                        (if (= (optimize-level) 3)
                                            #`(begin
                                                (define accessor-name ($record-accessor/proxy #,proxy accessor-index)) ...
                                                (define mutator-name ($record-mutator/proxy #,proxy mutator-index)) ...)
                                            #`(begin
                                                (define accessor-name ($record-accessor/proxy #,proxy rtd/vtable accessor-index)) ...
                                                (define mutator-name ($record-mutator/proxy #,proxy rtd/vtable mutator-index)) ...)))
                                    (define generic-name
                                      (let ([who 'generic-name]) ; can't ref generic-name pattern vble inside ... below
                                        (define method-accessor (#3%csv7:record-field-accessor '#,%vtable-rtd generic-index))
                                        ...
                                        (case-lambda
                                          [(ego . generic-formals)
                                           #,@(if (= (optimize-level) 3)
                                                  #'()
                                                  #`((unless (and (#3%record? ego rtd/vtable)
                                                                  (#3%record? (#3%record-rtd ego) '#,%vtable-rtd))
                                                       (errorf who "not applicable to ~s" ego))))
                                           ((method-accessor (#3%record-rtd ego)) ego . generic-flat-formals)]
                                          ...)))
                                    ...))))))))))))
        (syntax-case x ()
          [(_ name clause ...)
           (identifier? #'name)
           (do-define-record-type #'name
             (construct-name #'name "make-" #'name)
             (construct-name #'name #'name "?")
             #'(clause ...))]
          [(_ (name make-name pred-name) clause ...)
           (and (identifier? #'name)
                (identifier? #'make-name)
                (identifier? #'pred-name))
           (do-define-record-type #'name #'make-name
             #'pred-name #'(clause ...))]))))

  (set! $trans-record-type-descriptor
    (lambda (x what)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (r)
           (let ([info (unwrap-drtinfo (r #'name))])
             (cond
               [(and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
                (with-syntax ([(rtd . stuff) (cdr info)])
                  #''rtd)]
               [($drtinfo? info)
                (let ([maybe-rtd ($drtinfo-maybe-rtd info)])
                  (if maybe-rtd #`'#,maybe-rtd ($drtinfo-rtd-expr info)))]
               [else (syntax-error #'name (format "~a: unrecognized record" what))])))])))

  (set! $trans-record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (r)
           (let ([info (unwrap-drtinfo (r #'name))])
             (cond
               [($drtinfo? info)
                (let ([maybe-rcd ($drtinfo-maybe-rcd info)])
                  (if maybe-rcd #`'#,maybe-rcd ($drtinfo-rcd-expr info)))]
               [(and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
                (syntax-error #'name "no constructor descriptor for define-record record type")]
               [else (syntax-error #'name "record-constructor-descriptor: unrecognized record")])))])))

  ;;; pre-r6rs record definition syntax
  ;;;
  ;;; (define-record name pname (field ...))
  ;;; (define-record name pname (field ...)
  ;;;   ((field init) ...))
  ;;; (define-record name pname (field ...)
  ;;;   ((field init) ...)
  ;;;   (option ...))
  ;;; name ::= id
  ;;; pname ::= <empty> | id
  ;;; field ::= id | (class type id)
  ;;; type ::= <empty> | supported record field type
  ;;; class ::= <empty> | immutable | mutable
  ;;; option ::= (prefix string)
  ;;;          | (predicate id)
  ;;;          | (constructor id)
  ;;;
  ;;; initialize fields containing non-ptr types to 0, then fill with
  ;;; $object-set!

  (set! $trans-define-record
    (lambda (x)
      (define-syntactic-monad option cons-id pred-id pref-id)
      (define parse-options
        (option lambda (ols)
          (if (null? ols)
              (option values)
              (syntax-case (car ols) (constructor predicate prefix)
                [(prefix s)
                 (string? (datum s))
                 (option parse-options ((pref-id (datum s)))
                   (cdr ols))]
                [(predicate id)
                 (identifier? #'id)
                 (option parse-options ((pred-id #'id)) (cdr ols))]
                [(constructor id)
                 (identifier? #'id)
                 (option parse-options ((cons-id #'id)) (cdr ols))]))))
      (define record-name
        (lambda (x)
          (let ((x (syntax->datum x)))
            (if (gensym? x) x (symbol->string x)))))
      (define field->id
        ; field -> id | ([class] [type] id)
        ; class -> immutable | mutable
        ; type -> scheme-object | double-float | ...
        (lambda (f)
          (define okay?
            (lambda (class type id)
              (and (identifier? id)
                   (or (not class)
                       (free-identifier=? class #'immutable)
                       (free-identifier=? class #'mutable))
                   (or (not type)
                       (memq (filter-foreign-type (syntax->datum type))
                             (record-datatype list))))))
          (syntax-case f ()
            [id (okay? #f #f #'id) #'id]
            [(id) (okay? #f #f #'id) #'id]
            [(class/type id)
             (or (okay? #'class/type #f #'id) (okay? #f #'class/type #'id))
             #'id]
            [(class type id) (okay? #'class #'type #'id) #'id]
            [_ (syntax-error f "invalid field specifier")])))
      (define disjoint?
        (lambda (sym*)
          (or (null? sym*)
              (and (not (memq (car sym*) (cdr sym*)))
                   (disjoint? (cdr sym*))))))
      (define do-define-record
        (lambda (src name prtd pid1s f1s pid2s f2s pinits inits opts)
          (define-syntax with (identifier-syntax with-syntax))
          (with-values
            (option parse-options
              ((cons-id #f) (pred-id #f) (pref-id #f))
              opts)
            (option lambda ()
              (with ([name name]
                     [((pinit ...) ...) pinits]
                     [(init ...) inits]
                     [(o ...) opts]
                     [((pid1 ...) ...) pid1s]
                     [((pid2 ...) ...) pid2s]
                     [(id1 ...) (map field->id f1s)]
                     [(id2 ...) (map field->id f2s)]
                     [primlev (if (= (optimize-level) 3) 3 2)]
                     [prefix (or pref-id (construct-name name name "-"))]
                     [constructor (or cons-id (construct-name name "make-" name))]
                     [predicate (or pred-id (construct-name name name "?"))])
                (unless (disjoint? (map syntax->datum #'(id1 ... id2 ...)))
                  (syntax-error src "duplicate field names in record definition"))
                (with ([rtd (if prtd
                                (make-record-type prtd (record-name #'name)
                                  (syntax->datum (append f1s f2s)))
                                (make-record-type (record-name #'name)
                                  (syntax->datum (append f1s f2s))))])
                  (let* ([pids (with ([((pid ...) ...) #'((pid1 ... pid2 ...) ...)])
                                 #'(pid ... ...))]
                         [allids (append pids #'(id1 ... id2 ...))]
                         [npids (length pids)])
                    (with ([((access ordinal) ...)
                            (map (lambda (id ordinal)
                                   (list (construct-name #'name #'prefix id)
                                         ordinal))
                              (list-tail allids npids)
                              (list-tail (enumerate allids) npids))]
                           [((assign !ordinal) ...)
                            (let f ([ids (list-tail allids npids)]
                                    [ordinal npids])
                              (if (null? ids)
                                  '()
                                  (if (csv7:record-field-mutable? #'rtd ordinal)
                                      (cons
                                        (list
                                          (construct-name #'name "set-" #'prefix (car ids) "!")
                                          ordinal)
                                        (f (cdr ids) (+ ordinal 1)))
                                      (f (cdr ids) (+ ordinal 1)))))])
                      #`(begin
                          (define-syntax name
                            (make-compile-time-value
                              `(#{record val9xfsq6oa12q4-a} rtd
                                 ,#'((pid1 ...) ... (id1 ...))
                                 ,#'((pid2 ...) ... (id2 ...))
                                 ,#'((((... ...) pinit) ...) ... (((... ...) init) ...)))))
                          (define constructor
                            (let ([rcons (($primitive primlev record-constructor) 'rtd)])
                              (lambda (pid1 ... ... id1 ...)
                               ; duplicating pinit code here
                                (let* ([pid2 pinit] ... ... [id2 init] ...)
                                  (rcons #,@allids)))))
                          (define predicate
                            (($primitive primlev record-predicate) 'rtd))
                          (define access
                            (($primitive primlev csv7:record-field-accessor) 'rtd ordinal)) ...
                          (define assign
                            (($primitive primlev csv7:record-field-mutator) 'rtd !ordinal)) ...)))))))))
      (define base-record
        (lambda (src name f1s f2s inits opts)
          (do-define-record src name #f '() f1s  '() f2s '() inits opts)))
      (define child-record
        (lambda (src name pname f1s f2s inits opts)
          (lambda (r)
            (define parent (r pname))
            (unless (and (pair? parent) (eq? (car parent) '#{record val9xfsq6oa12q4-a}))
              (syntax-error pname "unrecognized parent record type"))
            (with-syntax ([(prtd ((pid1 ...) ...)
                                 ((pid2 ...) ...)
                                 ((pinit ...) ...))
                           (cdr parent)])
              (do-define-record x name #'prtd
                #'((pid1 ...) ...) f1s
                #'((pid2 ...) ...) f2s
                #'((pinit ...) ...) inits opts)))))
      (syntax-case x ()
        [(_ name (f1 ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) '() '() '())]
        [(_ name (f1 ...) ((f2 init) ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) #'(f2 ...) #'(init ...) '())]
        [(_ name (f1 ...) ((f2 init) ...) (o ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) #'(f2 ...) #'(init ...) #'(o ...))]
        [(_ name pname (f1 ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) '() '() '())]
        [(_ name pname (f1 ...) ((f2 init) ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) #'(f2 ...) #'(init ...) '())]
        [(_ name pname (f1 ...) ((f2 init) ...) (o ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) #'(f2 ...) #'(init ...)
           #'(o ...))])))
) ; let ()

;;; define-record and define-record-type aux keywords
(define-syntax constructor (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax fields (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax immutable (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax implements (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax methods (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax mutable (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax nongenerative (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax opaque (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax parent (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax parent-rtd (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax predicate (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax prefix (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax private (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax protocol (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax public (lambda (x) (syntax-error x "misplaced aux keyword")))
(define-syntax sealed (lambda (x) (syntax-error x "misplaced aux keyword")))

(define-syntax define-interface (lambda (x) ($trans-define-interface x)))
(define-syntax open-interface (lambda (x) ($trans-open-interface x)))
(define-syntax define-record-type (lambda (x) ($trans-define-record-type x)))
(define-syntax type-descriptor (lambda (x) ($trans-record-type-descriptor x "type-descriptor")))
(define-syntax record-type-descriptor (lambda (x) ($trans-record-type-descriptor x "record-type-descriptor")))
(define-syntax record-constructor-descriptor (lambda (x) ($trans-record-constructor-descriptor x)))
(define-syntax define-record (lambda (x) ($trans-define-record x)))
