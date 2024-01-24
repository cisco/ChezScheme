;;; cpcheck.ss
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

;;; cpcheck checks argument counts in calls to primitives and user-defined
;;; procedures, where it can recognize them.  by running it after cp0, we
;;; catch more potentially incorrect calls, including calls to the record
;;; constructors and accessors constructed by cp0.  running it after cp0 can
;;; also lead to bogus warnings on rare occasions, as in:
;;; 
;;; (define (f b)
;;;   (define h (lambda (b f) (if b (f 1) (f 1 2))))
;;;   (if b
;;;       (h b (lambda (x) x))
;;;       (h b (lambda (x y) y))))
;;;
;;; where the calls (f 1) and (f 1 2) will be identified as having possible
;;; incorrect argument counts.  it seems like a reasonable tradeoff.

(define $cpcheck
(let ()
  (import (nanopass))
  (include "base-lang.ss")

  (define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))
  (define rtd-size (csv7:record-field-accessor #!base-rtd 'size))

  (define maybe-remake-rtd
    (lambda (rtd)
      (if (eq? ($target-machine) (machine-type))
          rtd
          ($remake-rtd rtd (let () (include "layout.ss") compute-field-offsets)))))

  (define record-field-offset
    (lambda (rtd index)
      (let* ([rtd (maybe-remake-rtd rtd)]
             [flds (rtd-flds rtd)])
        (if (fixnum? flds)
            (fx+ (constant record-data-disp) (fxsll index (constant log2-ptr-bytes)))
            (fld-byte (list-ref flds index))))))

  (define-pass cpcheck : Lsrc (ir) -> Lsrc ()
    (definitions
      (define-record-type call-context
        (nongenerative)
        (sealed #t)
        (fields cnt (mutable err))
        (protocol
          (lambda (new)
            (lambda (cnt) (new cnt #f)))))

      (define check!
        (lambda (ctxt interface*)
          (define interface-okay?
            (lambda (interface* cnt)
              (ormap
                (lambda (interface)
                  (if (fx< interface 0)
                      (fx>= cnt (lognot interface))
                      (fx= cnt interface)))
                interface*)))
          (when ctxt
            (unless (interface-okay? interface* (call-context-cnt ctxt))
              (call-context-err-set! ctxt #t)))))

      (define record-lambda!
        (lambda (id val)
          (unless (prelex-assigned id)
            (nanopass-case (Lsrc Expr) val
              [(case-lambda ,preinfo (clause (,x** ...) ,interface* ,body*) ...)
               (prelex-operand-set! id interface*)]
              [else (void)]))))

      (define-syntax with-record-lambda
        (syntax-rules ()
          [(_ ids vals body)
           (begin
             (for-each record-lambda! ids vals)
             (let ([x body])
               (for-each (lambda (id) (prelex-operand-set! id #f)) ids)
               x))]))

      (with-output-language (Lsrc Expr)
        (define build-sequence
          (lambda (x* body)
            (fold-left (lambda (body x) `(seq ,x ,body)) body x*)))

        (define argcnt-error
          (lambda (preinfo f args)
            (let* ([call (parameterize ([print-gensym #f] [print-level 3] [print-length 6])
                           (format "~s" (preinfo-sexpr preinfo)))]
                   [warn (lambda (src)
                           ($source-warning 'compile src #t
                             "possible incorrect argument count in call ~a"
                             call))])
              (cond
               [(enable-error-source-expression)
                `(seq ,f
                   ,(build-sequence args
                      (cond
                        [(preinfo-src preinfo) =>
                         (lambda (src)
                           (warn src)
                           `(call ,preinfo
                              ,(lookup-primref 2 '$source-violation)
                              (quote #f)
                              (quote ,src)
                              (quote #t)
                              (quote "incorrect argument count in call ~a")
                              (quote ,call)))]
                        [else
                         `(call ,preinfo
                            ,(lookup-primref 2 '$oops)
                            (quote #f)
                            (quote "incorrect argument count in call ~a")
                            (quote ,call))])))]
               [else
                ;; Just report warning, if source, and keep original call
                (cond [(preinfo-src preinfo) => warn])
                `(call ,preinfo ,f ,args ...)]))))))
    (Expr : Expr (ir [ctxt #f]) -> Expr ()
      [(quote ,d) ir]
      [(ref ,maybe-src ,x)
       (cond
         [(prelex-operand x) =>
          (lambda (interface*)
            (and (list? interface*)
                 (check! ctxt interface*)))])
       `(ref ,maybe-src ,x)]
      [(set! ,maybe-src ,x ,[e #f -> e]) `(set! ,maybe-src ,x ,e)]
      [(seq ,[e1 #f -> e1] ,[e2]) `(seq ,e1 ,e2)]
      [(if ,[e1 #f -> e1] ,[e2 #f -> e2] ,[e3 #f -> e3]) `(if ,e1 ,e2 ,e3)]
      [(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type)
       (check! ctxt (list (length arg-type*)))
       `(foreign (,conv* ...) ,name ,(Expr e #f) (,arg-type* ...) ,result-type)]
      [(fcallable (,conv* ...) ,[e #f -> e] (,arg-type* ...) ,result-type)
        `(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type)]
      [(call ,preinfo0
         (case-lambda ,preinfo1
           (clause (,x* ...) ,interface ,body)
           ,cl* ...)
         ,[e* #f -> e*] ...)
       (guard (fx= (length e*) interface))
       `(call ,preinfo0
          (case-lambda ,preinfo1
            (clause (,x* ...) ,interface
              ,(with-record-lambda x* e* (Expr body ctxt))))
          ,e* ...)]
      [(call ,preinfo ,e ,[e* #f -> e*] ...)
       (let ([sexpr (preinfo-sexpr preinfo)])
         (define ugly-gensym? ; gensym w/no pretty name
           (lambda (x)
             (and (gensym? x)
                  (let ([name ($symbol-name x)])
                    (or (not (pair? name)) (not (cdr name)))))))
         (if (and sexpr (and (pair? sexpr) (not (ugly-gensym? (car sexpr)))))
             (let ([ctxt (make-call-context (length e*))])
               (let ([e (Expr e ctxt)])
                 (if (call-context-err ctxt)
                     (argcnt-error preinfo e e*)
                     `(call ,preinfo ,e ,e* ...))))
             `(call ,preinfo ,(Expr e #f) ,e* ...)))]
      [(case-lambda ,preinfo (clause (,x** ...) ,interface* ,[body* #f -> body*]) ...)
       (check! ctxt interface*)
       `(case-lambda ,preinfo (clause (,x** ...) ,interface* ,body*) ...)]
      [(letrec ([,x* ,e*] ...) ,body)
       (with-record-lambda x* e*
         `(letrec ([,x* ,(map (lambda (e) (Expr e #f)) e*)] ...)
            ,(Expr body ctxt)))]
      [,pr (let ([arity (primref-arity pr)]) (when arity (check! ctxt arity))) pr]
      [(record-ref ,rtd ,type ,index ,[e #f -> e])
       `(call ,(make-preinfo-call) ,(lookup-primref 3 '$object-ref)
          (quote ,type) ,e (quote ,(record-field-offset rtd index)))]
      [(record-set! ,rtd ,type ,index ,[e1 #f -> e1] ,[e2 #f -> e2])
       `(call ,(make-preinfo-call) ,(lookup-primref 3 '$object-set!)
          (quote ,type) ,e1 (quote ,(record-field-offset rtd index)) ,e2)]
      [(record ,rtd ,[rtd-expr #f -> rtd-expr] ,[e* #f -> e*] ...)
       (let ([rtd (maybe-remake-rtd rtd)])
         (let ([fld* (rtd-flds rtd)] [rec-t (make-prelex*)])
           (safe-assert (fx= (length e*) (if (fixnum? fld*) fld* (length fld*))))
           (let ([filler* (if (fixnum? fld*)
                              '()
                              (fold-right
                               (lambda (fld e filler*)
                                 (let ([type (fld-type fld)])
                                   (if (eq? (filter-foreign-type type) 'scheme-object)
                                       filler*
                                       (cons
                                        `(call ,(make-preinfo-call) ,(lookup-primref 3 '$object-set!)
                                               (quote ,type) (ref #f ,rec-t) (quote ,(fld-byte fld)) ,e)
                                        filler*))))
                               '() fld* e*))])
             (if (null? filler*)
                 `(call ,(make-preinfo-call) ,(lookup-primref 3 '$record) ,rtd-expr ,e* ...)
                 (begin
                   (set-prelex-referenced! rec-t #t)
                   (set-prelex-multiply-referenced! rec-t #t)
                   `(call ,(make-preinfo-call)
                      (case-lambda ,(make-preinfo-lambda)
                        (clause (,rec-t) 1 ,(build-sequence filler* `(ref #f ,rec-t))))
                      (call ,(make-preinfo-call) ,(lookup-primref 3 '$record) ,rtd-expr
                        ,(map (lambda (arg) (cond [(eqv? arg 0) `(quote 0)] [else arg]))
                           (make-record-call-args fld* (rtd-size rtd) e*))
                        ...)))))))]
      [(cte-optimization-loc ,box ,[e #f -> e] ,exts) e]
      [(immutable-list (,e* ...) ,[e]) e]
      [(immutable-vector (,e* ...) ,[e]) e]
      [(moi) ir]
      [(pariah) ir]
      [(profile ,src) ir]
      [else (sorry! who "unhandled record ~s" ir)]))

  (lambda (x) (cpcheck x))))
