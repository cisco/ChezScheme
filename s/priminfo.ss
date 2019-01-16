;;; priminfo.ss
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

(module priminfo (priminfo-unprefixed priminfo-libraries priminfo-mask priminfo-signatures priminfo-arity primvec
                  get-priminfo priminfo-boolean? priminfo-result-arity)
  (define-record-type priminfo
    (nongenerative)
    (sealed #t)
    (fields unprefixed libraries mask signatures arity))

  (define prim-db (make-eq-hashtable))

  (define primvec
    (lambda ()
      (hashtable-keys prim-db)))

  (define get-priminfo
    (lambda (name)
      (or (eq-hashtable-ref prim-db name #f)
          ($oops #f "unknown primitive ~s" name))))

  (define priminfo-boolean?
    (lambda (info)
      (let ([signature* (priminfo-signatures info)])
        (and (not (null? signature*))
             (andmap (lambda (sig)
                       (let ([out (cdr sig)])
                         (and (pair? out) (eq? (car out) 'boolean) (null? (cdr out)))))
                     signature*)))))

  (define priminfo-result-arity
    (lambda (info)
      (let ([signature* (priminfo-signatures info)])
        (cond
         [(null? signature*) 'unknown]
         [(andmap (lambda (sig)
                    (let ([out (cdr sig)])
                      (and (pair? out) (null? (cdr out)))))
                  signature*)
          ;; Note that a `(bottom)` result is treated as single-valued,
          ;; which is ok in the sense that the aborting operation will
          ;; produce a single value when it (never) returns.
          'single]
         [else 'multiple]))))

  (define signature->interface
    (lambda (sig)
      (define (ellipsis? x) (eq? x '...))
      (define (type? x)
        (syntax-case x ()
          [(t1 . t2) (and (type? #'t1) (type? #'t2))]
          [t (and (symbol? #'t) (not (ellipsis? #'t)))]))
      (syntax-case (car sig) ()
        [(a ...)
         (andmap type? #'(a ...))
         (length #'(a ...))]
        [(a ... b dots)
         (and (andmap type? #'(a ...))
              (type? #'b)
              (ellipsis? #'dots))
         (- -1 (length #'(a ...)))]
        [(a ... b dots d)
         (and (andmap type? #'(a ...))
              (type? #'b)
              (ellipsis? #'dots)
              (type? #'d))
         (- -2 (length #'(a ...)))])))

  (define put-priminfo!
    (lambda (prim unprefixed lib* mask sig*)
      (when (eq-hashtable-contains? prim-db prim)
        (warningf 'define-symbol-type "extra entry for ~s" prim))
      (unless (any-set? (prim-mask (or primitive system keyword system-keyword)) mask)
        (warningf 'define-symbol-type "none of primitive, system, keyword, and system-keyword is set for ~s" prim))
      (when (let ([mask (fxlogand mask (prim-mask (or primitive system keyword system-keyword)))])
              (not (fx= (fxlogand mask (fx- mask 1)) 0)))
        (warningf 'define-symbol-type "more than one of primitive, system, keyword, and system-keyword is set for ~s" prim))
      (when (and (null? sig*) (any-set? (prim-mask primitive) mask))
        (warningf 'define-symbol-type "no signatures for primitive ~s" prim))
      (when (and (not (null? sig*)) (any-set? (prim-mask (or keyword system-keyword)) mask))
        (warningf 'define-symbol-type "signatures given for keyword ~s" prim))
      (eq-hashtable-set! prim-db prim
        (make-priminfo unprefixed lib* mask sig* (map signature->interface sig*)))))

  (define-syntax define-symbol-flags*
    (lambda (x)
      (syntax-case x (libraries flags)
        [(_ ([libraries lib ...] [flags shared-flag ...]) entry ...)
         (andmap identifier? #'(shared-flag ...))
         (let ()
           (define prim-name
             (lambda (x)
               (syntax-case x ()
                 [(prefix prim)
                  (and (identifier? #'prefix) (identifier? #'prim))
                  (with-syntax ([prefix:prim (construct-name #'prim #'prefix #'prim)])
                    #'(prim . prefix:prim))]
                 [prim (identifier? #'prim) #'(prim . prim)])))
           (define ins-and-outs
             (lambda (ins outs)
               (syntax-case ins (->)
                 [((in ...) ...) #`(((in ...) #,outs) ...)])))
           (define do-entry
             (lambda (x)
               (syntax-case x (feature sig flags ->)
                 [(prim [feature f] . more) #`(when-feature f #,(do-entry #'(prim . more)))]
                 [(prim [flags flag ...]) (do-entry #'(prim [sig] [flags flag ...]))]
                 [(prim [sig [(in ...) ... -> (out ...)] ...] [flags flag ...])
                  (with-syntax ([(unprefixed . prim) (prim-name #'prim)])
                    (with-syntax ([((((in ...) (out ...)) ...) ...)
                                   (map ins-and-outs #'(((in ...) ...) ...) #'((out ...) ...))])
                      #'(put-priminfo! 'prim 'unprefixed '(lib ...)
                          (prim-mask (or shared-flag ... flag ...))
                          '([(in ...) . (out ...)] ... ...))))])))
           #`(begin #,@(map do-entry #'(entry ...))))])))

  (include "primdata.ss")
)
