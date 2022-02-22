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

(module priminfo (priminfo-unprefixed priminfo-libraries priminfo-mask priminfo-arity primvec get-priminfo
                  priminfo-arguments-type priminfo-rest-type priminfo-last-type priminfo-result-type priminfo-pred-type)
  (define-record-type priminfo
    (nongenerative)
    (sealed #t)
    (fields unprefixed libraries mask arity arguments-type rest-type last-type result-type pred-type))

  (define make-parameterlike box)

  (define prim-db (make-eq-hashtable))

  (define primvec
    (lambda ()
      (hashtable-keys prim-db)))

  (define get-priminfo
    (lambda (name)
      (or (eq-hashtable-ref prim-db name #f)
          ($oops #f "unknown primitive ~s" name))))

  (define (mask-result-type mask)
    (cond
      [(all-set? (prim-mask abort-op) mask)
       'bottom]
      [(all-set? (prim-mask boolean-valued) mask)
       'boolean]
      [(all-set? (prim-mask true) mask)
       'true]
      [(all-set? (prim-mask single-valued) mask)
       'ptr]
      [else #f]))

  (define (signature-boolean? signature*)
    (and (not (null? signature*))
         (andmap (lambda (sig)
                   (let ([out (cdr sig)])
                     (and (fx= (length out) 1)
                          (or (eq? (car out) 'boolean)
                              (eq? (car out) 'bottom)))))
                 signature*)))

  (define (signature-true? signature*)
    (and (not (null? signature*))
         (andmap (lambda (sig)
                   (let ([out (cdr sig)])
                     (and (pair? out)
                          (null? (cdr out))
                          (or (pair? (car out))
                          (not (or (eq? (car out) 'ptr)
                                   (eq? (car out) 'sub-ptr)
                                   (eq? (car out) 'boolean)
                                   (let ([name (symbol->string (car out))])
                                     (and (>= (string-length name) 6)
                                          (string=? (substring name 0 6) "maybe-"))))))))) 
                 signature*)))

  (define (signature-result-arity signature*)
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
     [else 'multiple]))
       
  (define (signature-result-type signature*)
    (cond
     [(null? signature*) #f] ; no signature
     [(and (fx= (length signature*) 2)
           (let ([sig0 (car signature*)])
             (and (fx= (length (car sig0)) 0)
                  (fx= (length (cdr sig0)) 1)))
           (let ([sig1 (cadr signature*)])
             (and (fx= (length (car sig1)) 1)
                  (fx= (length (cdr sig1)) 1)
                  (eq? (cadr sig1) 'void))))
      ; recognize parameter-like signatures
      ; TODO: recognize the inverse order
      (let ([type (cadar signature*)])
        (if (eq? type 'void)
          'void
          (make-parameterlike type)))]
     [(andmap (lambda (sig)
                (fx= (length (cdr sig)) 1))
              signature*)
      (let ([type (cadar signature*)])
        (cond
          [(andmap (lambda (sig)
                     (eq? (cadr sig) type))
                   (cdr signature*))
           type]
          [(signature-boolean? signature*) 'boolean]
          [(signature-true? signature*) 'true]
          [else 'ptr]))] ; a strange case, like list* and cons*
     [else #f])) ; multivalued

  (define (signature-parse signature*)
    (define (ellipsis? x) (eq? x '...))
    (define (type? x)
      (syntax-case x ()
        [(t1 . t2) (and (type? #'t1) (type? #'t2))]
        [t (and (symbol? #'t) (not (ellipsis? #'t)))]))
    (and (not (null? signature*))
         (map (lambda (sig)
                (syntax-case (car sig) ()
                  [(a ...)
                   (andmap type? #'(a ...))
                   (list (list->vector #'(a ...)) #f #f)]
                  [(a ... b dots)
                   (and (andmap type? #'(a ...))
                        (type? #'b)
                        (ellipsis? #'dots))
                   (list (list->vector #'(a ...)) #'b #f)]
                  [(a ... b dots d)
                   (and (andmap type? #'(a ...))
                        (type? #'b)
                        (ellipsis? #'dots)
                        (type? #'d))
                   (list (list->vector #'(a ...)) #'b #'d)]
                  [else
                   ($oops 'prims "unexpected pattern ~s in signature ~s" sig signature*)]))
              signature*)))

  (define (parsed-signature->interface psignature*)
    (and psignature*
         (map (lambda (sig)
                (define len (vector-length (car sig)))
                (cond
                  [(and (not (cadr sig)) (not (caddr sig)))
                   len]
                  [(not (caddr sig))
                   (- -1 len)]
                  [else 
                   (- -2 len)]))
              psignature*)))

  (define (parsed-signature->arguments-type psignature*)
    ; the last vector must be longer than the other, and the other must be subvectors
    (and psignature*
         (let ([longest (car (car (last-pair psignature*)))]) ; assume they are ordered
           (cond
             [(andmap (lambda (sig)
                        (define other (car sig))
                        (define l (vector-length other))
                        (let loop ([i 0])
                          (or (fx= i l)
                              (and (equal? (vector-ref longest i) (vector-ref other i))
                                   (loop (fx+ i 1))))))
                      psignature*)
              longest]
             [else
              ; the arguments disagree
              #f]))))

  (define (parsed-signature->rest-type psignature*)
    ; only one must be not #f
    (and psignature*
         (let loop ([psig* (map cadr psignature*)] [found #f])
           (cond
             [(null? psig*)
              found]
             [(not found)
              (loop (cdr psig*) (car psig*))]
             [(not (car psig*))
              (loop (cdr psig*) found)]
             [else
              ($oops 'prims "unexpected two values of rest argument ~s and ~s in signature with ~s" found (car psig*) psignature*)]))))

  (define (parsed-signature->last-type psignature*)
    ; only one must be not #f
    (and psignature*
         (let loop ([psig* (map caddr psignature*)] [found #f])
           (cond
             [(null? psig*)
              found]
             [(not found)
              (loop (cdr psig*) (car psig*))]
             [(not (car psig*))
              (loop (cdr psig*) found)]
             [else
              ($oops 'prims "unexpected two values of last argument ~s and ~s in signature with ~s" found (car psig*) psignature*)]))))

  (define put-priminfo!
    (lambda (prim unprefixed lib* mask sig* pred-type)
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
      (let ([result-arity (signature-result-arity sig*)]
            [result-type (if (null? sig*)
                             (mask-result-type mask)
                             (signature-result-type sig*))])
        (when (and (eq? result-arity 'multiple) (all-set? (prim-mask single-valued) mask))
          ($oops 'prims "inconsistent single-value information for ~s" prim))
        (when (and (eq? result-type 'ptr) (all-set? (prim-mask true) mask))
          ($oops 'prims "inconsistent true information for ~s ~s ~s ~s" prim result-type mask sig*))
        (let* ([mask (fxlogor mask
                              (if (eq? result-type 'bottom) (prim-mask abort-op) 0)
                              (if (eq? result-arity 'single) (prim-mask single-valued) 0)
                              (if (signature-boolean? sig*) (prim-mask boolean-valued) 0)
                              (if (signature-true? sig*) (prim-mask true) 0))]
               [psig* (signature-parse sig*)]
               [arguments-type (parsed-signature->arguments-type psig*)])
          (eq-hashtable-set! prim-db prim
            (make-priminfo unprefixed lib* mask (parsed-signature->interface psig*)
                           arguments-type
                           (and arguments-type (parsed-signature->rest-type psig*)) ; if arguments-type is confused, clean rest-type and last-type
                           (and arguments-type (parsed-signature->last-type psig*))
                           result-type
                           pred-type))))))

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
               (syntax-case x (feature sig flags pred ->)
                 [(prim [feature f] . more) #`(when-feature f #,(do-entry #'(prim . more)))]
                 [(prim [flags flag ...]) (do-entry #'(prim [sig] [pred #f] [flags flag ...]))]
                 [(prim [pred p] [flags flag ...]) (do-entry #'(prim [sig] [pred p] [flags flag ...]))]
                 [(prim [sig sigs ...] [flags flag ...])  (do-entry #'(prim [sig sigs ...] [pred #f] [flags flag ...]))]
                 [(prim [sig [(in ...) ... -> (out ...)] ...] [pred p] [flags flag ...])
                  (with-syntax ([(unprefixed . prim) (prim-name #'prim)])
                    (with-syntax ([((((in ...) (out ...)) ...) ...)
                                   (map ins-and-outs #'(((in ...) ...) ...) #'((out ...) ...))])
                      #'(put-priminfo! 'prim 'unprefixed '(lib ...)
                          (prim-mask (or shared-flag ... flag ...))
                          '([(in ...) . (out ...)] ... ...)
                          'p)))])))
           #`(begin #,@(map do-entry #'(entry ...))))])))

  (include "primdata.ss")
)
