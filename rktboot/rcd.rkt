#lang racket/base
(require "scheme-struct.rkt"
         (for-template racket/base))

(provide rcd->constructor
         (struct-out rcd-info)
         rcd->rcdi)

(define (rcd->constructor rcd lookup-protocol)
  (define rtd (rec-cons-desc-rtd rcd))
  (define ctr (struct-type-make-constructor rtd))
  ((record-constructor-generator rcd lookup-protocol) ctr)) 

(define (record-constructor-generator rcd lookup-protocol)
  (define rtd (rec-cons-desc-rtd rcd))
  (define p (rec-cons-desc-protocol rcd))
  (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
    (struct-type-info rtd))
  (cond
    [(not p) (lambda (ctr) ctr)]
    [(rec-cons-desc-parent-rcd rcd)
     => (lambda (p-rcd)
          (define p-gen (record-constructor-generator p-rcd lookup-protocol))
          (and p-gen
               (lambda (ctr)
                 (p (p-gen
                     (lambda args1
                       (lambda args2
                         (apply ctr (append args1 args2)))))))))]
    [(and super (not lookup-protocol)) #f]
    [super
     (define parent-p (lookup-protocol super))
     (lambda (ctr)
       (p (parent-p
           (lambda args1
             (lambda args2
               (apply ctr (append args1 args2)))))))]
    [else p]))

;; ----------------------------------------

(struct rcd-info (rtd proto-expr base-rcdi init-cnt)
  #:transparent)

(define (rcd->rcdi rcd)
  (cond
    [(rec-cons-desc-parent-rcd rcd)
     => (lambda (p-rcd)
          (define p-rcdi (rcd->rcdi p-rcd))
          (and p-rcdi
               (let ()
                 (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
                   (struct-type-info (rec-cons-desc-rtd rcd)))
                 (define proto (rec-cons-desc-protocol rcd))
                 (rcd-info (rec-cons-desc-rtd rcd)
                           proto
                           p-rcdi
                           (+ init-cnt
                              (rcd-info-init-cnt p-rcdi))))))]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info (rec-cons-desc-rtd rcd)))
     (define proto (rec-cons-desc-protocol rcd))
     (and (not super)
          (rcd-info (rec-cons-desc-rtd rcd)
                    proto
                    #f
                    init-cnt))]))
