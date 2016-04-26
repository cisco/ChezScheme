;;; queue
;;; an abstract datatype

;;; operations:
;;;    (queue)           ;create a queue object

;;;    if 'q' is a queue object:

;;;    (q 'type?)        ;return the type (queue), useful if there are other
;;;                      ;abstract datatypes floating around.
;;;    (q 'empty?)       ;returns true iff q is empty
;;;    (q 'put val)      ;adds val to end of q; returns val
;;;    (q 'get)          ;removes first element of q and returns it

;;; Examples

;;;    (define! q (queue))
;;;    (q 'type?)             => queue
;;;    (q 'empty?)            => #!true
;;;    (q 'put 3)
;;;    (q 'put 4)
;;;    (q 'put 5)
;;;    (q 'empty?)            => ()
;;;    (q 'get)               => 3
;;;    (q 'get)               => 4
;;;    (q 'put 7)
;;;    (q 'get)               => 5
;;;    (q 'get)               => 7
;;;    (q 'empty?)            => #!true

(define queue
   (lambda ()
      (let ([head '()] [tail '()])
         (lambda (request . args)
            (case request
               [type? 'queue]
               [empty? (null? head)]
               [put
                (let ([v (car args)])
                   (if (null? head)
                       (let ([p (cons v '())])
                          (set! tail p)
                          (set! head p))
                       (let ([quebit (cons v '())])
                          (set-cdr! tail quebit)
                          (set! tail quebit)))
                   v)]
               [get
                (if (null? head)
                    (error 'queue "queue is empty")
                    (let ([v (car head)])
                       (set! head (cdr head))
                       (when (null? head) (set! tail '()))
                       v))]
               [else
                (error 'queue "~s is not a valid request" request)])))))
