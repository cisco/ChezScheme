;;; scons.ss
;;; a stream-construction facility

;;; The scons special form performs a cons, suspending the cdr field
;;; by enclosing it in a procedure of no arguments.  scdr tests to see
;;; if the cdr is a procedure, and if so, invokes it.  scar is provided
;;; for symmetry; it is just car.

;;; The function stream-ref is simply list-ref defined in terms of
;;; scdr and scar.

;;; factlist and fiblist are two infinite streams.
;;; Try (stream-ref factlist 10) or (stream-ref fiblist 20).

;;; scons could easily suspend the car field as well.  This would
;;; implement the lazy cons of Friedman & Wise.

(define-syntax scons
  (syntax-rules ()
    ((_ car cdr) (cons car (lambda () cdr)))))

(define scar car)

(define scdr
   (lambda (x)
      (when (procedure? (cdr x)) (set-cdr! x ((cdr x))))
      (cdr x)))

(define stream-ref
   (lambda (x n)
      (if (zero? n)
          (scar x)
          (stream-ref (scdr x) (1- n)))))

(define factlist
   (let fact ([a 1] [n 1])
      (scons a (fact (* a n) (1+ n)))))

(define fiblist
   (let fib ([fib-2 0] [fib-1 1])
      (scons fib-1 (fib fib-1 (+ fib-2 fib-1)))))
