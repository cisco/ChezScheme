;;; simple factorial function

;;; it is interesting to change the 'lambda' into 'trace-lambda'
;;; or simply type (trace fact) before running fact to observe
;;; the nesting of recursive calls.

(define fact
   (lambda (x)
      (if (zero? x)
          1
          (* x (fact (1- x))))))
