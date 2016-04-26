;;; doubly recursive power (expt) function

;;; try using trace-lambda to see the nesting.

(define power
   (lambda (x n)
      (cond
         [(= n 0) 1]
         [(= n 1) x]
         [else
          (let ([q (quotient n 2)])
             (* (power x q) (power x (- n q))))])))
