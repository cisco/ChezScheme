;;; simple fibonacci function

;;; uses trace-lambda to show the nesting

(define fib
   (trace-lambda fib (x)
      (if (<= x 1)
          1
          (+ (fib (- x 1)) (fib (- x 2))))))
