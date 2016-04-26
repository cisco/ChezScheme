;;; fat fibonacci function

;;; this is "fat" because it uses only increments and decrements
;;; for addition and subtraction (i.e., peano arithmetic).

;;; note that fat+ is tail-recursive; this is how all looping is
;;; performed in Scheme.

(define fat+
   (lambda (x y)
      (if (zero? y)
          x
          (fat+ (1+ x) (1- y)))))

(define fatfib
   (lambda (x)
      (if (< x 2)
          1
          (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
