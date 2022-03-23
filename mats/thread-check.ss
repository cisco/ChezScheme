    (define $threads (foreign-procedure "(cs)threads" () scheme-object))
    (define $nthreads 1)
    (define $yield
      (let ([t (make-time 'time-duration 1000000 0)])
        (lambda () (sleep t))))
    (define $thread-check
      (lambda ()
        (let loop ([n 100] [nt (length ($threads))])
          (cond
            [(<= nt $nthreads)
             (set! $nthreads nt)
             (collect)]
            [else
             ($yield)
             (let* ([ls ($threads)] [nnt (length ls)])
               (cond
                 [(< nnt nt) (loop n nnt)]
                 [(= n 0)
                  (set! $nthreads nnt)
                  (errorf #f "extra threads running ~s" ls)]
                 [else (loop (- n 1) nnt)]))]))
        #t))
