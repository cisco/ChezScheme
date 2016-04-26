(define read-string
  (lambda (ip)
    (unless (eqv? (read-char ip) #\")
      (error 'read-string "no starting double-quote"))
    (list->string
      (let f ()
        (let ([c (read-char ip)])
          (cond
            [(eqv? c #\") '()]
            [(or (eqv? c #\newline) (eof-object? c))
             (error 'read-string "no ending double-quote")]
            [else (cons c (f))]))))))

(define readrol
   (lambda (ip)
      (let ([c (read-char ip)])
         (if (eq? c #\newline)
             '()
             (cons c (readrol ip))))))

(define read-line
  (lambda (ip)
    (if (eof-object? (peek-char ip))
        (peek-char ip)
        (let ([x (read-string ip)])
          (cons x (readrol ip))))))

(define summary-read
   (lambda (ip)
      (do ([ls '() (cons line ls)]
           [line (read-line ip) (read-line ip)])
          ((eof-object? line) (reverse! ls)))))

(define summary-sort
   (lambda (x)
      (sort! (lambda (x y) (string<? (car x) (car y))) x)))

(define write-list
   (lambda (ls op)
      (unless (null? ls)
         (write-char (car ls) op)
         (write-list (cdr ls) op))))

(define summary-print
   (lambda (x op)
      (for-each
         (lambda (x) (write-list (cdr x) op) (newline op))
         x)))

(define summary-make
  (lambda (root)
    (parameterize ((reset-handler abort))
      (let ([ip (open-input-file (format "~a.rfm" root))]
            [op (open-output-file (format "~a.sfm" root) 'replace)])
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (fprintf op "\\begin{thesummary}~%")
            (summary-print
              (summary-sort (summary-read ip))
              op)
            (fprintf op "\\end{thesummary}~%"))
          (lambda ()
            (close-input-port ip)
            (close-output-port op)))))))
