(mat compile-profile
  (error? ; invalid argument
    (compile-profile 'src))
  (eqv?
    (parameterize ([compile-profile #t])
      (compile-profile))
    'source)
  (eqv?
    (parameterize ([compile-profile 'source])
      (compile-profile))
    'source)
  (eqv?
    (parameterize ([compile-profile 'block])
      (compile-profile))
    'block)
  (error? ; incorrect argument count
    (profile-dump '()))
  (error? ; incorrect argument count
    (profile-clear '()))
  (error? ; incorrect argument count
    (profile-dump-list #t '() 3))
  (error? ; invalid dump
    (profile-dump-list #f 17))
  (error? ; invalid dump
    (profile-dump-list #f '(17)))
  (error? ; invalid dump
    (profile-dump-list #f '((a . 17))))
  (error? ; invalid dump
    (profile-dump-list #f `((,(make-source-object (source-file-descriptor "abc" 34) 0 3) . q))))
  (error? ; incorrect argument count
    (profile-dump-html "" '() 3))
  (error? ; not a string
    (profile-dump-html '(prefix)))
  (error? ; invalid dump
    (profile-dump-html "profile" 17))
  (error? ; invalid dump
    (profile-dump-html "profile" '(17)))
  (error? ; invalid dump
    (profile-dump-html "profile" '((a . 17))))
  (error? ; invalid dump
    (profile-dump-html "profile" `((,(make-source-object (source-file-descriptor "abc" 34) 0 3) . q))))
  (error? ; incorrect argument count
    (profile-dump-data))
  (error? ; incorrect argument count
    (profile-dump-data "profile.data" '() 'q))
  (error? ; not a string
    (profile-dump-data #t))
  (error? ; invalid dump
    (profile-dump-data "profile.data" 17))
  (error? ; invalid dump
    (profile-dump-data "profile.data" '(17)))
  (error? ; invalid dump
    (profile-dump-data "profile.data" '((a . 17))))
  (error? ; invalid dump
    (profile-dump-data "profile.data" `((,(make-source-object (source-file-descriptor "abc" 34) 0 3) . q))))
  (error? ; not a string
    (profile-load-data 'what?))
  (eqv? (parameterize ([compile-profile #t])
          (compile
            '(let ()
               (define (f x) (if (= x 0) 1 (* x (f (- x 1)))))
               (f 3))))
        6)
  (eqv? (parameterize ([compile-profile #t])
          (compile
            '(let ()
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
               (fatfib 20))))
         10946)
  (equal?
    (parameterize ([compile-profile #t])
      (compile
        '(let ()
           (define $values (lambda (n) (lambda () (apply values (make-list n)))))
           (define foo
             (lambda (n)
               (call/cc
                 (lambda (k)
                   (with-exception-handler
                     (lambda (c) (collect) (k 'okay))
                     (lambda ()
                       (define f (case-lambda))
                       (let ([x (random 10)])
                         (call-with-values ($values n) f))))))))
           (list (foo 0) (foo 1) (foo 3) (foo 10) (foo 100) (foo 1000)))))
    '(okay okay okay okay okay okay))
  ; no longer recording (useless) profiling information when source file & position aren't available
  #;(let ([ls (profile-dump)])
    (and (list? ls)
      (not (null? ls))))
  (eqv? (profile-clear) (void))
  (or (eq? (compile-profile) 'source) (andmap zero? (map cdr (remp preexisting-profile-dump-entry? (profile-dump)))))
  (begin (set! cp-fatfib (void)) #t) ; release fatfib

  (begin (define $old-cp (compile-profile)) #t)
  ; this collect is here to make it more likely that we won't get a generation 1
  ; collection cementing in place the code that defines cp-fact
  (begin (collect 1) #t)
  (mat/cf (testfile "testfile")
    (eval-when (compile) (compile-profile 'source))
    (define (cp-fact x) (if (= x 0) 1 (* x (cp-fact (- x 1))))))
  (eq? (compile-profile) $old-cp)
  ; drop code that defines cp-fact so it won't show up in profile-dump-list in
  ; hopes of resolving potential issue with comparison to pdl further down
  (begin (collect (collect-maximum-generation)) #t)
  (= (cp-fact 10) 3628800)
  (begin
    (define (prefix=? prefix s)
      (let ([n (string-length prefix)])
        (and (>= (string-length s) n) (string=? (substring s 0 n) prefix))))
    (define (sdir? x) (or (prefix=? "../s" (cadr x)) (prefix=? "../unicode" (cadr x))))
    (define-values (pdl pdl2)
      (with-interrupts-disabled
        (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
          (values
            (remp sdir? (profile-dump-list #t (profile-dump)))
            (remp sdir? (profile-dump-list))))))
    #t)
  (equal? pdl pdl2)
  (not (null? pdl))
  (begin
    (rm-rf "testdir")
    (mkdir "testdir")
    (parameterize ([gensym-prefix 0]) (profile-dump-html "testdir/" (profile-dump)))
    #t)
  (file-exists? "testdir/profile.html")
  (file-exists? "testdir/testfile.ss.html")

  (begin (define $old-cp (compile-profile)) #t)
  (mat/cf (testfile "testfile-block")
    (eval-when (compile) (compile-profile 'block))
    (define (cp-fact-block x) (if (= x 0) 1 (* x (cp-fact-block (- x 1))))))
  (eq? (compile-profile) $old-cp)
  (= (cp-fact-block 10) 3628800)
  (or (equal? (compile-profile) 'source)
      (equal?
        (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
          (remp sdir? (profile-dump-list)))
        pdl))
  (begin
    (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
      (profile-dump-html))
    #t)
  (file-exists? "profile.html")
  (file-exists? "testfile.ss.html")
  (not (file-exists? "testfile2.ss.html"))

  (eqv? (profile-clear) (void))

  (mat/cf (testfile "testfile")
    (eval-when (compile) (compile-profile #t))
    (define (cp-fact x) (if (= x 0) 1 (* x (cp-fact (- x 1))))))
  (= (cp-fact 10) 3628800)
  (eqv? (profile-dump-data "testfile1.pd" (remp preexisting-profile-dump-entry? (profile-dump))) (void))
  (file-exists? "testfile1.pd")
  (eqv? (profile-load-data) (void))
  (eqv? (profile-load-data "testfile1.pd") (void))
  (begin
    (define $cp-ip (open-file-input-port "testfile.ss"))
    (define $cp-sfd (make-source-file-descriptor "testfile.ss" $cp-ip))
    (define $qw (lambda (bfp efp) (profile-query-weight (make-source-object $cp-sfd bfp efp))))
    #t)

  (eqv? (close-port $cp-ip) (void))

  (eqv? ($qw 0 0) 0.0) ; bfp, efp combination not in database
  (eqv? ; file not in database
    (let* ([fn (format "~a/Mf-base" *mats-dir*)]
           [ip (open-file-input-port fn)]
           [sfd (make-source-file-descriptor fn ip)])
      (close-port ip)
      (profile-query-weight (make-source-object sfd 0 0)))
    #f)
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0))) #t)) ($qw 0 42))
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0))) #t)) ($qw 43 102))
  (eqv? ($qw 63 101) 1.0)
  (eqv? ($qw 75 76) (fl/ 1.0 11.0))
  (eqv? ($qw 77 100) (fl/ 10.0 11.0))
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0))) #t)) ($qw 103 127))
  (eqv? ($qw 119 126) 0.0)
  (eqv? ($qw 120 125) 0.0)
  (eqv? (profile-clear) (void))
  (= (cp-fact 5) 120)
  (eqv? (profile-dump-data "testfile2.pd" (remp preexisting-profile-dump-entry? (profile-dump))) (void))
  (eqv? (profile-load-data "testfile2.pd") (void))
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0) (fl/ (/ 1.0 11.0) 2.0))) #t)) ($qw 0 42))
  (eqv? ($qw 21 40) 0.0)
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0) (fl/ (/ 1.0 11.0) 2.0))) #t)) ($qw 43 102))
  (eqv? ($qw 63 101) 1.0)
  (eqv? ($qw 75 76) (fl/ (fl+ (/ 1.0 11.0) (fl/ 1.0 6.0)) 2.0))
  (eqv? ($qw 77 100) (fl/ (fl+ (fl/ 10.0 11.0) (fl/ 5.0 6.0)) 2.0))
  ((lambda (x) (and (memv x (list 0.0 (/ 1.0 11.0) (fl/ (/ 1.0 11.0) 2.0))) #t)) ($qw 103 127))
  (eqv? ($qw 119 126) 0.0)
  (eqv? ($qw 120 125) 0.0)
  (eqv? (profile-clear) (void))

  ; make sure all is well when compiled with source profile info
  (mat/cf (testfile "testfile")
    (eval-when (compile) (compile-profile 'block))
    (define (cp-fact x) (if (= x 0) 1 (* x (cp-fact (- x 1))))))
  (eqv? (profile-dump-data "testfile3.pd" (remp preexisting-profile-dump-entry? (profile-dump))) (void))
  (file-exists? "testfile3.pd")
  (eqv? (profile-load-data "testfile3.pd") (void))
  ; and again with block profile info
  (mat/cf (testfile "testfile")
    (eval-when (compile) (compile-profile #f))
    (define (cp-fact x) (if (= x 0) 1 (* x (cp-fact (- x 1))))))
  (= (cp-fact 5) 120)

  (eqv? (profile-clear-database) (void))
  (eqv? ($qw 0 42) #f)
  (eqv? ($qw 77 100) #f)

  ; make sure record-ref, record-type, and record-cd are properly handled by
  ; find-source in pdhtml
  (mat/cf
    (eval-when (compile) (compile-profile #t))
    (library (A) (export make-foo foo? foo-x) (import (chezscheme)) (define-record-type foo (fields x)))
    (let ()
      (import (A))
      (define add-foo-xs
        (lambda ls
          (let f ([ls ls] [sum 0])
            (if (null? ls) sum (f (cdr ls) (+ (foo-x (car ls)) sum))))))
      ; make sure this is still around when we call profile-dump-list
      (set! $add-foo-xs add-foo-xs)
      (pretty-print (add-foo-xs (make-foo 1) (make-foo 2) (make-foo 3)))))
  (not (null? (profile-dump-list)))
  (eqv? (profile-clear) (void))
  (begin (set! $add-foo-xs #f) #t)

  (vector? (profile-palette))
  (vector?
    (parameterize ([profile-palette (vector-map
                                      (lambda (p) (cons "white" (car p)))
                                      (profile-palette))])
      (profile-palette)))
  (parameterize ([profile-palette
                  '#(("black" . "white")
                     ("red" . "white")
                     ("blue" . "black"))])
    (= (vector-length (profile-palette)) 3))
  (error? (profile-palette '#()))
  (error? (profile-palette '#(("black" . "white"))))
  (error? (profile-palette '#(("black" . "white") ("red" . "white"))))
  (error?
    (profile-palette
      '#(("black" . "white")
         #("red" "white")
         ("blue" . "black"))))
  (error?
    (profile-palette
      '#(("black" . "white")
         ("red" . "white")
         ("blue" . black))))
  (error?
    (profile-palette
      '#(("black" . "white")
         ("red" . "white")
         (#x0000ff . "black"))))
  ; test for proper counts in the presence of control operators
  (begin
    (define $return)
    (define $retry)
    (with-output-to-file "testfile-cp1.ss"
      (lambda ()
        (display-string "\
(define $frumble
  (lambda (ls)
    (if (null? ls)
        1
        (let ([n (car ls)])
          (if (eqv? n 0)
              (call/cc (lambda (k) (set! $retry k) ($return 0)))
              (let ([q ($frumble (cdr ls))])
                (add1 (* q n))))))))
"))
      'replace)
    (profile-clear)
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #t])
      (load "testfile-cp1.ss" compile))
    #t)
  (eqv?
    ($frumble (make-list 100 5))
    9860761315262647567646607066034827870915080438862787559628486633300781)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp1.ss")) (profile-dump-list))
    '((101 "testfile-cp1.ss" 36 258 3 5)
      (101 "testfile-cp1.ss" 40 50 3 9)
      (101 "testfile-cp1.ss" 41 46 3 10)
      (101 "testfile-cp1.ss" 47 49 3 16)
      (100 "testfile-cp1.ss" 69 257 5 9)
      (100 "testfile-cp1.ss" 78 86 5 18)
      (100 "testfile-cp1.ss" 79 82 5 19)
      (100 "testfile-cp1.ss" 83 85 5 23)
      (100 "testfile-cp1.ss" 99 256 6 11)
      (100 "testfile-cp1.ss" 103 113 6 15)
      (100 "testfile-cp1.ss" 104 108 6 16)
      (100 "testfile-cp1.ss" 109 110 6 21)
      (100 "testfile-cp1.ss" 111 112 6 23)
      (100 "testfile-cp1.ss" 193 255 8 15)
      (100 "testfile-cp1.ss" 202 221 8 24)
      (100 "testfile-cp1.ss" 203 211 8 25)
      (100 "testfile-cp1.ss" 212 220 8 34)
      (100 "testfile-cp1.ss" 213 216 8 35)
      (100 "testfile-cp1.ss" 217 219 8 39)
      (100 "testfile-cp1.ss" 240 254 9 17)
      (100 "testfile-cp1.ss" 241 245 9 18)
      (100 "testfile-cp1.ss" 246 253 9 23)
      (100 "testfile-cp1.ss" 247 248 9 24)
      (100 "testfile-cp1.ss" 249 250 9 26)
      (100 "testfile-cp1.ss" 251 252 9 28)
      (1 "testfile-cp1.ss" 0 260 1 1)
      (1 "testfile-cp1.ss" 19 259 2 3)
      (1 "testfile-cp1.ss" 59 60 4 9)
      (0 "testfile-cp1.ss" 128 178 7 15)
      (0 "testfile-cp1.ss" 129 136 7 16)
      (0 "testfile-cp1.ss" 137 177 7 24)
      (0 "testfile-cp1.ss" 149 164 7 36)
      (0 "testfile-cp1.ss" 162 163 7 49)
      (0 "testfile-cp1.ss" 165 176 7 52)
      (0 "testfile-cp1.ss" 166 173 7 53)
      (0 "testfile-cp1.ss" 174 175 7 61)))
  (eqv? 
    (call/cc
      (lambda (k)
        (set! $return k)
        (let ([ans ($frumble (append (make-list 50 5) (list 0) (make-list 50 7)))])
          ($return ans))))
    0)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp1.ss")) (profile-dump-list))
    '((152 "testfile-cp1.ss" 36 258 3 5)
      (152 "testfile-cp1.ss" 40 50 3 9)
      (152 "testfile-cp1.ss" 41 46 3 10)
      (152 "testfile-cp1.ss" 47 49 3 16)
      (151 "testfile-cp1.ss" 69 257 5 9)
      (151 "testfile-cp1.ss" 78 86 5 18)
      (151 "testfile-cp1.ss" 79 82 5 19)
      (151 "testfile-cp1.ss" 83 85 5 23)
      (151 "testfile-cp1.ss" 99 256 6 11)
      (151 "testfile-cp1.ss" 103 113 6 15)
      (151 "testfile-cp1.ss" 104 108 6 16)
      (151 "testfile-cp1.ss" 109 110 6 21)
      (151 "testfile-cp1.ss" 111 112 6 23)
      (150 "testfile-cp1.ss" 193 255 8 15)
      (150 "testfile-cp1.ss" 202 221 8 24)
      (150 "testfile-cp1.ss" 203 211 8 25)
      (150 "testfile-cp1.ss" 212 220 8 34)
      (150 "testfile-cp1.ss" 213 216 8 35)
      (150 "testfile-cp1.ss" 217 219 8 39)
      (100 "testfile-cp1.ss" 240 254 9 17)
      (100 "testfile-cp1.ss" 241 245 9 18)
      (100 "testfile-cp1.ss" 246 253 9 23)
      (100 "testfile-cp1.ss" 247 248 9 24)
      (100 "testfile-cp1.ss" 249 250 9 26)
      (100 "testfile-cp1.ss" 251 252 9 28)
      (1 "testfile-cp1.ss" 0 260 1 1)
      (1 "testfile-cp1.ss" 19 259 2 3)
      (1 "testfile-cp1.ss" 59 60 4 9)
      (1 "testfile-cp1.ss" 128 178 7 15)
      (1 "testfile-cp1.ss" 129 136 7 16)
      (1 "testfile-cp1.ss" 137 177 7 24)
      (1 "testfile-cp1.ss" 149 164 7 36)
      (1 "testfile-cp1.ss" 162 163 7 49)
      (1 "testfile-cp1.ss" 165 176 7 52)
      (1 "testfile-cp1.ss" 166 173 7 53)
      (1 "testfile-cp1.ss" 174 175 7 61)))
  (eqv?
    (call/cc
      (lambda (k)
        (set! $return k)
        ($retry 1)))
    111022302462515654042363166809082031)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp1.ss")) (profile-dump-list))
    '((152 "testfile-cp1.ss" 36 258 3 5)
      (152 "testfile-cp1.ss" 40 50 3 9)
      (152 "testfile-cp1.ss" 41 46 3 10)
      (152 "testfile-cp1.ss" 47 49 3 16)
      (151 "testfile-cp1.ss" 69 257 5 9)
      (151 "testfile-cp1.ss" 78 86 5 18)
      (151 "testfile-cp1.ss" 79 82 5 19)
      (151 "testfile-cp1.ss" 83 85 5 23)
      (151 "testfile-cp1.ss" 99 256 6 11)
      (151 "testfile-cp1.ss" 103 113 6 15)
      (151 "testfile-cp1.ss" 104 108 6 16)
      (151 "testfile-cp1.ss" 109 110 6 21)
      (151 "testfile-cp1.ss" 111 112 6 23)
      (150 "testfile-cp1.ss" 193 255 8 15)
      (150 "testfile-cp1.ss" 202 221 8 24)
      (150 "testfile-cp1.ss" 203 211 8 25)
      (150 "testfile-cp1.ss" 212 220 8 34)
      (150 "testfile-cp1.ss" 213 216 8 35)
      (150 "testfile-cp1.ss" 217 219 8 39)
      (150 "testfile-cp1.ss" 240 254 9 17)
      (150 "testfile-cp1.ss" 241 245 9 18)
      (150 "testfile-cp1.ss" 246 253 9 23)
      (150 "testfile-cp1.ss" 247 248 9 24)
      (150 "testfile-cp1.ss" 249 250 9 26)
      (150 "testfile-cp1.ss" 251 252 9 28)
      (1 "testfile-cp1.ss" 0 260 1 1)
      (1 "testfile-cp1.ss" 19 259 2 3)
      (1 "testfile-cp1.ss" 59 60 4 9)
      (1 "testfile-cp1.ss" 128 178 7 15)
      (1 "testfile-cp1.ss" 129 136 7 16)
      (1 "testfile-cp1.ss" 137 177 7 24)
      (1 "testfile-cp1.ss" 149 164 7 36)
      (1 "testfile-cp1.ss" 162 163 7 49)
      (1 "testfile-cp1.ss" 165 176 7 52)
      (1 "testfile-cp1.ss" 166 173 7 53)
      (1 "testfile-cp1.ss" 174 175 7 61)))
  (begin
    (collect (collect-maximum-generation)) ; drop code object for the define and lambda
    (profile-release-counters) ; drop proile information for the dropped code object
    #t)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp1.ss")) (profile-dump-list))
    '((152 "testfile-cp1.ss" 36 258 3 5)
      (152 "testfile-cp1.ss" 40 50 3 9)
      (152 "testfile-cp1.ss" 41 46 3 10)
      (152 "testfile-cp1.ss" 47 49 3 16)
      (151 "testfile-cp1.ss" 69 257 5 9)
      (151 "testfile-cp1.ss" 78 86 5 18)
      (151 "testfile-cp1.ss" 79 82 5 19)
      (151 "testfile-cp1.ss" 83 85 5 23)
      (151 "testfile-cp1.ss" 99 256 6 11)
      (151 "testfile-cp1.ss" 103 113 6 15)
      (151 "testfile-cp1.ss" 104 108 6 16)
      (151 "testfile-cp1.ss" 109 110 6 21)
      (151 "testfile-cp1.ss" 111 112 6 23)
      (150 "testfile-cp1.ss" 193 255 8 15)
      (150 "testfile-cp1.ss" 202 221 8 24)
      (150 "testfile-cp1.ss" 203 211 8 25)
      (150 "testfile-cp1.ss" 212 220 8 34)
      (150 "testfile-cp1.ss" 213 216 8 35)
      (150 "testfile-cp1.ss" 217 219 8 39)
      (150 "testfile-cp1.ss" 240 254 9 17)
      (150 "testfile-cp1.ss" 241 245 9 18)
      (150 "testfile-cp1.ss" 246 253 9 23)
      (150 "testfile-cp1.ss" 247 248 9 24)
      (150 "testfile-cp1.ss" 249 250 9 26)
      (150 "testfile-cp1.ss" 251 252 9 28)
      (1 "testfile-cp1.ss" 59 60 4 9)
      (1 "testfile-cp1.ss" 128 178 7 15)
      (1 "testfile-cp1.ss" 129 136 7 16)
      (1 "testfile-cp1.ss" 137 177 7 24)
      (1 "testfile-cp1.ss" 149 164 7 36)
      (1 "testfile-cp1.ss" 162 163 7 49)
      (1 "testfile-cp1.ss" 165 176 7 52)
      (1 "testfile-cp1.ss" 166 173 7 53)
      (1 "testfile-cp1.ss" 174 175 7 61)))
  ; test profiling with compiled files
  (begin
    (with-output-to-file "testfile-cp2.ss"
      (lambda ()
        (display-string "\
(define cp2-fib
  (rec fib
    (lambda (n)
      (cond
        [(fx= n 0) 1]
        [(fx= n 1) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))))
"))
      'replace)
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #t])
      (compile-file "testfile-cp2"))
    (profile-clear)
    (load "testfile-cp2.so")
    #t)
  (eqv? (cp2-fib 10) 89)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp2.ss")) (profile-dump-list))
    '((177 "testfile-cp2.ss" 49 146 4 7)
      (177 "testfile-cp2.ss" 64 73 5 10)
      (177 "testfile-cp2.ss" 65 68 5 11)
      (177 "testfile-cp2.ss" 69 70 5 15)
      (177 "testfile-cp2.ss" 71 72 5 17)
      (143 "testfile-cp2.ss" 86 95 6 10)
      (143 "testfile-cp2.ss" 87 90 6 11)
      (143 "testfile-cp2.ss" 91 92 6 15)
      (143 "testfile-cp2.ss" 93 94 6 17)
      (88 "testfile-cp2.ss" 113 144 7 15)
      (88 "testfile-cp2.ss" 114 115 7 16)
      (88 "testfile-cp2.ss" 116 129 7 18)
      (88 "testfile-cp2.ss" 117 120 7 19)
      (88 "testfile-cp2.ss" 121 128 7 23)
      (88 "testfile-cp2.ss" 122 123 7 24)
      (88 "testfile-cp2.ss" 124 125 7 26)
      (88 "testfile-cp2.ss" 126 127 7 28)
      (88 "testfile-cp2.ss" 130 143 7 32)
      (88 "testfile-cp2.ss" 131 134 7 33)
      (88 "testfile-cp2.ss" 135 142 7 37)
      (88 "testfile-cp2.ss" 136 137 7 38)
      (88 "testfile-cp2.ss" 138 139 7 40)
      (88 "testfile-cp2.ss" 140 141 7 42)
      (55 "testfile-cp2.ss" 96 97 6 20)
      (34 "testfile-cp2.ss" 74 75 5 20)
      (1 "testfile-cp2.ss" 0 149 1 1)
      (1 "testfile-cp2.ss" 18 148 2 3)
      (1 "testfile-cp2.ss" 23 26 2 8)
      (1 "testfile-cp2.ss" 31 147 3 5)))
  (begin
    (collect (collect-maximum-generation)) ; drop code object for the define and lambda
    (profile-release-counters) ; drop proile information for the dropped code object
    #t)
  (equal?
    (filter (lambda (x) (equal? (cadr x) "testfile-cp2.ss")) (profile-dump-list))
    '((177 "testfile-cp2.ss" 49 146 4 7)
      (177 "testfile-cp2.ss" 64 73 5 10)
      (177 "testfile-cp2.ss" 65 68 5 11)
      (177 "testfile-cp2.ss" 69 70 5 15)
      (177 "testfile-cp2.ss" 71 72 5 17)
      (143 "testfile-cp2.ss" 86 95 6 10)
      (143 "testfile-cp2.ss" 87 90 6 11)
      (143 "testfile-cp2.ss" 91 92 6 15)
      (143 "testfile-cp2.ss" 93 94 6 17)
      (88 "testfile-cp2.ss" 113 144 7 15)
      (88 "testfile-cp2.ss" 114 115 7 16)
      (88 "testfile-cp2.ss" 116 129 7 18)
      (88 "testfile-cp2.ss" 117 120 7 19)
      (88 "testfile-cp2.ss" 121 128 7 23)
      (88 "testfile-cp2.ss" 122 123 7 24)
      (88 "testfile-cp2.ss" 124 125 7 26)
      (88 "testfile-cp2.ss" 126 127 7 28)
      (88 "testfile-cp2.ss" 130 143 7 32)
      (88 "testfile-cp2.ss" 131 134 7 33)
      (88 "testfile-cp2.ss" 135 142 7 37)
      (88 "testfile-cp2.ss" 136 137 7 38)
      (88 "testfile-cp2.ss" 138 139 7 40)
      (88 "testfile-cp2.ss" 140 141 7 42)
      (55 "testfile-cp2.ss" 96 97 6 20)
      (34 "testfile-cp2.ss" 74 75 5 20)))
  (eqv? (profile-clear) (void))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define f (lambda () 0))))
      'replace)
    (parameterize ([compile-profile #t]) (load "testfile.ss" compile))
    #t)
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define f (lambda () 1))))
      'replace)
    #t)
  (eqv? (f) 0)
  (warning? ; unmodified source file not found
    (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
      (profile-dump-list)))
  (warning? ; unmodified source file not found
    (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
      (profile-dump-list #t)))
  (warning? ; unmodified source file not found
    (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
      (profile-dump-list #t (profile-dump))))
  (warning? ; unmodified source file not found
    (parameterize ([source-directories (cons* "../s" "../unicode" (source-directories))])
      (profile-dump-list #t (profile-dump))))
  (guard (c [else #f])
    (profile-dump-list #f)
    #t)
  (guard (c [else #f])
    (profile-dump-list #f (profile-dump))
    #t)
  (eqv? (profile-clear) (void))

  ; verify that annotations are preserved within syntax objects when
  ; profiling is enabled even when generation of inspector information
  ; is disabled.
  (begin
    (mkfile "testfile-ca3.ss"
      '(library (testfile-ca3) (export a) (import (chezscheme))
         (define-syntax a (lambda (x) #'(cons 0 1)))))
    (mkfile "testfile-cp3.ss"
      '(import (chezscheme) (testfile-ca3))
      '(do ([i 123 (fx- i 1)] [q #f a]) ((fx= i 0) (pretty-print q)))
      '(profile-dump-html))
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-inspector-information #f]
                        [compile-profile #t])
           (compile-library x)))
      'ca3)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-profile #t])
           (compile-program x)))
      'cp3)
    #t)
  (equal?
    (separate-eval
      '(load-program "testfile-cp3.so")
      '(cdr (find (lambda (x) (equal? (source-file-descriptor-path (source-object-sfd (car x))) "testfile-ca3.ss")) (profile-dump))))
    "(0 . 1)\n123\n")

  ; verify that we get profiling information for local macro transformers
  (begin
    (call-with-port (open-output-file "testfile-cp4.ss" 'replace)
      (lambda (op)
        (put-string op "\
(let ()
  (define-syntax a
    (lambda (q)
      (define square
        (lambda (n)
          (* n n)))
      (syntax-case q ()
        [(_ x (d ...) e)
         #`(let ([x (quote #,(map square (datum (d ...))))])
             e)])))
  (pretty-print (list (a b (8 6 7) b) (a b (5 3 0 9) (list b)))))")))
    (delete-file "testfile-cp4.so")
    (parameterize ([print-gensym #f] [current-eval compile] [compile-profile #t])
      (compile-file "testfile-cp4"))
    #t)
  (equal?
    (sort (lambda (x y) (< (list-ref x 2) (list-ref y 2)))
      (filter (lambda (x) (equal? (list-ref x 1) "testfile-cp4.ss"))
        (profile-dump-list)))
    '((1 "testfile-cp4.ss" 31 232 3 5)     ; first transformer count ...
      (2 "testfile-cp4.ss" 72 102 5 9)
      (7 "testfile-cp4.ss" 94 101 6 11)
      (7 "testfile-cp4.ss" 95 96 6 12)
      (7 "testfile-cp4.ss" 97 98 6 14)
      (7 "testfile-cp4.ss" 99 100 6 16)
      (2 "testfile-cp4.ss" 110 231 7 7)
      (2 "testfile-cp4.ss" 123 124 7 20)
      (2 "testfile-cp4.ss" 162 229 9 10)
      (2 "testfile-cp4.ss" 182 210 9 30)
      (2 "testfile-cp4.ss" 183 186 9 31)
      (2 "testfile-cp4.ss" 187 193 9 35)
      (2 "testfile-cp4.ss" 194 209 9 42)   ; ... last transformer count
      ))
  (begin
    (collect (collect-maximum-generation))
    (profile-release-counters)
    #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (revisit "testfile-cp4.so")))
    "((64 36 49) ((25 9 0 81)))\n")
  (equal?
    (sort (lambda (x y) (< (list-ref x 2) (list-ref y 2)))
      (filter (lambda (x) (equal? (list-ref x 1) "testfile-cp4.ss"))
        (profile-dump-list)))
    '((1 "testfile-cp4.ss" 0 299 1 1)      ; top-level let
      (1 "testfile-cp4.ss" 236 298 11 3)   ; pretty-print call ...
      (1 "testfile-cp4.ss" 237 249 11 4)   ; ... and subforms
      (1 "testfile-cp4.ss" 250 297 11 17)
      (1 "testfile-cp4.ss" 251 255 11 18)
      (1 "testfile-cp4.ss" 256 271 11 23)
      (1 "testfile-cp4.ss" 269 270 11 36)
      (1 "testfile-cp4.ss" 272 296 11 39)
      (1 "testfile-cp4.ss" 287 295 11 54)
      (1 "testfile-cp4.ss" 288 292 11 55)
      (1 "testfile-cp4.ss" 293 294 11 60)
      ))
)

(mat profile-form
  (error? ; invalid syntax
    (profile))
  (error? ; invalid syntax
    (profile 1 2 3))
  (error? ; not a source object
    (profile 3))
  (begin
    (define str "(ugh (if \x3b2;))")
    (define bv (string->utf8 str))
    (define ip (open-bytevector-input-port bv))
    (define sfd (make-source-file-descriptor "foo" ip #t))
    #t)
  (eq? (eval `(profile ,(make-source-object sfd 2 3))) (void))
  (begin
    (define compile-triv-file
      (lambda (ifn ofn)
        (define insert-profile-forms
          (lambda (x)
            (unless (annotation? x) (errorf 'compile-triv-file "expected an annotation, got ~s" x))
            (let ([src (annotation-source x)] [exp (annotation-expression x)])
              `(begin (profile ,src)
                 ,(syntax-case exp ()
                    [(?do-times n e)
                      (eq? (annotation-expression #'?do-times) 'do-times)
                      (let ([n (annotation-expression #'n)])
                        `(do ([i ,n (fx- i 1)]) ((fx= i 0)) ,(insert-profile-forms #'e)))]
                    [(?print string)
                      (eq? (annotation-expression #'?print) 'print)
                      `(printf "~a\n" ,(annotation-expression #'string))]
                    [else (syntax-error exp)])))))
        (define parse
          (lambda (ifn)
            (let ([ip (open-file-input-port ifn)])
              (let ([sfd (make-source-file-descriptor ifn ip #t)])
                (let ([ip (transcoded-port ip (native-transcoder))])
                  (let f ([bfp 0])
                    (let-values ([(x bfp) (get-datum/annotations ip sfd bfp)])
                      (if (eof-object? x)
                          (begin (close-port ip) '())
                          (cons x (f bfp))))))))))
        (parameterize ([compile-profile 'source] [generate-profile-forms #f])
          (compile-to-file (list `(define (triv) ,@(map insert-profile-forms (parse ifn)))) ofn))))
    #t)
  (begin
    (with-output-to-file "testfile-triv.ss"
      (lambda ()
        (pretty-print '(do-times 10 (print "hello")))
        (pretty-print '(do-times 5 (print "goodbye"))))
      'replace)
    (compile-triv-file "testfile-triv.ss" "testfile-triv.so")
    (load "testfile-triv.so")
    #t)
  (equal?
    (with-output-to-string triv)
    "hello\nhello\nhello\nhello\nhello\nhello\nhello\nhello\nhello\nhello\ngoodbye\ngoodbye\ngoodbye\ngoodbye\ngoodbye\n")
  (equal?
    (sort
      ; sort by bfp
      (lambda (x y) (< (list-ref x 2) (list-ref y 2)))
      (filter (lambda (x) (equal? (list-ref x 1) "testfile-triv.ss")) (profile-dump-list)))
    '((1 "testfile-triv.ss" 0 29 1 1)
      (10 "testfile-triv.ss" 13 28 1 14)
      (1 "testfile-triv.ss" 30 60 2 1)
      (5 "testfile-triv.ss" 42 59 2 13)))
  (eqv? (profile-clear) (void))
)

(mat coverage
  (begin
    (mkfile "testfile.ss" '(printf "hello\n"))
    (define $ct0 (make-source-table))
    (define $ct0-src1
      (make-source-object
        (call-with-port (open-file-input-port "testfile.ss")
          (lambda (ip)
            (make-source-file-descriptor "testfile.ss" ip #t)))
        3 7))
    (define $ct0-src2
      (make-source-object
        (call-with-port (open-file-input-port "testfile.ss")
          (lambda (ip)
            (make-source-file-descriptor "testfile.ss" ip #t)))
        5 11))
    (define $ct0-src3
      (make-source-object
        (call-with-port (open-file-input-port "testfile.ss")
          (lambda (ip)
            (make-source-file-descriptor "not-testfile.ss" ip #t)))
        17 19))
    #t)
  (source-table? $ct0)
  (= (source-table-size $ct0) 0)
  (not (source-table-contains? $ct0 $ct0-src1))
  (eq? (source-table-ref $ct0 $ct0-src2 'q) 'q)
  (begin
    (source-table-set! $ct0 $ct0-src1 17)
    #t)
  (= (source-table-size $ct0) 1)
  (source-table-contains? $ct0 $ct0-src1)
  (not (source-table-contains? $ct0 $ct0-src2))
  (eq? (source-table-ref $ct0 $ct0-src3 'q) 'q)
  (begin
    (source-table-set! $ct0 $ct0-src2 37)
    (source-table-set! $ct0 $ct0-src3 43)
    #t)
  (= (source-table-size $ct0) 3)
  (source-table-contains? $ct0 $ct0-src1)
  (source-table-contains? $ct0 $ct0-src2)
  (source-table-contains? $ct0 $ct0-src3)
  (eqv? (source-table-ref $ct0 $ct0-src1 'q) 17)
  (eqv? (source-table-ref $ct0 $ct0-src2 'q) 37)
  (eqv? (source-table-ref $ct0 $ct0-src3 'q) 43)
  (let ([a (source-table-cell $ct0 $ct0-src1 #f)])
    (and (eqv? (cdr a) 17)
         (begin
           (set-cdr! a 23)
           #t)))
  (= (source-table-size $ct0) 3)
  (source-table-contains? $ct0 $ct0-src1)
  (source-table-contains? $ct0 $ct0-src2)
  (source-table-contains? $ct0 $ct0-src3)
  (eqv? (source-table-ref $ct0 $ct0-src1 'q) 23)
  (eqv? (source-table-ref $ct0 $ct0-src2 'q) 37)
  (eqv? (source-table-ref $ct0 $ct0-src3 'q) 43)
  (eqv? (source-table-delete! $ct0 $ct0-src1) (void))
  (= (source-table-size $ct0) 2)
  (not (source-table-contains? $ct0 $ct0-src1))
  (source-table-contains? $ct0 $ct0-src2)
  (source-table-contains? $ct0 $ct0-src3)
  (eqv? (source-table-delete! $ct0 $ct0-src3) (void))
  (= (source-table-size $ct0) 1)
  (not (source-table-contains? $ct0 $ct0-src1))
  (source-table-contains? $ct0 $ct0-src2)
  (not (source-table-contains? $ct0 $ct0-src3))
  (eqv? (source-table-delete! $ct0 $ct0-src2) (void))
  (= (source-table-size $ct0) 0)
  (not (source-table-contains? $ct0 $ct0-src1))
  (not (source-table-contains? $ct0 $ct0-src2))
  (not (source-table-contains? $ct0 $ct0-src3))
  (begin
    (define $source-table-filter
      (lambda (universe-ct ct)
        (let ([new-ct (make-source-table)])
          (for-each
            (lambda (p)
              (let ([src (car p)] [count (cdr p)])
                (when (source-table-contains? universe-ct src)
                  (source-table-set! new-ct src count))))
            (source-table-dump ct))
          new-ct)))
    (begin
      (mkfile "testfile-coverage1a.ss"
        '(library (testfile-coverage1a) (export a f) (import (chezscheme))
           (define-syntax a (lambda (x) #'(cons 0 1)))
           (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1))))))))
      (parameterize ([generate-covin-files #t] [compile-profile #t])
        (compile-library "testfile-coverage1a")))
    (begin
      (mkfile "testfile-coverage1b.ss"
        `(top-level-program
           (import (chezscheme) (testfile-coverage1a))
           (do ([i 3 (fx- i 1)])
               ((fx= i 0) (printf "~s\n" (f 3)))
             (printf "a = ~s\n" a))))
      (call-with-port (open-file-input-port "testfile-coverage1b.ss")
        (lambda (ip)
          (let ([sfd (make-source-file-descriptor "testfile-coverage1b.ss" ip #t)])
            (call-with-port (transcoded-port ip (native-transcoder))
              (lambda (ip)
                (call-with-port (open-file-output-port "testfile-coverage1b.so" (file-options replace))
                  (lambda (op)
                    (call-with-port (open-output-file "testfile-coverage1b.covin" 'replace)
                      (lambda (covop)
                        (parameterize ([compile-profile #t])
                          (compile-port ip op sfd #f covop))))))))))))
    (begin
      (mkfile "testfile-coverage1c.ss"
        '(top-level-program
           (import (chezscheme) (testfile-coverage1a))
           (do ([i 4 (fx- i 1)])
               ((fx= i 0) (printf "~s\n" (f 4)))
             (printf "a = ~s\n" a))))
      (call-with-port (open-file-input-port "testfile-coverage1c.ss")
        (lambda (ip)
          (let ([sfd (make-source-file-descriptor "testfile-coverage1c.ss" ip #t)])
            (call-with-port (transcoded-port ip (native-transcoder))
              (lambda (ip)
                (call-with-port (open-file-output-port "testfile-coverage1c.so" (file-options replace))
                  (lambda (op)
                    (call-with-port (open-output-file "testfile-coverage1c.covin" 'replace)
                      (lambda (covop)
                        (parameterize ([compile-profile #t])
                          (let-values ([(x fp) (get-datum/annotations ip sfd 0)])
                            (compile-to-port (list x) op sfd #f covop)))))))))))))
    (begin
      (mkfile "testfile-coverage1d.ss"
        '(import (chezscheme) (testfile-coverage1a))
        '(do ([i 3 (fx- i 1)])
             ((fx= i 0) (printf "~s\n" (f 5)))
           (printf "a = ~s\n" a)))
      (parameterize ([generate-covin-files #t] [compile-profile #t])
        (compile-program "testfile-coverage1d")))
    (define $ct0
      (let ()
        (define (with-source-input-port path p)
          (call-with-port
            (open-file-input-port path
              (file-options compressed)
              (buffer-mode block)
              (current-transcoder))
            p))
        (let ([ct (make-source-table)])
          (with-source-input-port "testfile-coverage1b.covin" (lambda (ip) (get-source-table! ip ct)))
          (with-source-input-port "testfile-coverage1c.covin" (lambda (ip) (get-source-table! ip ct (lambda (x y) (assert (= x y 0)) x))))
          ct)))
    #t)
  (source-table? $ct0)
  (andmap zero? (map cdr (source-table-dump $ct0)))
  (call-with-values
    (lambda ()
      (with-profile-tracker
        (lambda ()
          (call/cc
            (lambda (k)
              (values k
                (with-output-to-string
                  (lambda ()
                    (load-program "testfile-coverage1b.so")
                    (load-program "testfile-coverage1c.so")
                    (load-program "testfile-coverage1d.so")))))))))
    (lambda (ct k s)
      (let* ([ct ($source-table-filter $ct0 ct)])
        (if k
            (and (string=? s "a = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n6\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n24\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n120\n")
                 (procedure? k)
                 (begin
                   (set! $ct1 ct)
                   (k #f "yup.")))
            (and (string=? s "yup.")
                 (begin
                   (set! $ct2 ct)
                   #t))))))
  (source-table? $ct1)
  (source-table? $ct2)
  (and
    (andmap
      (lambda (dumpit)
        (and (source-table-contains? $ct2 (car dumpit))
             (>= (source-table-ref $ct2 (car dumpit) #f) (cdr dumpit))))
      (source-table-dump $ct1))
    (andmap
      (lambda (dumpit)
        (and (source-table-contains? $ct1 (car dumpit))
             (<= (source-table-ref $ct1 (car dumpit) #f) (cdr dumpit))))
      (source-table-dump $ct2)))
  (not (ormap zero? (map cdr (source-table-dump $ct1))))
  (let ([dump (source-table-dump $ct1)])
    (define (file-found? path)
      (ormap
        (lambda (dumpit)
          (string=? (source-file-descriptor-path (source-object-sfd (car dumpit))) path))
        dump))
    (and (file-found? "testfile-coverage1a.ss")
         (file-found? "testfile-coverage1b.ss")
         (file-found? "testfile-coverage1c.ss")
         (not (file-found? "testfile-coverage1d.ss"))))
    (string=?
      (with-output-to-string
        (lambda ()
          ; shouldn't matter whether this is before or after the with-profile-tracker call
          (load-program "testfile-coverage1b.so")
          (let-values ([(ct . ignore) (with-profile-tracker #t
                                        (lambda ()
                                          (load-program "testfile-coverage1c.so")
                                          (load-program "testfile-coverage1d.so")))])
            (let ([ct ($source-table-filter $ct0 ct)])
              (set! $ct3 ct)))))
      "a = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n6\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n24\na = (0 . 1)\na = (0 . 1)\na = (0 . 1)\n120\n")
  (source-table? $ct3)
  (let ([dump (source-table-dump $ct3)])
    (define (file-found? path)
      (ormap
        (lambda (dumpit)
          (string=? (source-file-descriptor-path (source-object-sfd (car dumpit))) path))
        dump))
    (and (file-found? "testfile-coverage1a.ss")
         (file-found? "testfile-coverage1b.ss")
         (file-found? "testfile-coverage1c.ss")
         (not (file-found? "testfile-coverage1d.ss"))))
  ; the coverage table retrieved should include counts for both sets of load-program calls
  (and
    (andmap
      (lambda (dumpit)
        (>= (source-table-ref $ct3 (car dumpit) #f) (* 2 (cdr dumpit))))
      (source-table-dump $ct1))
    (andmap
      (lambda (dumpit)
        (<= (* 2 (source-table-ref $ct1 (car dumpit) #f)) (cdr dumpit)))
      (source-table-dump $ct3)))
  (begin
    (call-with-output-file "testfile.covout"
      (lambda (op)
        (put-source-table op $ct3))
      'replace)
    (define $ct5
      (let ([ct (make-source-table)])
        (call-with-input-file "testfile.covout" (lambda (ip) (get-source-table! ip ct)))
        ct))
    #t)
  (andmap
    (lambda (dumpit)
      (= (source-table-ref $ct5 (car dumpit) #f) (cdr dumpit)))
    (source-table-dump $ct3))
  (andmap
    (lambda (dumpit)
      (= (source-table-ref $ct3 (car dumpit) #f) (cdr dumpit)))
    (source-table-dump $ct5))
  (begin
    (call-with-input-file "testfile.covout" (lambda (ip) (get-source-table! ip $ct5 (lambda (x y) (- (* x y))))))
    #t)
  (andmap
    (lambda (dumpit)
      (= (source-table-ref $ct5 (car dumpit) #f) (- (expt (cdr dumpit) 2))))
    (source-table-dump $ct3))
)
