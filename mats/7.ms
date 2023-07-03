;;; 7.ms
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; section 7-1:

(mat load/compile-file
   (error? (load "/file/not/there"))
   (error? (compile-file "/file/not/there"))
   (error? ; abc is not a string
     (load-program 'abc))
   (error? ; xxx is not a procedure
     (load-program "/file/not/there" 'xxx))
   (error? ; 3 is not a string
     (parameterize ([source-directories '("/tmp" ".")]) (load-program 3)))
   (error? ; 3 is not a string
     (parameterize ([source-directories '("/tmp" ".")]) (load-program 3 values)))
   (not (top-level-bound? 'aaaaa))
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display "(let ((x 3) (y 4)) (set! aaaaa (+ x y)))" p)
      (close-output-port p)
      (load "testfile.ss")
      (eqv? aaaaa 7))
   (call/cc
      (lambda (k)
         (load "testfile.ss"
               (lambda (x)
                  (unless (equal? (annotation-stripped x)
                            '(let ((x 3) (y 4))
                               (set! aaaaa (+ x y))))
                    (k #f))))
         #t))
   (begin
     (printf "***** expect \"compile-file\" message:~%")
     (compile-file "testfile")
     (set! aaaaa 0)
     (load "testfile.so")
     (eqv? aaaaa 7))
   (parameterize ([fasl-compressed #f])
     (printf "***** expect \"compile-file\" message:~%")
     (compile-file "testfile")
     (set! aaaaa 0)
     (load "testfile.so")
     (eqv? aaaaa 7))
   (let ((ip (open-input-string "(let ((x -3) (y -4)) (set! aaaaa (+ x y)))"))
         (op (open-file-output-port "testfile.so" (file-options replace))))
     (compile-port ip op)
     (close-input-port ip)
     (close-port op)
     (set! aaaaa 0)
     (load "testfile.so")
     (eqv? aaaaa -7))
   (let ((ip (open-input-string "(let ((x -3) (y -4)) (set! aaaaa (+ x y)))"))
         (op (open-file-output-port "testfile.so" (file-options replace #;compressed))))
     (compile-port ip op)
     (close-input-port ip)
     (close-port op)
     (set! aaaaa 0)
     (load "testfile.so")
     (eqv? aaaaa -7))
  ; test compiling a file containing most-negative-fixnum
   (let ([p (open-output-file "testfile.ss" 'replace)])
     (printf "***** expect \"compile-file\" message:~%")
     (display `(define $mnfixnum ,(most-negative-fixnum)) p)
     (close-output-port p)
     (compile-file "testfile")
     (load "testfile.so")
     (eqv? $mnfixnum (most-negative-fixnum)))
 )

(mat compile-to-port
  (eqv?
    (call-with-port (open-file-output-port "testfile.so" (file-options replace))
      (lambda (op)
        (compile-to-port '((define ctp1 'hello) (set! ctp1 (cons 'goodbye ctp1))) op)))
    (void))
  (begin
    (load "testfile.so")
    #t)
  (equal? ctp1 '(goodbye . hello))
  (begin
    (with-output-to-file "testfile-ctp2a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ctp2a) (export fact) (import (chezscheme))
             (define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))))))
      'replace)
    #t)
  (equal?
    (call-with-port (open-file-output-port "testfile.so" (file-options replace #;compressed))
      (lambda (op)
        (parameterize ([compile-imported-libraries #t])
          (compile-to-port
            '((top-level-program
                (import (chezscheme) (testfile-ctp2a))
                (pretty-print (fact 3))))
            op))))
    '((testfile-ctp2a)))
  (equal?
    (with-output-to-string (lambda () (load "testfile.so")))
    "6\n")
 )

(mat load-compiled-from-port
  (let ()
    (define-values (o get) (open-bytevector-output-port))
    (compile-to-port '((define lcfp1 'worked) 'loaded) o)
    (eq? 'loaded (load-compiled-from-port (open-bytevector-input-port (get)))))
  (begin
    (define lcfp-bv
      (let-values ([(o get) (open-bytevector-output-port)])
        (compile-to-port
          '((printf "revisit\n")
            (define-syntax $lcfp-a (begin (printf "visit\n") (lambda (x) 0)))
            (eval-when (visit revisit) (printf "visit-revisit\n"))
            (eval-when (visit) 'visit-return)
            'revisit-return)
          o)
        (get)))
    #t)
  (equal?
    (with-output-to-string (lambda () (printf "result = ~s\n" (load-compiled-from-port (open-bytevector-input-port lcfp-bv)))))
    "revisit\nvisit\nvisit-revisit\nresult = revisit-return\n")
  (equal?
    (with-output-to-string (lambda () (printf "result = ~s\n" (visit-compiled-from-port (open-bytevector-input-port lcfp-bv)))))
    "visit\nvisit-revisit\nresult = visit-return\n")
  (equal?
    (with-output-to-string (lambda () (printf "result = ~s\n" (revisit-compiled-from-port (open-bytevector-input-port lcfp-bv)))))
    "revisit\nvisit-revisit\nresult = revisit-return\n")

  
  (let ()
    (define-values (o get) (open-bytevector-output-port))
    (compile-to-port '((lambda () 'banana)) o #f #f #f (machine-type) #f (lambda (v) (eq? v 'banana)))
    (eq? 'apple ((load-compiled-from-port (open-bytevector-input-port (get)) '#(apple)))))
)

(mat compile-to-file
  (begin
    (delete-file (format "testfile.~s" (machine-type)))
    (compile-to-file '((define ctf1 'hello) (set! ctf1 (cons ctf1 'goodbye))) "testfile.so")
    #t)
  (begin
    (load "testfile.so")
    #t)
  ;; NB: should we protect the following in case we are actually cross compiling?
  (not (file-exists? (format "testfile.~s" (machine-type))))
  (equal? ctf1 '(hello . goodbye))
  (begin
    (with-output-to-file "testfile-ctf2a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ctf2a) (export fib) (import (chezscheme))
             (define fib (lambda (x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))))))
      'replace)
    #t)
  (equal?
    (parameterize ([compile-imported-libraries #t])
      (compile-to-file
        '((top-level-program
            (import (chezscheme) (testfile-ctf2a))
            (pretty-print (fib 11))))
        "testfile.so"))
    '((testfile-ctf2a)))
  (not (file-exists? (format "testfile-ctf2a.~s" (machine-type))))
  (not (file-exists? (format "testfile.~s" (machine-type))))
  (equal?
    (with-output-to-string (lambda () (load "testfile.so")))
    "89\n")
  (begin
    (compile-to-file
      '((library (testfile-ctf2a) (export fib) (import (chezscheme))
          (define fib (lambda (x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2))))))))
        "testfile.so")
    #t)
  (not (file-exists? (format "testfile.~s" (machine-type))))
 )

(mat compile-script
  (error? (compile-script "/file/not/there"))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "#! /usr/bin/scheme --script\n")
        (pretty-print '(define $cs-x 14))
        (pretty-print '(define $cs-y (lambda (q) (+ $cs-x q)))))
      'replace)
    (compile-script "testfile")
    #t)
  (error? $cs-x)
  (error? $cs-y)
  (begin
    (load "testfile.so")
    #t)
  (eqv? $cs-x 14)
  (eqv? ($cs-y -17) -3)
  (eqv? (with-input-from-file "testfile.so" read-char) #\#)

 ; test visit/revisit of compiled script
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (printf "#! /usr/bin/scheme --script\n")
        (pretty-print '(eval-when (visit) (display "hello from testfile\n")))
        (pretty-print '(display "hello again from testfile\n")))
      'replace)
    (compile-script "testfile")
    #t)
  (equal?
    (with-output-to-string
      (lambda () (visit "testfile.so")))
    "hello from testfile\n")
  (equal?
    (with-output-to-string
      (lambda () (revisit "testfile.so")))
    "hello again from testfile\n")
  (equal?
    (with-output-to-string
      (lambda () (load "testfile.so")))
    "hello from testfile\nhello again from testfile\n")
)

(mat load-program/compile-program
  (error? (compile-program "/file/not/there"))
  (error? (load-program "/file/not/there"))
  (error? ; abc is not a string
    (load-program 'abc))
  (error? ; xxx is not a procedure
    (load-program "/file/not/there" 'xxx))
  (error? ; 3 is not a string
    (parameterize ([source-directories '("/tmp" ".")]) (load-program 3)))
  (error? ; 3 is not a string
    (parameterize ([source-directories '("/tmp" ".")]) (load-program 3 values)))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "#! /usr/bin/scheme --program\n")
        (pretty-print '(import (rnrs)))
        (pretty-print '(define $cp-x 14))
        (pretty-print '(define $cp-y (lambda (q) (+ $cp-x q))))
        (pretty-print '(begin
                         (when (file-exists? "testfile-cp.ss")
                           (delete-file "testfile-cp.ss"))
                         (with-output-to-file "testfile-cp.ss"
                           (lambda () (write (cons $cp-x ($cp-y 35))))))))
      'replace)
    (compile-program "testfile")
    #t)
  (begin
    (load-program "testfile.so")
    #t)
  (error? $cp-x)
  (error? $cp-y)
  (let ([p (with-input-from-file "testfile-cp.ss" read)])
    (eqv? (car p) 14)
    (eqv? (cdr p) 49))
  (eqv? (with-input-from-file "testfile.so" read-char) #\#)

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "#! /usr/bin/scheme --program\n")
        (pretty-print '(import (rnrs)))
        (pretty-print '(begin
                         (when (file-exists? "testfile-cp.ss")
                           (delete-file "testfile-cp.ss"))
                         (with-output-to-file "testfile-cp.ss"
                           (lambda () (write "hello from testfile"))))))
      'replace)
    #t)
  (begin
    (load-program "testfile.ss")
    #t)
  (equal? (with-input-from-file "testfile-cp.ss" read) "hello from testfile")

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "#! /usr/bin/scheme --program\n")
        (pretty-print '(import (rnrs)))
        (pretty-print '(pretty-print 'hello)))
      'replace)
    #t)
  (error? ; unbound variable pretty-print
    (compile-program "testfile"))
  (error? ; unbound variable pretty-print
    (load-program "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "#! /usr/bin/scheme --program\n")
        (pretty-print '(import (rnrs)))
        (pretty-print '(#%write 'hello)))
      'replace)
    #t)
  (error? ; invalid #% syntax in #!r6rs mode
    (compile-program "testfile"))
  (error? ; invalid #% syntax in #!r6rs mode
    (load-program "testfile.ss"))
)

(mat maybe-compile
  (error? ; not a procedure
    (compile-program-handler 'ignore))
  (procedure? (compile-program-handler))
  (error? ; not a string
    (maybe-compile-file '(spam)))
  (error? ; not a string
    (maybe-compile-file "spam" 'spam))
  (error? ; not a string
    (maybe-compile-file -2.5 "spam"))
  (error? ; .ss file does not exist
    (maybe-compile-file "probably-does-not-exist.ss"))
  (error? ; .ss file does not exist
    (maybe-compile-file "probably-does-not-exist.ss" "probably-does-not-exist.so"))
  (begin
    (with-output-to-file "testfile-mc.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme))
            (pretty-print 'hello))))
      'replace)
    #t)
  (error? ; cannot create .so file
    (maybe-compile-file "testfile-mc.ss" "/probably/does/not/exist.so"))
  (error? ; not a string
    (maybe-compile-program '(spam)))
  (error? ; not a string
    (maybe-compile-program "spam" 'spam))
  (error? ; not a string
    (maybe-compile-program -2.5 "spam"))
  (error? ; .ss file does not exist
    (maybe-compile-program "probably-does-not-exist.ss"))
  (error? ; .ss file does not exist
    (maybe-compile-program "probably-does-not-exist.ss" "probably-does-not-exist.so"))
  (begin
    (with-output-to-file "testfile-mc.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme))
            (pretty-print 'hello))))
      'replace)
    #t)
  (error? ; cannot create .so file
    (maybe-compile-program "testfile-mc.ss" "/probably/does/not/exist.so"))
  (error? ; not a string
    (maybe-compile-library '(spam)))
  (error? ; not a string
    (maybe-compile-library "spam" 'spam))
  (error? ; not a string
    (maybe-compile-library -2.5 "spam"))
  (error? ; .ss file does not exist
    (maybe-compile-library "probably-does-not-exist.ss"))
  (error? ; .ss file does not exist
    (maybe-compile-library "probably-does-not-exist.ss" "probably-does-not-exist.so"))
  (begin
    (with-output-to-file "testfile-mc.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc) (export) (import))))
      'replace)
    #t)
  (error? ; cannot create .so file
    (maybe-compile-library "testfile-mc.ss" "/probably/does/not/exist.so"))
  (begin
    (with-output-to-file "testfile-mc.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme))
            (if))))
      'replace)
    #t)
  (error? ; syntax error
    (maybe-compile-file "testfile-mc.ss" "testfile-mc.so"))
  (not (file-exists? "testfile-mc.so"))
  (error? ; syntax error
    (maybe-compile-program "testfile-mc.ss" "testfile-mc.so"))
  (not (file-exists? "testfile-mc.so"))
  (begin
    (with-output-to-file "testfile-mc.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc) (export x) (import (chezscheme)) (define))))
      'replace)
    #t)
  (error? ; syntax error
    (maybe-compile-library "testfile-mc.ss" "testfile-mc.so"))
  (not (file-exists? "testfile-mc.so"))
  (begin
    (for-each delete-file '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
    (with-output-to-file "testfile-mc-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-a) (export a) (import (chezscheme)) (define a "a"))))
      'replace)
    (with-output-to-file "testfile-mc-b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-b) (export a b) (import (chezscheme) (testfile-mc-a)) (define b "b"))))
      'replace)
    (with-output-to-file "testfile-mc-c.ss"
      (lambda ()
        (pretty-print
          '(define c "c")))
      'replace)
    (with-output-to-file "testfile-mc-foo.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme) (testfile-mc-b))
            (include "testfile-mc-c.ss")
            (pretty-print (list a b c)))))
      'replace)
    (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = =))
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) 'mc-b)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = =))
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) 'mc-a)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = =))
  (touch "testfile-mc-foo.so" "testfile-mc-foo.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = >))
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (touch "testfile-mc-foo.so" "testfile-mc-c.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = >))
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (touch "testfile-mc-foo.so" "testfile-mc-b.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= > >))
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (touch "testfile-mc-foo.so" "testfile-mc-a.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (let ([s (separate-compile '(lambda (x) (parameterize ([compile-program-handler (lambda (ifn ofn) (printf "yippee!\n") (compile-program ifn ofn))]
                                                             [compile-imported-libraries #t]
                                                             [compile-file-message #f])
                                                 (maybe-compile-program x)))
                 'mc-foo)])
        (cons
          (map
            (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
            (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
            mt*)
          s)))
    '((> > >) . "yippee!\n"))
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (let ([s (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t] [compile-file-message #f]) (maybe-compile-program x))) 'mc-foo)])
        (cons
          (map
            (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
            (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
            mt*)
          s)))
    '((= = =) . ""))
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (touch "testfile-mc-foo.so" "testfile-mc-b.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) 'mc-b)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= > =))
  ; NB: create testfile-mc-a.ss newer than testfile-mc-1b.so, since testfile-mc-1b.so might be newer than testfile-mc-foo.so
  (touch "testfile-mc-b.so" "testfile-mc-a.ss")
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (let ([s (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f] [import-notify #t]) (maybe-compile-library x))) 'mc-b)])
        (cons
          (map
            (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
            (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
            mt*)
          s)))
    '((= = =) . "maybe-compile-library: object file is not older\nmaybe-compile-library: did not find source file \"testfile-mc-a.chezscheme.sls\"\nmaybe-compile-library: found source file \"testfile-mc-a.ss\"\nmaybe-compile-library: found corresponding object file \"testfile-mc-a.so\"\n"))
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) 'mc-b)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(> > =))
  (equal?
    (let ([mt* (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-program x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time (list "testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-foo.so"))
        mt*))
    '(= = >))
  (equal?
    (separate-eval '(load-program "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\")\n")
  (begin
    (for-each delete-file '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
    (with-output-to-file "testfile-mc-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-a) (export a) (import (chezscheme)) (define a "a"))))
      'replace)
    (with-output-to-file "testfile-mc-b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-b) (export a b) (import (chezscheme) (testfile-mc-a)) (define b "b"))))
      'replace)
    (with-output-to-file "testfile-mc-c.ss"
      (lambda ()
        (pretty-print
          '(define c "c")))
      'replace)
    (with-output-to-file "testfile-mc-d.ss"
      (lambda ()
        (pretty-print
          '(module M (d)
             (import (testfile-mc-a) (testfile-mc-b) (chezscheme))
             (define d (vector b a)))))
      'replace)
    (with-output-to-file "testfile-mc-e.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-e) (export e-str) (import (chezscheme)) (define e-str "e"))))
      'replace)
    (with-output-to-file "testfile-mc-e-import.ss"
      (lambda ()
        (pretty-print
          '(import (testfile-mc-e))))
      'replace)
    (with-output-to-file "testfile-mc-f.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mc-f) (export f-str) (import (chezscheme)) (define f-str "f"))))
      'replace)
    (with-output-to-file "testfile-mc-foo.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme) (testfile-mc-b))
            (include "testfile-mc-c.ss")
            (include "testfile-mc-d.ss")
            (import M)
            (meta define build-something-f
              (lambda (k something)
                (import (testfile-mc-f))
                (datum->syntax k (string->symbol (string-append something "-" f-str)))))
            (define-syntax e
              (lambda (x)
                (syntax-case x ()
                  [(k) (let ()
                         (include "testfile-mc-e-import.ss")
                         #`'#,(build-something-f #'k e-str))])))
            (pretty-print (list a b c d (e))))))
      'replace)
    (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
    #t)
  (equal?
    (separate-eval '(load "testfile-mc-foo.so"))
    "(\"a\" \"b\" \"c\" #(\"b\" \"a\") e-f)\n")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = =))
  (touch "testfile-mc-foo.so" "testfile-mc-foo.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = >))
  (touch "testfile-mc-foo.so" "testfile-mc-a.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = =))
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(> > = = >))
  (touch "testfile-mc-foo.so" "testfile-mc-c.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = >))
  (touch "testfile-mc-foo.so" "testfile-mc-e.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = =))
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = > = >))
  (touch "testfile-mc-foo.so" "testfile-mc-e-import.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = >))
  (touch "testfile-mc-foo.so" "testfile-mc-f.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = = =))
  (equal?
    (let ([mt* (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-file x))) 'mc-foo)
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testfile-mc-a.so" "testfile-mc-b.so" "testfile-mc-e.so" "testfile-mc-f.so" "testfile-mc-foo.so"))
        mt*))
    '(= = = > >))
  (begin
    (rm-rf "testdir")
    (mkdir "testdir")
    (mkfile "testdir/testfile-mc-1a.ss"
      '(define mcratfink 'abe))
    (mkfile "testdir/testfile-mc-1b.ss"
      '(library (testdir testfile-mc-1b)
         (export mc-1b-x)
         (import (chezscheme))
         (include "testfile-mc-1a.ss")
         (define mc-1b-x
           (lambda ()
             (list mcratfink)))))
    (mkfile "testdir/testfile-mc-1c.ss"
      '(library (testdir testfile-mc-1c)
         (export mc-1b-x)
         (import (testdir testfile-mc-1b))))
    (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (compile-library x))) "testdir/testfile-mc-1c")
    #t)
  (equal?
    (separate-eval '(let () (import (testdir testfile-mc-1c)) (mc-1b-x)))
    "(abe)\n")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(= =))
  (touch "testdir/testfile-mc-1c.so" "testdir/testfile-mc-1a.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(= =))
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(> >))
  (touch "testdir/testfile-mc-1c.so" "testdir/testfile-mc-1b.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(= =))
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(> >))
  (touch "testdir/testfile-mc-1c.so" "testdir/testfile-mc-1c.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #t]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(= >))
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))])
      (separate-compile '(lambda (x) (parameterize ([compile-imported-libraries #f]) (maybe-compile-library x))) "testdir/testfile-mc-1c")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so" "testdir/testfile-mc-1c.so"))
        mt*))
    '(= =))
  (error? ; can't find testfile-mc-1a.ss
    (separate-compile 'compile-library "testdir/testfile-mc-1b"))
  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([source-directories (cons "testdir" (source-directories))])
           (maybe-compile-library x)))
      "testdir/testfile-mc-1b")
    #t)
  (error? ; can't find testfile-mc-1a.ss
    (separate-compile 'maybe-compile-library "testdir/testfile-mc-1b"))
  ; make sure maybe-compile-file wipes out b.so when it fails to find a.ss
  (or (= (optimize-level) 3) (not (file-exists? "testdir/testfile-mc-1b.so")))
  (begin
    (separate-compile '(lambda (x)
                         (parameterize ([source-directories (cons "testdir" (source-directories))])
                           (maybe-compile-library x)))
      "testdir/testfile-mc-1b")
    #t)
  (touch "testdir/testfile-mc-1b.so" "testdir/testfile-mc-1a.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so"))])
      (separate-compile '(lambda (x)
                           (parameterize ([source-directories (cons "testdir" (source-directories))])
                             (maybe-compile-library x)))
        "testdir/testfile-mc-1b")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so"))
        mt*))
    '(>))
  (touch "testdir/testfile-mc-1b.so" "testdir/testfile-mc-1b.ss")
  (equal?
    (let ([mt* (map file-modification-time '("testdir/testfile-mc-1b.so"))])
      (separate-compile '(lambda (x)
                           (parameterize ([source-directories (cons "testdir" (source-directories))])
                             (maybe-compile-library x)))
        "testdir/testfile-mc-1b")
      (map
        (lambda (x y) (if (time=? x y) '= (if (time<? x y) '< '>)))
        (map file-modification-time '("testdir/testfile-mc-1b.so"))
        mt*))
    '(>))
  (delete-file "testdir/testfile-mc-1a.ss")
  (error? ; maybe-compile-library: can't find testfile-mc-1a.ss
    (separate-compile '(lambda (x)
                         (parameterize ([source-directories (cons "testdir" (source-directories))])
                           (maybe-compile-library x)))
      "testdir/testfile-mc-1b"))
  ; make sure maybe-compile-file wipes out b.so when it fails to find a.ss
  (or (= (optimize-level) 3) (not (file-exists? "testdir/testfile-mc-1b.so")))
  (begin
    (rm-rf "testdir")
    #t)
  ; make sure maybe-compile-file handles incomplete fasl files
  (begin
    (mkfile "testfile-mc-2a.ss"
      '(library (testfile-mc-2a)
         (export q)
         (import (chezscheme))
         (define f
           (lambda ()
             (printf "running f\n")
             "x"))
         (define-syntax q
           (begin
             (printf "expanding testfile-mc-2a\n")
             (lambda (x)
               (printf "expanding q\n")
               #'(f))))))
    (mkfile "testfile-mc-2.ss"
      '(import (chezscheme) (testfile-mc-2a))
      '(define-syntax qq
         (begin
           (printf "expanding testfile-mc-2\n")
           (lambda (x)
             (printf "expanding qq\n")
             #'q)))
      '(printf "qq => ~a\n" qq))
    (delete-file "testfile-mc-2a.so")
    (delete-file "testfile-mc-2.so")
    (display-string (separate-compile '(lambda (x) (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [fasl-compressed #f]) (maybe-compile-program x))) 'mc-2))
    #t)
  (begin
    (let ([p (open-file-input/output-port "testfile-mc-2a.so" (file-options no-create no-fail no-truncate))])
      (set-port-length! p 73)
      (close-port p))
    (display-string (separate-compile '(lambda (x) (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [fasl-compressed #f] [import-notify #t]) (maybe-compile-program x))) 'mc-2))
    #t)
  (begin
    (let ([p (open-file-input/output-port "testfile-mc-2.so" (file-options no-create no-fail no-truncate))])
      (set-port-length! p 87)
      (close-port p))
    (display-string (separate-compile '(lambda (x) (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [fasl-compressed #f] [import-notify #t]) (maybe-compile-program x))) 'mc-2))
    #t)
  ; make sure maybe-compile-file handles missing include files gracefully
  (begin
    (mkfile "testfile-mc-3a.ss"
      "hello from 3a!")
    (mkfile "testfile-mc-3b.ss"
      '(library (testfile-mc-3b)
         (export q)
         (import (chezscheme))
         (define-syntax q
           (begin
             (printf "expanding testfile-mc-3b\n")
             (lambda (x)
               (printf "expanding q\n")
               (include "./testfile-mc-3a.ss"))))))
    (mkfile "testfile-mc-3.ss"
      '(import (chezscheme) (testfile-mc-3b))
      '(define-syntax qq
         (begin
           (printf "expanding testfile-mc-3\n")
           (lambda (x)
             (printf "expanding qq\n")
             #'q)))
      '(printf "qq => ~a\n" qq))
    (delete-file "testfile-mc-3b.so")
    (delete-file "testfile-mc-3.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t])
           (maybe-compile-program x)))
      'mc-3)
    #t)
  (begin
    (delete-file "testfile-mc-3a.ss")
    #t)
  (error? ; separate-compile: no such file or directory: testfile-mc-3a.ss
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t])
           (maybe-compile-program x)))
      'mc-3))
  ; make sure maybe-compile-file handles missing include files gracefully
  (begin
    (define-record-type hash-bang-chezscheme)
    (record-writer (type-descriptor hash-bang-chezscheme) (lambda (x p wr) (display-string "#!chezscheme")))
    (mkfile "testfile-mc-4a.ss"
      "hello from 4a!")
    (mkfile "testfile-mc-4b.ss"
      (make-hash-bang-chezscheme)
      '(library (testfile-mc-4b)
         (export b)
         (import (chezscheme))
         (define-syntax q
           (lambda (x)
             (if (file-exists? "testfile-mc-4a.ss")
                 (begin
                   (printf "HEY!\n")
                   (#%$require-include "./testfile-mc-4a.ss")
                   (call-with-input-file "testfile-mc-4a.ss" read))
                 (begin
                   (printf "BARLEY!\n")
                   "testfile-mc-4a is no more"))))
         (define (b) q)))
    (mkfile "testfile-mc-4.ss"
      '(import (chezscheme) (testfile-mc-4b))
      '(printf "q => ~a\n" (b)))
    (delete-file "testfile-mc-4b.so")
    (delete-file "testfile-mc-4.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t])
           (maybe-compile-program x)))
      'mc-4)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-4.so"))
    "q => hello from 4a!\n")
  (begin
    (mkfile "testfile-mc-4a.ss"
      "goodbye from 4a!")
    (touch "testfile-mc-4.so" "testfile-mc-4a.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [import-notify #t])
           (maybe-compile-program x)))
      'mc-4)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-4.so"))
    "q => goodbye from 4a!\n")
  (begin
    (delete-file "testfile-mc-4a.ss")
    #t)
  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [import-notify #t])
           (maybe-compile-program x)))
      'mc-4)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-4.so"))
    "q => testfile-mc-4a is no more\n")
  ; make sure maybe-compile-file handles missing include files gracefully
  (begin
    (define-record-type hash-bang-chezscheme)
    (record-writer (type-descriptor hash-bang-chezscheme) (lambda (x p wr) (display-string "#!chezscheme")))
    (mkfile "testfile-mc-5a.ss"
      "hello from 5a!")
    (mkfile "testfile-mc-5b.ss"
      (make-hash-bang-chezscheme)
      '(library (testfile-mc-5b)
         (export q)
         (import (chezscheme))
         (define-syntax q
           (lambda (x)
             (if (file-exists? "testfile-mc-5a.ss")
                 (begin
                   (printf "HEY!\n")
                   (#%$require-include "./testfile-mc-5a.ss")
                   (call-with-input-file "testfile-mc-5a.ss" read))
                 (begin
                   (printf "BARLEY!\n")
                   "testfile-mc-5a is no more"))))))
    (mkfile "testfile-mc-5.ss"
      '(import (chezscheme) (testfile-mc-5b))
      '(define-syntax qq (lambda (x) #'q))
      '(printf "qq => ~a\n" qq))
    (delete-file "testfile-mc-5b.so")
    (delete-file "testfile-mc-5.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t])
           (maybe-compile-program x)))
      'mc-5)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-5.so"))
    "qq => hello from 5a!\n")
  (begin
    (mkfile "testfile-mc-5a.ss"
      "goodbye from 5a!")
    (touch "testfile-mc-5.so" "testfile-mc-5a.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [import-notify #t])
           (maybe-compile-program x)))
      'mc-5)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-5.so"))
    "qq => goodbye from 5a!\n")
  (begin
    (delete-file "testfile-mc-5a.ss")
    #t)
  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-file-message #f] [compile-imported-libraries #t] [import-notify #t])
           (maybe-compile-program x)))
      'mc-5)
    #t)
  (equal?
    (separate-eval '(load-program "testfile-mc-5.so"))
    "qq => testfile-mc-5a is no more\n")
 )

(mat make-boot-file
  (eq? (begin
         (with-output-to-file "testfile-1.ss"
           (lambda ()
             (pretty-print '(display "hello 1\n")))
           'replace)
         (with-output-to-file "testfile-2.ss"
           (lambda ()
             (pretty-print '(display "hello 2\n")))
           'replace)
         (with-output-to-file "testfile-3.ss"
           (lambda ()
             (pretty-print '(display "hello 3\n")))
           'replace)
         (with-output-to-file "testfile-4.ss"
           (lambda ()
             (pretty-print '(display "hello 4\n")))
           '(replace))
         (with-output-to-file "testfile-5.ss"
           (lambda ()
             (pretty-print '(display "hello 5\n")))
           '(replace))
         (parameterize ([optimize-level 2])
           (compile-script "testfile-1")
           (compile-script "testfile-2")
           (compile-file "testfile-3")
           (compile-file "testfile-4")
           (compile-file "testfile-5")))
       (void))
  (equal?
    (begin
      (parameterize ([optimize-level 2])
        (make-boot-file "testfile.boot" '("petite")
          "testfile-1.so"
          "testfile-2.ss"
          "testfile-3.so"
          "testfile-4.so"
          "testfile-5.ss"))
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (format "~a -b ./testfile.boot -q" (patch-exec-path *scheme*))
                      (buffer-mode block)
                      (native-transcoder))])
        (close-output-port to-stdin)
        (let ([out (get-string-all from-stdout)]
              [err (get-string-all from-stderr)])
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (unless (eof-object? err) (error 'bootfile-test1 err))
          out)))
    "hello 1\nhello 2\nhello 3\nhello 4\nhello 5\n")
  (equal?
    (begin
      (parameterize ([optimize-level 2])
        (compile-to-file
         '((library (A) (export a) (import (scheme)) (define a 'aye))
           (library (B) (export b) (import (A) (scheme)) (define b (list a 'captain))))
         "testfile-libs.so")
        (make-boot-file "testfile.boot" '("petite") "testfile-libs.so"))
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (format "~a -b ./testfile.boot -q" (patch-exec-path *scheme*))
                      (buffer-mode block)
                      (native-transcoder))])
        (pretty-print '(let () (import (B)) (printf "~s\n" b)) to-stdin)
        (close-output-port to-stdin)
        (let ([out (get-string-all from-stdout)]
              [err (get-string-all from-stderr)])
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (unless (eof-object? err) (error 'bootfile-test1 err))
          out)))
    "(aye captain)\n")
  (equal?
    (let ([would-be-scheme (find-source (format "..~abin~a~a~ascheme~a" 
                                                (directory-separator)
                                                (directory-separator) (machine-type) (directory-separator)
                                                (if (windows?) ".exe" "")))])
      (unless (or (embedded?) (path-equal? *scheme* would-be-scheme))
        (errorf #f "not testing boot file based on ../boot/~a/petite.boot, since *scheme* ~a isn't ~a"
          (machine-type) *scheme* would-be-scheme))
      (parameterize ([optimize-level 2])
        (make-boot-file "testfile.boot" '()
          (find-source (format "../boot/~a/petite.boot" (machine-type)))
          "testfile-1.so"
          "testfile-2.so"
          "testfile-3.ss"
          "testfile-4.ss"
          "testfile-5.so"))
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (format "~a -b ./testfile.boot -q" (patch-exec-path *scheme*))
                      (buffer-mode block)
                      (native-transcoder))])
        (close-output-port to-stdin)
        (let ([out (get-string-all from-stdout)]
              [err (get-string-all from-stderr)])
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (unless (eof-object? err) (error 'bootfile-test2 err))
          out)))
    "hello 1\nhello 2\nhello 3\nhello 4\nhello 5\n")
  ; regression test to verify that we can evaluate a foreign-callable form inside the procedure to
  ; which scheme-start is set, which was failing because its relocation information was discarded
  ; by the static-generation collection.
  (or
   (pb?) ; callables not always in pb, and boot files may not work with pbchunk
   (equal?
    (let ([would-be-scheme (find-source (format "..~abin~a~a~ascheme~a" 
                                                (directory-separator)
                                                (directory-separator) (machine-type) (directory-separator)
                                                (if (windows?) ".exe" "")))])
      (unless (or (embedded?) (path-equal? *scheme* would-be-scheme))
        (errorf #f "not testing boot file based on ../boot/~a/petite.boot, since *scheme* ~a isn't ~a"
          (machine-type) *scheme* would-be-scheme))
      (mkfile "testfile.ss"
        '(scheme-start
           (lambda ()
             (let ([x 0])
               (printf "~s\n" (foreign-callable (lambda () (set! x (+ x 1)) x) () void))))))
      (make-boot-file "testfile.boot" '("petite") "testfile.ss")
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (format "~a -b ./testfile.boot -q" (patch-exec-path *scheme*))
                      (buffer-mode block)
                      (native-transcoder))])
        (close-output-port to-stdin)
        (let ([out (get-string-all from-stdout)]
              [err (get-string-all from-stderr)])
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (unless (eof-object? err) (error 'bootfile-test2 err))
          out)))
    "#<code>\n"))
)

(mat hostop
  (begin
    (separate-compile
      `(lambda (x)
         (call-with-port
           (open-file-output-port (format "~a.so" x) (file-options #;compressed replace))
           (lambda (op)
             (call-with-port
               (open-file-output-port (format "~a.host" x) (file-options #;compressed replace))
               (lambda (hostop)
                 (compile-to-port
                   '((library (testfile-hop1)
                       (export a b c)
                       (import (chezscheme))
                       (define-syntax a (identifier-syntax 17))
                       (module b (b1 b2)
                         (define b1 "23.5")
                         (define-syntax b2 (identifier-syntax (cons b1 b1))))
                       (define c (lambda (x) (import b) (vector b2 x)))))
                   op #f #f #f ',(machine-type) hostop))))))
      "testfile-hop1")
    (with-output-to-file "testfile-hop2.ss"
      (lambda ()
        (pretty-print '(eval-when (compile) (load "testfile-hop1.so")))
        (pretty-print '(eval-when (compile) (import (testfile-hop1))))
        (pretty-print '(eval-when (compile) (import b)))
        (pretty-print '(pretty-print (list a b1 b2 (c 55)))))
      'replace)
    (with-output-to-file "testfile-hop3.ss"
      (lambda ()
        (pretty-print '(eval-when (compile) (load "testfile-hop1.host")))
        (pretty-print '(eval-when (compile) (import (testfile-hop1))))
        (pretty-print '(eval-when (compile) (import b)))
        (pretty-print '(pretty-print (list a b1 b2 (c 55)))))
      'replace)
    (for-each separate-compile '(hop2 hop3))
    #t)
  (equal?
    (separate-eval
      '(load "testfile-hop1.so")
      '(import (testfile-hop1))
      'a
      '(import b)
      'b1
      'b2
      '(c 55))
    "17\n\
     \"23.5\"\n\
     (\"23.5\" . \"23.5\")\n\
     #((\"23.5\" . \"23.5\") 55)\n\
     ")
  (equal?
    (separate-eval
      '(visit "testfile-hop1.so") ; visit now---$invoke-library will revisit later
      '(import (testfile-hop1))
      'a
      '(import b)
      'b1
      'b2
      '(c 55))
    "17\n\
     \"23.5\"\n\
     (\"23.5\" . \"23.5\")\n\
     #((\"23.5\" . \"23.5\") 55)\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.so")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop2.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.so")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop3.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (equal?
    (separate-eval
      '(load "testfile-hop1.host")
      '(import (testfile-hop1))
      'a
      '(import b)
      'b1
      'b2
      '(c 55))
    "17\n\
     \"23.5\"\n\
     (\"23.5\" . \"23.5\")\n\
     #((\"23.5\" . \"23.5\") 55)\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.host")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop2.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.host")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop3.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (begin
    (#%$compile-host-library 'moi "testfile-hop1.host")
    (define bv (call-with-port (open-file-input-port "testfile-hop1.host") get-bytevector-all))
    #t)
  (begin
    ; doing it a second time should be a no-op
    (#%$compile-host-library 'moi "testfile-hop1.host")
    (bytevector=?
      (call-with-port (open-file-input-port "testfile-hop1.host") get-bytevector-all)
      bv))
  (begin
    (set! bv #f)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-hop1.host")
      '(import (testfile-hop1))
      'a
      '(import b)
      'b1
      'b2
      '(c 55))
    "17\n\
     \"23.5\"\n\
     (\"23.5\" . \"23.5\")\n\
     #((\"23.5\" . \"23.5\") 55)\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.host")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop2.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (equal?
    (separate-eval
      '(revisit "testfile-hop1.host")
      '(expand 'a)
      '(guard (c [else (display-condition c) (newline)]) (eval '(import b)))
      '(expand 'b1)
      '(expand 'b2)
      '(load "testfile-hop3.so"))
    "a\n\
     Exception: unknown module b\n\
     b1\n\
     b2\n\
     (17 \"23.5\" (\"23.5\" . \"23.5\") #((\"23.5\" . \"23.5\") 55))\n\
     ")
  (equal?
    (separate-eval
      '(visit "testfile-hop1.so")
      '(delete-file "testfile-hop1.so") ; prevent import from revisiting testfile-hop1.so
      '(import (testfile-hop1))
      'a
      '(import b)
      '(guard (c [else (display-condition c) (newline)]) (eval 'b1))
      '(guard (c [else (display-condition c) (newline)]) (eval 'b2))
      '(guard (c [else (display-condition c) (newline)]) (eval 'c)))
    "#t\n\
     17\n\
     Exception: failed for testfile-hop1.so: no such file or directory\n\
     Exception: failed for testfile-hop1.so: no such file or directory\n\
     Exception: failed for testfile-hop1.so: no such file or directory\n\
     ")
)

(mat eval
   (error? ; 7 is not an environment (should be reported by compile or interpret)
     (eval 3 7))
   (error? ; 7 is not an environment
     (interpret 3 7))
   (error? ; 7 is not an environment
     (compile 3 7))
   (eqv? (eval '(+ 3 4)) 7)
   (eq? (eval '(define foo (lambda (x) x))) (void))
   (eval '(let ([x '(a b c)]) (eq? (foo x) x)))
 )

(mat expand ; tested in mats extend-syntax & with in 8.ms
   (error? ; 7 is not an environment (should be reported by sc-expand)
     (expand 3 7))
   (error? ; 7 is not an environment
     (sc-expand 3 7))
   (procedure? expand)
 )

(mat eval-when
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display "
(eval-when (eval) (set! aaa 'eval))
(eval-when (load) (set! aaa 'load))
(eval-when (compile) (set! aaa 'compile))
" p)
      (close-output-port p)
      #t)
   (begin (set! aaa #f) (load "testfile.ss") (eq? aaa 'eval))
   (begin (printf "***** expect \"compile-file\" message:~%")
          (set! aaa #f)
          (compile-file "testfile")
          (eq? aaa 'compile))
   (begin (set! aaa #f) (load "testfile.so") (eq? aaa 'load))
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display "
(eval-when (eval)
   (eval-when (eval) (set! aaa 'eval@eval))
   (eval-when (load) (set! aaa 'load@eval))
   (eval-when (compile) (set! aaa 'compile@eval)))
(eval-when (load)
   (eval-when (eval) (set! bbb 'eval@load))
   (eval-when (load) (set! bbb 'load@load))
   (eval-when (compile) (set! bbb 'compile@load)))
(eval-when (compile)
   (eval-when (eval) (set! ccc 'eval@compile))
   (eval-when (load) (set! ccc 'load@compile))
   (eval-when (compile) (set! ccc 'compile@compile)))
" p)
      (close-output-port p)
      #t)
   (begin (set! aaa #f)
          (set! bbb #f)
          (set! ccc #f)
          (load "testfile.ss")
          (equal? (list aaa bbb ccc) '(eval@eval #f #f)))
   (begin (printf "***** expect \"compile-file\" message:~%")
          (set! aaa #f)
          (set! bbb #f)
          (set! ccc #f)
          (compile-file "testfile")
          (equal? (list aaa bbb ccc) '(#f compile@load eval@compile)))
   (begin (set! aaa #f)
          (set! bbb #f)
          (set! ccc #f)
          (load "testfile.so")
          (equal? (list aaa bbb ccc) '(#f load@load #f)))
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display "
(eval-when (eval) (pretty-print 'evaluating))
(eval-when (compile) (pretty-print 'compiling))
(eval-when (load) (pretty-print 'loading))
(eval-when (visit) (pretty-print 'visiting))
(eval-when (revisit) (pretty-print 'revisiting))
(eval-when (visit revisit) (pretty-print 'visit/revisit))
(eval-when (compile)
  (eval-when (eval)
    (pretty-print 'oops)))
(eval-when (load eval)
  (eval-when (compile)
    (pretty-print 'foo6)))
" p)
      (close-output-port p)
      #t)
   (let ()
     (define with-output-to-string
       (lambda (p)
         (parameterize ([current-output-port (open-output-string)])
           (p)
           (get-output-string (current-output-port)))))
     (and
       (string=?
         (with-output-to-string
           (lambda ()
             (compile-file "testfile")))
"compiling testfile.ss with output to testfile.so
compiling
oops
foo6
"
       )
       (string=?
         (with-output-to-string
           (lambda ()
             (visit "testfile.so")))
"visiting
visit/revisit
"
       )
       (string=?
         (with-output-to-string
           (lambda ()
             (revisit "testfile.so")))
"loading
revisiting
visit/revisit
"
       )
       (string=?
         (with-output-to-string
           (lambda ()
             (load "testfile.so")))
"loading
visiting
revisiting
visit/revisit
"
       )))
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display "
(define-syntax $a (identifier-syntax 'b))
(define $foo)
(eval-when (visit) (define visit-x 17))
(eval-when (revisit) (define-syntax revisit-x (identifier-syntax 23)))
" p)
      (close-output-port p)
      #t)
   (begin (define-syntax $foo (syntax-rules ())) #t)
   (begin (define-syntax $a (syntax-rules ())) #t)
   (begin (define-syntax visit-x (syntax-rules ())) #t)
   (begin (define-syntax revisit-x (syntax-rules ())) #t)
   (error? $foo)
   (error? $a)
   (error? visit-x)
   (error? revisit-x)
   (begin (compile-file "testfile") #t)
   (eq? $a 'b)
   (error? $foo)
   (error? visit-x)
   (error? revisit-x)
   (begin (define-syntax $foo (syntax-rules ())) #t)
   (begin (define-syntax $a (syntax-rules ())) #t)
   (begin (define-syntax visit-x (syntax-rules ())) #t)
   (begin (define-syntax revisit-x (syntax-rules ())) #t)
   (begin (visit "testfile.so") #t)
   (eq? $a 'b)
   (error? $foo)
   (eq? visit-x 17)
   (error? revisit-x)
   (begin (revisit "testfile.so") #t)
   (eq? $a 'b)
   (eq? $foo (void))
   (eq? visit-x 17)
   (eq? revisit-x 23)
   (begin (define get-$foo (lambda () $foo)) (eq? (get-$foo) (void)))
   (begin (define-syntax $foo (syntax-rules ())) #t)
   (begin (define-syntax $a (syntax-rules ())) #t)
   (begin (define-syntax visit-x (syntax-rules ())) #t)
   (begin (define-syntax revisit-x (syntax-rules ())) #t)
   (begin (revisit "testfile.so") #t)
   (error? $a)
   (error? $foo)
   (eq? (get-$foo) (void))
   (error? visit-x)
   (eq? revisit-x 23)
   (begin (visit "testfile.so") #t)
   (eq? $a 'b)
   (eq? $foo (void))
   (eq? (get-$foo) (void))
   (eq? visit-x 17)
   (eq? revisit-x 23)
   (begin (define-syntax $foo (syntax-rules ())) #t)
   (begin (define-syntax $a (syntax-rules ())) #t)
   (begin (define-syntax visit-x (syntax-rules ())) #t)
   (begin (define-syntax revisit-x (syntax-rules ())) #t)
   (begin (load "testfile.so") #t)
   (eq? $a 'b)
   (eq? $foo (void))
   (eq? (get-$foo) (void))
   (eq? visit-x 17)
   (eq? revisit-x 23)
   (begin (define-syntax $foo (syntax-rules ())) #t)
   (begin (define-syntax $a (syntax-rules ())) #t)
   (begin (define-syntax visit-x (syntax-rules ())) #t)
   (begin (define-syntax revisit-x (syntax-rules ())) #t)
   (begin (load "testfile.ss") #t)
   (eq? $a 'b)
   (eq? $foo (void))
   (error? visit-x)
   (error? revisit-x)
   (eqv?
     (let ((x 77))
       (eval-when (eval)
         (define x 88))
       x)
     88)
   (eqv?
     (let ((x 77))
       (eval-when (compile visit load revisit)
         (define x 88))
       x)
     77)
   (begin
     (define $qlist '())
     (define-syntax $qdef
       (syntax-rules ()
         [(_ x e)
          (begin
            (eval-when (compile)
              (set! $qlist (cons 'x $qlist)))
            (eval-when (load eval)
              (define x e)))]))
     ($qdef $bar 33)
     (and (null? $qlist) (eqv? $bar 33)))
   (let ([p (open-output-file "testfile.ss" 'replace)])
     (pretty-print '($qdef $baz (lambda () ($qdef x 44) x)) p)
     (close-output-port p)
     #t)
   (begin (compile-file "testfile") #t)
   (equal? $qlist '($baz))
   (begin (load "testfile.so") #t)
   (equal? $qlist '($baz))
   (eq? ($baz) 44)
  ; regression: make sure that visit doesn't evaluate top-level module
  ; inits and definition right-hand-sides
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (display
"(eval-when (visit) (printf \"visit A\\n\"))
(eval-when (revisit) (printf \"revisit A\\n\"))
(eval-when (load compile) (printf \"compile load A\\n\"))
(define foo (printf \"evaluating top-level foo rhs\\n\"))
(printf \"evaluating top-level init\\n\") 

(eval-when (visit) (printf \"visit B\\n\"))
(eval-when (revisit) (printf \"revisit B\\n\"))
(eval-when (load compile) (printf \"compile load B\\n\"))
(module ()
  (define foo (printf \"evaluating module foo rhs\\n\"))
  (printf \"evaluating module init\\n\"))
" p)
      (close-output-port p)
      #t)
   (let ()
     (define with-output-to-string
       (lambda (p)
         (parameterize ([current-output-port (open-output-string)])
           (p)
           (get-output-string (current-output-port)))))
     (and
       (string=?
         (with-output-to-string
           (lambda ()
             (compile-file "testfile")))
"compiling testfile.ss with output to testfile.so
compile load A
compile load B
"
       )
       (string=?
         (with-output-to-string
           (lambda ()
             (visit "testfile.so")))
"visit A
visit B
")
       (string=?
         (with-output-to-string
           (lambda ()
             (revisit "testfile.so")))
"revisit A
compile load A
evaluating top-level foo rhs
evaluating top-level init
revisit B
compile load B
evaluating module foo rhs
evaluating module init
")))
 )

(mat compile-whole-program
  (error? ; no such file or directory nosuchfile.wpo
    (compile-whole-program "nosuchfile.wpo" "testfile-wpo-ab-all.so"))
  (error? ; incorrect number of arguments 
    (compile-whole-program "testfile-wpo-ab.wpo"))
  (begin
    (with-output-to-file "testfile-wpo-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a)
             (export make-tree tree tree? tree-left tree-right tree-value)
             (import (chezscheme))

             (define-record-type tree
               (nongenerative)
               (fields (mutable left) (mutable value) (mutable right)))
             (record-writer (record-type-descriptor tree)
               (lambda (r p wr)
                 (display "#[tree " p)
                 (wr (tree-left r) p)
                 (display " " p)
                 (wr (tree-value r) p)
                 (display " " p)
                 (wr (tree-right r) p)
                 (display "]" p))))))
      'replace)
    (with-output-to-file "testfile-wpo-b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b)
             (export make-constant-tree make-tree tree? tree-left tree-right
               tree-value tree->list)
             (import (rnrs) (testfile-wpo-a))
             (define-syntax make-constant-tree
               (lambda (x)
                 (define build-tree
                   (lambda (tree-desc)
                     (syntax-case tree-desc ()
                       [(l v r)
                        (make-tree (build-tree #'l) (syntax->datum #'v) (build-tree #'r))]
                       [v (make-tree #f (syntax->datum #'v) #f)])))
                 (syntax-case x ()
                   [(_ tree-desc) #`'#,(build-tree #'tree-desc)])))
             (define tree->list
               (lambda (t)
                 (let f ([t t] [s '()])
                   (if (not t)
                       s
                       (f (tree-left t) (cons (tree-value t) (f (tree-right t) s))))))))))
      'replace)
    (with-output-to-file "testfile-wpo-ab.ss"
      (lambda ()
        (pretty-print '(import (chezscheme) (testfile-wpo-b)))
        (pretty-print '(define a (make-constant-tree ((1 2 4) 5 (8 10 12)))))
        (pretty-print '(printf "constant tree: ~s~%" a))
        (pretty-print '(printf "constant tree value: ~s~%" (tree-value a)))
        (pretty-print '(printf "constant tree walk: ~s~%" (tree->list a))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-ab")
    #t)

  (file-exists? "testfile-wpo-a.wpo")
  (file-exists? "testfile-wpo-b.wpo")
  (file-exists? "testfile-wpo-ab.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-ab.so"))
    "constant tree: #[tree #[tree #[tree #f 1 #f] 2 #[tree #f 4 #f]] 5 #[tree #[tree #f 8 #f] 10 #[tree #f 12 #f]]]\nconstant tree value: 5\nconstant tree walk: (1 2 4 5 8 10 12)\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x)))
      "testfile-wpo-ab")
    "()\n")

  (delete-file "testfile-wpo-a.so")
  (delete-file "testfile-wpo-b.so")
  (delete-file "testfile-wpo-ab.so")
  
  (equal? 
    (separate-eval '(load-program "testfile-wpo-ab-all.so"))
    "constant tree: #[tree #[tree #[tree #f 1 #f] 2 #[tree #f 4 #f]] 5 #[tree #[tree #f 8 #f] 10 #[tree #f 12 #f]]]\nconstant tree value: 5\nconstant tree walk: (1 2 4 5 8 10 12)\n")

  (begin
    (load-program "testfile-wpo-ab-all.so")
    #t)

  (not (memq '(testfile-wpo-a) (library-list)))
  (not (memq '(testfile-wpo-b) (library-list)))

  (begin
    (with-output-to-file "testfile-wpo-lib.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-lib)
             (export f)
             (import (chezscheme))
             (define (f n) (if (zero? n) 1 (* n (f (- n 1))))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-library x)))
      "testfile-wpo-lib")
    (file-exists? "testfile-wpo-lib.wpo"))

  (begin
    (with-output-to-file "testfile-wpo-prog.ss"
      (lambda ()
        (pretty-print '(import (chezscheme)))
        (pretty-print '(pretty-print (let () (import (testfile-wpo-lib)) (f 10))))
        (pretty-print '(pretty-print ((top-level-value 'f (environment '(testfile-wpo-lib))) 10))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-prog")
    (file-exists? "testfile-wpo-prog.wpo"))

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog.so"))
    "3628800\n3628800\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t))
      "testfile-wpo-prog")
    "()\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a-none.so" x) #f))
      "testfile-wpo-prog")
    "()\n")

  (delete-file "testfile-wpo-lib.ss")
  (delete-file "testfile-wpo-lib.so")
  (delete-file "testfile-wpo-lib.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog-all.so"))
    "3628800\n3628800\n")

  (error?
    (separate-eval '(load-program "testfile-wpo-prog-none.so")))

  (begin
    (with-output-to-file "testfile-wpo-a3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a3)
             (export ! z?)
             (import (rnrs))
             (define (z? n) (= n 0))
             (define (! n) (if (z? n) 1 (* n (! (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-wpo-b3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b3)
             (export fib !)
             (import (rnrs) (testfile-wpo-a3))
             (define (fib n)
               (cond
                 [(z? n) 1]
                 [(z? (- n 1)) 1]
                 [else (+ (fib (- n 1)) (fib (- n 2)))])))))
      'replace)
    (with-output-to-file "testfile-wpo-c3.ss"
      (lambda ()
        (pretty-print '(import (testfile-wpo-b3) (chezscheme)))
        (pretty-print '(pretty-print
                         (list (fib 10) (! 10)
                           ((top-level-value 'fib (environment '(testfile-wpo-b3))) 10)
                           ((top-level-value '! (environment '(testfile-wpo-b3))) 10)
                           ((top-level-value 'z? (environment '(testfile-wpo-a3))) 10)))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-c3")
    #t)

  (equal?
    (separate-eval '(load-program "testfile-wpo-c3.so"))
    "(89 3628800 89 3628800 #f)\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t)))
      "testfile-wpo-c3")
    "()\n")

  (delete-file "testfile-wpo-a3.ss")
  (delete-file "testfile-wpo-a3.so")
  (delete-file "testfile-wpo-a3.wpo")
  (delete-file "testfile-wpo-b3.ss")
  (delete-file "testfile-wpo-b3.so")
  (delete-file "testfile-wpo-b3.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-c3-all.so"))
    "(89 3628800 89 3628800 #f)\n")

  (begin
    (with-output-to-file "testfile-wpo-a4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a4)
             (export !)
             (import (chezscheme))
             (define (! n) (if (= n 0) 1 (* n (! (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-wpo-b4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b4)
             (export fib)
             (import (chezscheme))
             (define (fib n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))))))
      'replace)
    (with-output-to-file "testfile-wpo-c4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-c4)
             (export !fib)
             (import (chezscheme) (testfile-wpo-a4) (testfile-wpo-b4))
             (define (!fib n) (! (fib n))))))
      'replace)
    (with-output-to-file "testfile-wpo-prog4.ss"
      (lambda ()
        (pretty-print '(import (chezscheme) (testfile-wpo-c4)))
        (pretty-print '(pretty-print (!fib 5))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-prog4")
    #t)

  (delete-file "testfile-wpo-a4.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t)))
      'wpo-prog4)
    "((testfile-wpo-a4))\n")

  (begin
    (rename-file "testfile-wpo-a4.ss" "testfile-wpo-a4.ss.spam")
    (rename-file "testfile-wpo-b4.ss" "testfile-wpo-b4.ss.spam")
    (rename-file "testfile-wpo-c4.ss" "testfile-wpo-c4.ss.spam")
    (rename-file "testfile-wpo-prog4.ss" "testfile-wpo-prog4.ss.spam")
    #t)

  (delete-file "testfile-wpo-b4.so")
  (delete-file "testfile-wpo-b4.wpo")
  (delete-file "testfile-wpo-c4.so")
  (delete-file "testfile-wpo-c4.wpo")
  (delete-file "testfile-wpo-prog4.so")
  (delete-file "testfile-wpo-prog4.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog4-all.so"))
    "40320\n")

  (delete-file "testfile-wpo-a4.so")

  (error? ; library (testfile-wpo-a4) not found
    (separate-eval '(load-program "testfile-wpo-prog4-all.so")))

  (delete-file "testfile-wpo-prog4-all.so")

  (begin
    (rename-file "testfile-wpo-a4.ss.spam" "testfile-wpo-a4.ss")
    (rename-file "testfile-wpo-b4.ss.spam" "testfile-wpo-b4.ss")
    (rename-file "testfile-wpo-c4.ss.spam" "testfile-wpo-c4.ss")
    (rename-file "testfile-wpo-prog4.ss.spam" "testfile-wpo-prog4.ss")
    #t)

  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-prog4")
    #t)

  (delete-file "testfile-wpo-c4.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t)))
      'wpo-prog4)
    "((testfile-wpo-c4))\n")

  (delete-file "testfile-wpo-a4.ss")
  (delete-file "testfile-wpo-b4.ss")
  (delete-file "testfile-wpo-c4.ss")
  (delete-file "testfile-wpo-prog4.ss")
  (delete-file "testfile-wpo-a4.so")
  (delete-file "testfile-wpo-a4.wpo")
  (delete-file "testfile-wpo-b4.so")
  (delete-file "testfile-wpo-b4.wpo")
  (delete-file "testfile-wpo-prog4.so")
  (delete-file "testfile-wpo-prog4.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog4-all.so"))
    "40320\n")

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-wpo-prog4-all.so"))
    "")

  (delete-file "testfile-wpo-c4.so")

  (error? ; library (testfile-wpo-c4) not found
    (separate-eval '(load-program "testfile-wpo-prog4-all.so")))

  (begin
    (with-output-to-file "testfile-wpo-a5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a5)
             (export a)
             (import (chezscheme))
             (define a
               (lambda (n)
                 (+ ((top-level-value 'c (environment '(testfile-wpo-c5)))) n))))))
      'replace)
    (with-output-to-file "testfile-wpo-b5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b5)
             (export b)
             (import (chezscheme) (testfile-wpo-a5))
             (define b (a 10)))))
      'replace)
    (with-output-to-file "testfile-wpo-c5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-c5)
             (export c)
             (import (chezscheme) (testfile-wpo-a5) (testfile-wpo-b5))
             (define c (lambda () (+ (a 10) b))))))
      'replace)
    (with-output-to-file "testfile-wpo-prog5.ss"
      (lambda ()
        (pretty-print '(import (chezscheme) (testfile-wpo-b5) (testfile-wpo-c5)))
        (pretty-print '(pretty-print (cons (b) c))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      "testfile-wpo-prog5")
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t)))
      'wpo-prog5)
    "()\n")

  (error? ; attempt to invoke library (testfile-wpo-c5) while it is still being loaded
    (separate-eval '(load-program "testfile-wpo-prog5-all.so")))

  (begin
    (with-output-to-file "testfile-wpo-a6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a6)
             (export x a)
             (import (rnrs))
             (define x 3)
             (define z 17)
             (define-syntax a (identifier-syntax z))
             (display "invoke a\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-b6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b6)
             (export y)
             (import (rnrs) (testfile-wpo-a6))
             (define counter 9)
             (define (y) (set! counter (+ counter 5)) (list x counter a))
             (display "invoke b\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-prog6.ss"
      (lambda ()
        (pretty-print '(import (testfile-wpo-b6) (rnrs) (only (chezscheme) printf)))
        (pretty-print '(printf "==== ~s ====" (y)))
        (pretty-print '(printf "==== ~s ====" (y))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-prog6)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog6.so"))
    "invoke a\ninvoke b\n==== (3 14 17) ======== (3 19 17) ====")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x))))
      'wpo-prog6)
    "()\n")

  (delete-file "testfile-wpo-a6.ss")
  (delete-file "testfile-wpo-a6.so")
  (delete-file "testfile-wpo-a6.wpo")
  (delete-file "testfile-wpo-b6.ss")
  (delete-file "testfile-wpo-b6.so")
  (delete-file "testfile-wpo-b6.wpo")

  (equal?
    (separate-eval '(load-program "testfile-wpo-prog6-all.so"))
    "invoke a\ninvoke b\n==== (3 14 17) ======== (3 19 17) ====")

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-wpo-prog6-all.so"))
    "")

  (begin
    (with-output-to-file "testfile-wpo-aa7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-aa7)
             (export ax)
             (import (chezscheme))
             (define ax (gensym))
             (printf "invoking aa\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-a7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-a7)
             (export x)
             (import (chezscheme) (testfile-wpo-aa7))
             (define x (cons ax (gensym)))
             (printf "invoking a\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-b7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-b7)
             (export z)
             (import (chezscheme) (testfile-wpo-c7))
             (define z (cons 'b y))
             (printf "invoking b\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-c7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-c7)
             (export y)
             (import (chezscheme) (testfile-wpo-a7))
             (define y (cons 'c x))
             (printf "invoking c\n"))))
      'replace)
    (with-output-to-file "testfile-wpo-ab7.ss"
      (lambda ()
        (for-each pretty-print 
          '((import (chezscheme) (testfile-wpo-c7) (testfile-wpo-a7) (testfile-wpo-b7))
            (pretty-print (eq? (cdr y) x))
            (pretty-print (eq? (cdr z) y))
            (pretty-print (and (eq? (car y) 'c) (eq? (car z) 'b))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-ab7)
    #t)

  (equal?
    (separate-eval '(load "testfile-wpo-ab7.so"))
    "invoking aa\ninvoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n")

  (delete-file "testfile-wpo-c7.ss")
  (delete-file "testfile-wpo-c7.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x))))
      'wpo-ab7)
    "((testfile-wpo-c7))\n")

  (equal?
    (separate-eval '(load "testfile-wpo-ab7-all.so"))
    "invoking aa\ninvoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n")

  (begin
    (with-output-to-file "testfile-wpo-extlib.chezscheme.sls"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-extlib)
             (export magic)
             (import (rnrs))
             (define magic (cons 9 5)))))
      'replace)
    (with-output-to-file "testfile-wpo-ext.ss"
      (lambda ()
        (pretty-print '(import (chezscheme) (testfile-wpo-extlib)))
        (pretty-print '(pretty-print magic)))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-ext)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x))))
      'wpo-ext)
    "()\n")

  (equal?
    (separate-eval '(load "testfile-wpo-ext-all.so"))
    "(9 . 5)\n")

  ; test propagation of #! shell-script line
  (begin
    (define $hash-bang-line "#! /usr/bin/scheme --program\n")
    (delete-file "testfile-wpo-c8.so")
    (delete-file "testfile-wpo-c8-all.so")
    (delete-file "testfile-wpo-c8.wpo")
    (with-output-to-file "testfile-wpo-c8.ss"
      (lambda ()
        (display-string $hash-bang-line)
        (for-each pretty-print
          '((import (chezscheme))
            (printf "hello\n"))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-program x)))
      'wpo-c8)
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x)))
      'wpo-c8)
    #t)

  (equal?
    (separate-eval '(load "testfile-wpo-c8.so"))
    "hello\n")

  (equal?
    (separate-eval '(load "testfile-wpo-c8-all.so"))
    "hello\n")

  (equal?
    (call-with-port (open-file-input-port "testfile-wpo-c8-all.so")
      (lambda (ip)
        (get-bytevector-n ip (string-length $hash-bang-line))))
    (string->utf8 $hash-bang-line))

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-wpo-c8-all.so"))
    "")

  (begin
    (mkfile "testfile-wpo-a9.ss"
      '(library (testfile-wpo-a9)
         (export x)
         (import (chezscheme))
         (define x (eval 'x (environment '(testfile-wpo-a9))))))
    (mkfile "testfile-wpo-b9.ss"
      '(import (chezscheme) (testfile-wpo-a9))
      '(printf "x = ~s\n" x))
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t] [compile-imported-libraries #t])
           (compile-program x)))
      'wpo-b9)
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a-all.so" x) #t))
      'wpo-b9)
    (separate-compile
      '(lambda (x)
         (compile-whole-library (format "~a.wpo" x) (format "~a-all.so" x)))
      'wpo-a9)
    #t)

  (error? ; invoke cycle
    (separate-eval
      '(load-library "testfile-wpo-a9.so")
      '(let () (import (testfile-wpo-a9)) x)))

  (error? ; invoke cycle
    (separate-eval
      '(load-library "testfile-wpo-a9-all.so")
      '(let () (import (testfile-wpo-a9)) x)))

  (error? ; invoke cycle
    (separate-eval
      '(load-program "testfile-wpo-b9.so")))

  (error? ; invoke cycle
    (separate-eval
      '(load-program "testfile-wpo-b9-all.so")))

  (begin
    (mkfile "testfile-wpo-a10.ss"
      '(library (testfile-wpo-a10)
         (export ax)
         (import (chezscheme))
         (define ax (cons 'a '()))))
    (mkfile "testfile-wpo-b10.ss"
      '(library (testfile-wpo-b10)
         (export bx)
         (import (chezscheme) (testfile-wpo-a10))
         (define bx (cons 'b ax))))
    (mkfile "testfile-wpo-c10.ss"
      '(library (testfile-wpo-c10)
         (export cx)
         (import (chezscheme) (testfile-wpo-b10))
         (define cx (cons 'c bx))))
    (mkfile "testfile-wpo-d10.ss"
      '(import (chezscheme) (testfile-wpo-c10))
      '(printf "d: cx = ~s\n" cx))
    (mkfile "testfile-wpo-e10.ss"
      '(import (chezscheme) (testfile-wpo-a10))
      '(printf "e: ax = ~s\n" ax))
    (mkfile "testfile-wpo-f10.ss"
      '(import (chezscheme) (testfile-wpo-c10))
      '(printf "f: cx = ~s\n" cx))
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t]
                        [compile-imported-libraries #t])
           (compile-program x)))
      'wpo-d10)
    (separate-compile 'compile-program 'wpo-e10)
    (separate-compile 'compile-program 'wpo-f10)
    #t)
  
  ; cause b10 to be excluded from the whole program
  (delete-file "testfile-wpo-b10.wpo")
  
  (equal?
    (separate-eval
      '(compile-whole-program "testfile-wpo-d10.wpo"
         "testfile-wpo-d10-all.so" #f))
    "((testfile-wpo-b10))\n")
  
  (equal?
    (separate-eval '(verify-loadability 'visit "testfile-wpo-d10-all.so"))
    "")

  (equal?
    (separate-eval '(verify-loadability 'revisit "testfile-wpo-d10-all.so"))
    "")

  (equal?
    (separate-eval '(verify-loadability 'load "testfile-wpo-d10-all.so"))
    "")
  
  (equal?
    (separate-eval '(load-program "testfile-wpo-d10-all.so"))
    "d: cx = (c b a)\n")
  
  ; library a10 must be visible for (excluded library)
  ; b10's benefit, so e10 can reference its export
  (equal?
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(load-program "testfile-wpo-e10.so"))
    "d: cx = (c b a)\ne: ax = (a)\n")
  
  ; library c10 need not and should not be visible, so f10
  ; shouldn't be able to reference its export.
  (error?
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(load-program "testfile-wpo-f10.so")))

  (error? ; testfile-wpo-c10 is not visible
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(import (testfile-wpo-c10))))

  (equal?
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(verify-loadability 'visit "testfile-wpo-f10.so"))
    "d: cx = (c b a)\n")

  ; verify-loadability should error out trying to invoke
  ; c10 because c10 is not visible
  (error? ; not visible
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(verify-loadability 'revisit "testfile-wpo-f10.so")))

  (error? ; not visible
    (separate-eval
      '(load-program "testfile-wpo-d10-all.so")
      '(verify-loadability 'load "testfile-wpo-f10.so")))

  (begin
    (mkfile "testfile-wpo-a11.ss"
      '(library (testfile-wpo-a11)
         (export ax)
         (import (chezscheme))
         (define ax (cons 'a '()))
         (printf "invoking a\n")))
    (parameterize ([generate-wpo-files #t])
      (compile-library "testfile-wpo-a11"))
    #t)

  (equal?
    (compile-whole-library "testfile-wpo-a11.wpo" "testfile-wpo-a11-all.so")
    '())

  (equal?
    (separate-eval
      '(load-library "testfile-wpo-a11.so"))
    "")

  (equal?
    (separate-eval
      '(load-library "testfile-wpo-a11.so")
      '(let () (import (testfile-wpo-a11)) ax))
    "invoking a\n(a)\n")

  (equal?
    (separate-eval
      '(load-library "testfile-wpo-a11-all.so"))
    "")

  (equal?
    (separate-eval
      '(load-library "testfile-wpo-a11-all.so")
      '(let () (import (testfile-wpo-a11)) ax))
    "invoking a\n(a)\n")

  (begin
    (mkfile "testfile-wpo-a12.ss"
      '(library (testfile-wpo-a12)
         (export ax)
         (import (chezscheme))
         (define ax (cons 'a '()))))
    (mkfile "testfile-wpo-b12.ss"
      '(library (testfile-wpo-b12)
         (export bx)
         (import (chezscheme) (testfile-wpo-a12))
         (define bx (eval 'cx (environment '(testfile-wpo-c12))))))
    (mkfile "testfile-wpo-c12.ss"
      '(library (testfile-wpo-c12)
         (export cx)
         (import (chezscheme) (testfile-wpo-b12))
         (define cx (cons 'c bx))))
    (mkfile "testfile-wpo-d12.ss"
      '(import (chezscheme) (testfile-wpo-c12))
      '(printf "d: cx = ~s\n" cx))
    (parameterize ([generate-wpo-files #t]
                   [compile-imported-libraries #t])
      (compile-program "testfile-wpo-d12"))
    #t)
  
  (error? ; cyclc
    (separate-eval '(load-program "testfile-wpo-d12.so")))

  ; cause b12 to be excluded from the whole library and program
  (delete-file "testfile-wpo-b12.wpo")

  (equal?
    (separate-eval
      '(compile-whole-library "testfile-wpo-c12.wpo"
         "testfile-wpo-c12-all.so"))
    "((testfile-wpo-b12))\n")
  
  (equal?
    (separate-eval
      '(compile-whole-program "testfile-wpo-d12.wpo"
         "testfile-wpo-d12-all.so" #t))
    "((testfile-wpo-b12))\n")
  
  (equal?
    (separate-eval
      '(load-library "testfile-wpo-c12-all.so"))
    "")

  (error? ; cycle
    (separate-eval
      '(load-library "testfile-wpo-c12-all.so")
      '(let () (import (testfile-wpo-c12)) cx)))
  
  (error? ; cycle
    (separate-eval '(load-program "testfile-wpo-d12-all.so")))
  
  ; verify-loadability doesn't catch (dynamic) cycles
  (equal?
    (separate-eval
      '(verify-loadability 'visit "testfile-wpo-c12.so"))
    "")

  (equal?
    (separate-eval
      '(verify-loadability 'revisit "testfile-wpo-c12.so"))
    "")

  (equal?
    (separate-eval
      '(verify-loadability 'load "testfile-wpo-c12.so"))
    "")
  
  ; verify-loadability doesn't catch (dynamic) cycles
  (equal?
    (separate-eval
      '(verify-loadability 'visit "testfile-wpo-d12.so"))
    "")

  (equal?
    (separate-eval
      '(verify-loadability 'revisit "testfile-wpo-d12.so"))
    "")

  (equal?
    (separate-eval
      '(verify-loadability 'load "testfile-wpo-d12.so"))
    "")
)

(mat compile-whole-library
  (begin
    (with-output-to-file "testfile-cwl-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a1)
             (export x a)
             (import (rnrs))
             (define x 3)
             (define z 17)
             (define-syntax a (identifier-syntax z))
             (display "invoke a\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b1)
             (export y)
             (import (rnrs) (testfile-cwl-a1))
             (define counter 9)
             (define (y) (set! counter (+ counter 5)) (list x counter a))
             (display "invoke b\n"))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-b1")
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-b1)
    "()\n")

  (begin
    (rename-file "testfile-cwl-a1.ss" "testfile-cwl-a1.ss.spam")
    #t)

  (delete-file "testfile-cwl-a1.so")
  (delete-file "testfile-cwl-a1.wpo")

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b1))
         (printf ">~s\n" (y))
         (printf ">~s\n" (y))))
    "invoke a\ninvoke b\n>(3 14 17)\n>(3 19 17)\n")

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-cwl-b1.so"))
    "")

  (error? ; library (testfile-cwl-a1) not found
    (separate-eval
      '(begin
         (import (testfile-cwl-a1))
         (import (testfile-cwl-b1)))))

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b1))
         (import (testfile-cwl-a1))
         (printf ">~s\n" (y))
         (printf ">~s\n" (list a x))))
    "invoke a\ninvoke b\n>(3 14 17)\n>(17 3)\n")

  (begin
    (rename-file "testfile-cwl-a1.ss.spam" "testfile-cwl-a1.ss")
    (with-output-to-file "testfile-cwl-d1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-d1)
             (export z)
             (import (rnrs) (testfile-cwl-a1))
             (define counter 7)
             (define (z) (set! counter (+ counter 5)) (list x counter a))
             (display "invoke d\n"))))
      'replace)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-d1)
    "compiling testfile-cwl-d1.ss with output to testfile-cwl-d1.so\ncompiling testfile-cwl-a1.ss with output to testfile-cwl-a1.so\n")

  (begin
    (with-output-to-file "testfile-cwl-a2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a2)
             (export f)
             (import (chezscheme))
             (define (f n) (if (zero? n) 1 (* n (f (- n 1))))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-library x)))
      'cwl-a2)
    (file-exists? "testfile-cwl-a2.wpo"))

  (begin
    (with-output-to-file "testfile-cwl-b2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b2)
             (export main)
             (import (chezscheme))
             (define (main)
               (import (testfile-cwl-a2))
               ((top-level-value 'f (environment '(testfile-cwl-a2))) 10)))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-b2")
    (file-exists? "testfile-cwl-b2.wpo"))

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b2))
         (main)))
    "3628800\n")

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-cwl-b2.so"))
    "")

  (equal?
    (separate-compile
      '(lambda (x)
         (compile-whole-library (format "~a.wpo" x) (format "~a.so" x)))
      "testfile-cwl-b2")
    "()\n")

  (delete-file "testfile-cwl-a2.ss")
  (delete-file "testfile-cwl-a2.so")

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b2))
         (main)))
    "3628800\n")

  (begin
    (with-output-to-file "testfile-cwl-c1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-c1)
             (export main)
             (import (chezscheme))
             (define (main)
               (import (testfile-cwl-b1))
               (printf ">~s\n" ((top-level-value 'y (environment '(testfile-cwl-b1)))))
               (printf ">~s\n" ((top-level-value 'y (environment '(testfile-cwl-b1)))))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-c1")
    #t)

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-c1))
         (main)))
    "invoke a\ninvoke b\n>(3 14 17)\n>(3 19 17)\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (compile-whole-library (format "~a.wpo" x) (format "~a.so" x)))
      "testfile-cwl-c1")
    "()\n")

  (delete-file "testfile-cwl-a1.so")
  (delete-file "testfile-cwl-a1.ss")
  (delete-file "testfile-cwl-b1.so")
  (delete-file "testfile-cwl-b1.ss")

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-c1))
         (main)))
    "invoke a\ninvoke b\n>(3 14 17)\n>(3 19 17)\n")

  (begin
    (with-output-to-file "testfile-cwl-a3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a3)
             (export ! z?)
             (import (rnrs))
             (define (z? n) (= n 0))
             (define (! n) (if (z? n) 1 (* n (! (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-cwl-b3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b3)
             (export fib !)
             (import (rnrs) (testfile-cwl-a3))
             (define (fib n)
               (cond
                 [(z? n) 1]
                 [(z? (- n 1)) 1]
                 [else (+ (fib (- n 1)) (fib (- n 2)))])))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-b3")
    #t)

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b3))
         (import (testfile-cwl-a3))
         (pretty-print (list (! 10) (fib 10) (z? 10)))))
    "(3628800 89 #f)\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      "testfile-cwl-b3")
    "()\n")

  (delete-file "testfile-cwl-a3.so")
  (delete-file "testfile-cwl-a3.wpo")

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b3))
         (import (testfile-cwl-a3))
         (pretty-print (list (! 10) (fib 10) (z? 10)))))
    "(3628800 89 #f)\n")

  (begin
    (with-output-to-file "testfile-cwl-x4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-x4)
             (export ack)
             (import (rnrs))
             (define (ack m n)
               (if (= m 0)
                   (+ n 1)
                   (if (= n 0)
                       (ack (- m 1) 1)
                       (ack (- m 1) (ack m (- n 1)))))))))
      'replace)
    (with-output-to-file "testfile-cwl-y4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-y4)
             (export fact)
             (import (rnrs))
             (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-cwl-z4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-z4)
             (export fib)
             (import (rnrs))
             (define (fib n)
               (cond
                 [(= n 0) 1]
                 [(= n 1) 1]
                 [else (+ (fib (- n 1)) (fib (- n 2)))])))))
      'replace)
    (with-output-to-file "testfile-cwl-w4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-w4)
             (export mult)
             (import (rnrs))
             (define (mult n m) (if (= n 1) m (+ m (mult (- n 1) m)))))))
      'replace)
    (with-output-to-file "testfile-cwl-a4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a4)
             (export a-stuff)
             (import (rnrs) (testfile-cwl-x4) (testfile-cwl-y4) (testfile-cwl-z4) (testfile-cwl-b4) (testfile-cwl-c4))
             (define (a-stuff) (list (ack 3 4) (fib 5) (fact 10))))))
      'replace)
    (with-output-to-file "testfile-cwl-b4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b4)
             (export b-stuff)
             (import (rnrs) (testfile-cwl-x4) (testfile-cwl-w4))
             (define (b-stuff) (mult 3 (ack 3 4))))))
      'replace)
    (with-output-to-file "testfile-cwl-c4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-c4)
             (export c-stuff)
             (import (rnrs) (testfile-cwl-y4) (testfile-cwl-w4))
             (define (c-stuff) (mult 5 (fact 10))))))
      'replace)
    #t)

  (begin
    (define (separate-compile-cwl4)
      (separate-compile
        '(lambda (x)
           (parameterize ([compile-imported-libraries #t]
                          [generate-wpo-files #t])
             (compile-library x)))
        "testfile-cwl-b4")
      (separate-compile
        '(lambda (x)
           (parameterize ([compile-imported-libraries #t]
                          [generate-wpo-files #t])
             (compile-library x)))
        "testfile-cwl-c4")
      (separate-compile
        '(lambda (x)
           (parameterize ([compile-imported-libraries #t]
                          [generate-wpo-files #t])
             (compile-library x)))
        "testfile-cwl-a4")
      (andmap
        (lambda (n)
          (and (file-exists? (format "testfile-cwl-~s4.wpo" n))
               (file-exists? (format "testfile-cwl-~s4.so" n))))
        '(a b c x y z w)))
    #t)

  (begin
    (define (clear-cwl4-output)
      (andmap
        (lambda (n)
          (and (delete (format "testfile-cwl-~s4.wpo" n))
               (delete (format "testfile-cwl-~s4.so" n))))
        '(a b c x y z w)))
    #t)

  (separate-compile-cwl4)

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-a4))
         (import (testfile-cwl-b4) (testfile-cwl-c4))
         (pretty-print (a-stuff))
         (pretty-print (b-stuff))
         (pretty-print (c-stuff))))
    "(125 8 3628800)\n375\n18144000\n")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      "testfile-cwl-a4")
    "()\n")

  (andmap
    (lambda (name)
      (andmap
        (lambda (ext)
          (delete-file (format "testfile-cwl-~s4.~s" name ext)))
        '(so ss wpo)))
    '(b c x y z w))

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-a4))
         (import (testfile-cwl-b4) (testfile-cwl-c4))
         (pretty-print (a-stuff))
         (pretty-print (b-stuff))
         (pretty-print (c-stuff))))
    "(125 8 3628800)\n375\n18144000\n")

  (begin
    (with-output-to-file "testfile-cwl-a5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a5)
             (export fact)
             (import (rnrs))
             (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-cwl-b5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b5)
             (export fib+fact)
             (import (rnrs) (testfile-cwl-a5))
             (define (fib n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2)))))
             (define (fib+fact n) (+ (fib n) (fact n))))))
      'replace)
    (with-output-to-file "testfile-cwl-c5.ss"
      (lambda ()
        (pretty-print
          `(library (testfile-cwl-c5)
             (export ack+fact)
             (import (rnrs) (testfile-cwl-a5))
             (define (ack m n)
               (cond
                 [(= m 0) (+ n 1)]
                 [(= n 0) (ack (- m 1) 1)]
                 [else (ack (- m 1) (ack m (- n 1)))]))
             (define (ack+fact m n) (+ (ack m n) (fact m) (fact n))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (for-each compile-library x)))
      '(quote ("testfile-cwl-b5" "testfile-cwl-c5")))
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      "testfile-cwl-b5")
    "()\n")

  (delete-file "testfile-cwl-a5.ss")
  (delete-file "testfile-cwl-a5.so")
  (delete-file "testfile-cwl-a5.wpo")

  (equal?
    (separate-eval
      '(let ()
         (import (testfile-cwl-b5))
         (import (testfile-cwl-c5))
         (list (fib+fact 10) (ack+fact 3 4))))
    "(3628889 155)\n")


  (begin
    (with-output-to-file "testfile-cwl-a5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a5)
             (export fact)
             (import (rnrs))
             (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))))
      'replace)

    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (for-each compile-library x)))
      '(quote ("testfile-cwl-b5" "testfile-cwl-c5")))
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      "testfile-cwl-b5")
    "()\n")

  (error? ; attempting to re-install compile-time (or run-time) part of library (testfile-cwl-a5)
    (separate-eval
      '(let ()
         (import (testfile-cwl-c5))
         (import (testfile-cwl-b5))
         (list (fib+fact 10) (ack+fact 3 4)))))

  (error? ; attempting to re-install compile-time (or run-time) part of library (testfile-cwl-a5)
    (separate-eval
      '(eval '(list (fib+fact 10) (ack+fact 3 4))
             (environment '(chezscheme) '(testfile-cwl-c5) '(testfile-cwl-b5)))))

  (equal?
    (separate-eval
      '(eval '(list (fib+fact 10) (ack+fact 3 4))
             (environment '(chezscheme) '(testfile-cwl-b5) '(testfile-cwl-c5))))
    "(3628889 155)\n")

  (begin
    (with-output-to-file "testfile-cwl-d5.ss"
      (lambda ()
        (pretty-print
          '(eval '(list (fib+fact 10) (ack+fact 3 4))
             (environment '(chezscheme) '(testfile-cwl-c5) '(testfile-cwl-b5)))))
      'replace)
    (separate-compile 'cwl-d5)
    #t)

  (error? ; attempting to re-install compile-time (or run-time) part of library (testfile-cwl-a5)
    (separate-eval '(load "testfile-cwl-d5.so")))

  (begin
    (with-output-to-file "testfile-cwl-d5.ss"
      (lambda ()
        (pretty-print
          '(eval '(list (fib+fact 10) (ack+fact 3 4))
             (environment '(chezscheme) '(testfile-cwl-b5) '(testfile-cwl-c5)))))
      'replace)
    (separate-compile 'cwl-d5)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      "testfile-cwl-c5")
    "()\n")

  (delete-file "testfile-cwl-a5.ss")
  (delete-file "testfile-cwl-a5.so")
  (delete-file "testfile-cwl-a5.wpo")

  (error? ; attempting to re-install compile-time (or run-time) part of library (testfile-cwl-a5)
    (separate-eval
      '(let ()
         (import (testfile-cwl-c5))
         (import (testfile-cwl-b5))
         (list (fib+fact 10) (ack+fact 3 4)))))

  (error? ; attempting to re-install compile-time (or run-time) part of library (testfile-cwl-a5)
    (separate-eval
      '(let ()
         (import (testfile-cwl-b5))
         (import (testfile-cwl-c5))
         (list (fib+fact 10) (ack+fact 3 4)))))

  (begin
    (with-output-to-file "testfile-cwl-a6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a6)
             (export !)
             (import (chezscheme))
             (define (! n) (if (= n 0) 1 (* n (! (- n 1))))))))
      'replace)
    (with-output-to-file "testfile-cwl-b6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b6)
             (export fib)
             (import (chezscheme))
             (define (fib n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))))))
      'replace)
    (with-output-to-file "testfile-cwl-c6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-c6)
             (export !fib)
             (import (chezscheme) (testfile-cwl-a6) (testfile-cwl-b6))
             (define (!fib n) (! (fib n))))))
      'replace)
    (with-output-to-file "testfile-cwl-d6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-d6)
             (export runit)
             (import (chezscheme) (testfile-cwl-c6))
             (define (runit) (pretty-print (!fib 5)))
             (display "invoking d6\n"))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-d6")
    #t)

  (delete-file "testfile-cwl-a6.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-d6)
    "((testfile-cwl-a6))\n")

  (begin
    (rename-file "testfile-cwl-a6.ss" "testfile-cwl-a6.ss.spam")
    (rename-file "testfile-cwl-b6.ss" "testfile-cwl-b6.ss.spam")
    (rename-file "testfile-cwl-c6.ss" "testfile-cwl-c6.ss.spam")
    (rename-file "testfile-cwl-d6.ss" "testfile-cwl-d6.ss.spam")
    #t)

  (delete-file "testfile-cwl-b6.so")
  (delete-file "testfile-cwl-b6.wpo")
  (delete-file "testfile-cwl-c6.so")
  (delete-file "testfile-cwl-c6.wpo")
  (delete-file "testfile-cwl-d6.wpo")

  (equal?
    (separate-eval '(begin (import (testfile-cwl-d6)) (runit)))
    "invoking d6\n40320\n")

  (delete-file "testfile-cwl-a6.so")

  (error? ; cannot find a6
    (separate-eval '(begin (import (testfile-cwl-d6)) (runit))))

  (delete-file "testfile-cwl-d6.so")

  (begin
    (rename-file "testfile-cwl-a6.ss.spam" "testfile-cwl-a6.ss")
    (rename-file "testfile-cwl-b6.ss.spam" "testfile-cwl-b6.ss")
    (rename-file "testfile-cwl-c6.ss.spam" "testfile-cwl-c6.ss")
    (rename-file "testfile-cwl-d6.ss.spam" "testfile-cwl-d6.ss")
    #t)

  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      "testfile-cwl-d6")
    #t)

  (delete-file "testfile-cwl-c6.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-d6)
    "((testfile-cwl-c6))\n")

  (delete-file "testfile-cwl-a6.so")
  (delete-file "testfile-cwl-a6.wpo")
  (delete-file "testfile-cwl-b6.so")
  (delete-file "testfile-cwl-b6.wpo")
  (delete-file "testfile-cwl-d6.wpo")
  (delete-file "testfile-cwl-a6.ss")
  (delete-file "testfile-cwl-b6.ss")
  (delete-file "testfile-cwl-c6.ss")
  (delete-file "testfile-cwl-d6.ss")

  (equal?
    (separate-eval '(begin (import (testfile-cwl-d6)) (runit)))
    "invoking d6\n40320\n")

  (delete-file "testfile-cwl-c6.so")

  (error? ; cannot find c6
    (separate-eval '(begin (import (testfile-cwl-d6)) (runit))))

  (begin
    (with-output-to-file "testfile-cwl-a7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a7)
             (export x)
             (import (chezscheme))
             (define $x (make-parameter 1))
             (define-syntax x (identifier-syntax ($x)))
             (printf "invoking a\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-b7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b7)
             (export z)
             (import (chezscheme) (testfile-cwl-c7))
             (define $z (make-parameter (+ y 1)))
             (define-syntax z (identifier-syntax ($z)))
             (printf "invoking b\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-c7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-c7)
             (export y)
             (import (chezscheme) (testfile-cwl-a7))
             (define $y (make-parameter (+ x 1)))
             (define-syntax y (identifier-syntax ($y)))
             (printf "invoking c\n"))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-b7)
    #t)

  (delete-file "testfile-cwl-c7.wpo")
  (delete-file "testfile-cwl-c7.ss")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) "testfile-cwl-ab7.so")))
      'cwl-b7)
    "((testfile-cwl-c7))\n")

  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-a7))
      '(write x)
      '(newline)
      '(import (testfile-cwl-b7))
      '(write z)
      '(newline)
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\n1\ninvoking c\ninvoking b\n3\n2\n")

  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-a7))
      '(write x)
      '(newline)
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline)
      '(import (testfile-cwl-b7))
      '(write z)
      '(newline))
    "invoking a\n1\ninvoking c\n2\ninvoking b\n3\n")

  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-a7))
      '(write x)
      '(newline)
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
  "invoking a\n1\ninvoking c\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-b7))
      '(write z)
      '(newline)
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\ninvoking b\n3\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-a7))
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-b7))
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-a7) (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-c7) (testfile-cwl-b7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\n2\n")
  (equal?
    (separate-eval
      '(load "testfile-cwl-ab7.so")
      '(import (testfile-cwl-c7))
      '(write y)
      '(newline))
    "invoking a\ninvoking c\n2\n")

  (begin
    (with-output-to-file "testfile-cwl-a8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a8)
             (export x)
             (import (chezscheme))
             (define x (gensym))
             (printf "invoking a\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-b8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b8)
             (export z)
             (import (chezscheme) (testfile-cwl-c8))
             (define z (cons 'b y))
             (printf "invoking b\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-c8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-c8)
             (export y)
             (import (chezscheme) (testfile-cwl-a8))
             (define y (cons 'c x))
             (printf "invoking c\n"))))
      'replace)
    (with-output-to-file "testfile-cwl-d8.ss"
      (lambda ()
        (pretty-print 
          '(library (testfile-cwl-d8)
             (export runit)
             (import (chezscheme) (testfile-cwl-c8) (testfile-cwl-a8) (testfile-cwl-b8))
             (define (runit yes?)
               (pretty-print (eq? (cdr y) x))
               (pretty-print (eq? (cdr z) y))
               (pretty-print (and (eq? (car y) 'c) (eq? (car z) 'b)))
               (when yes? (eq? (eval 'x (environment '(testfile-cwl-a8))) x))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-d8)
    #t)

  (equal?
    (separate-eval '(begin (import (testfile-cwl-d8)) (runit #f)))
    "invoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n")

  (equal?
    (separate-eval '(begin (import (testfile-cwl-d8)) (runit #t)))
    "invoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n#t\n")

  (delete-file "testfile-cwl-c8.ss")
  (delete-file "testfile-cwl-c8.wpo")

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-d8)
    "((testfile-cwl-c8))\n")

  (equal?
     (separate-eval '(begin (import (testfile-cwl-d8)) (runit #f)))
    "invoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n")

  (equal?
    (separate-eval '(begin (import (testfile-cwl-d8)) (runit #t)))
    "invoking a\ninvoking c\ninvoking b\n#t\n#t\n#t\n#t\n")

  (begin
    (with-output-to-file "testfile-cwl-a9.ss"
      (lambda ()
        (pretty-print
          '(eval-when (visit)
             (library (testfile-cwl-a9)
               (export x)
               (import (chezscheme))
               (define x 5)))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-a9)
    #t)

  (error? ; found visit-only run-time library (testfile-cwl-a9)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-a9))

  (begin
    (with-output-to-file "testfile-cwl-a10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a10)
             (export f x)
             (import (chezscheme) (testfile-cwl-b10))
             (define f (lambda (x) (* x 17)))
             (define x 5))))
      'replace)
    (with-output-to-file "testfile-cwl-b10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b10)
             (export g y)
             (import (chezscheme))
             (define g (lambda (x) (+ x 23)))
             (define y 37))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-a10)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-a10)
    #t)

  (delete-file "testfile-cwl-a10.ss")
  (delete-file "testfile-cwl-a10.wpo")
  (delete-file "testfile-cwl-b10.ss")
  (delete-file "testfile-cwl-b10.so")
  (delete-file "testfile-cwl-b10.wpo")

  (test-cp0-expansion
    `(let ()
       (import (testfile-cwl-a10) (testfile-cwl-b10))
       (+ (f (g y)) x))
    `(begin
       (#3%$invoke-library '(testfile-cwl-b10) '() ',gensym?)
       (#3%$invoke-library '(testfile-cwl-a10) '() ',gensym?)
       1025))

  (begin
    (with-output-to-file "testfile-cwl-a11.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a11)
             (export f x)
             (import (chezscheme) (testfile-cwl-b11))
             (define f (lambda (x) (* x 17)))
             (define x 5))))
      'replace)
    (with-output-to-file "testfile-cwl-b11.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b11)
             (export g y)
             (import (chezscheme))
             (define g (lambda (x) (+ x 23)))
             (define y 37))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-a11)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t] [run-cp0 (lambda (cp0 x) x)])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-a11)
    #t)

  (delete-file "testfile-cwl-a11.ss")
  (delete-file "testfile-cwl-a11.wpo")
  (delete-file "testfile-cwl-b11.ss")
  (delete-file "testfile-cwl-b11.so")
  (delete-file "testfile-cwl-b11.wpo")

  (test-cp0-expansion
    `(let ()
       (import (testfile-cwl-a11) (testfile-cwl-b11))
       (+ (f (g y)) x))
    `(begin
       (#3%$invoke-library '(testfile-cwl-b11) '() ',gensym?)
       (#3%$invoke-library '(testfile-cwl-a11) '() ',gensym?)
       ,(lambda (x) (not (eqv? x 1025)))))

  (begin
    (delete-file "testfile-cwl-a12.so")
    (delete-file "testfile-cwl-a12.wpo")
    (delete-file "testfile-cwl-b12.so")
    (delete-file "testfile-cwl-b12.wpo")
    (with-output-to-file "testfile-cwl-a12.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a12)
             (export f)
             (import (chezscheme))
             (define f (lambda (x) (* x 17))))))
      'replace)
    (with-output-to-file "testfile-cwl-b12.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b12)
             (export g f)
             (import (chezscheme) (testfile-cwl-a12))
             (define g (lambda (x) (+ x 23))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'cwl-b12)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'cwl-b12)
    #t)
  
  (equal?
    (separate-eval '(let () (import (testfile-cwl-b12)) (list (f 3) (g 5))))
    "(51 28)\n")

  (begin
    (delete-file "testfile-cwl-a13.so")
    (delete-file "testfile-cwl-a13.wpo")
    (delete-file "testfile-cwl-b13.so")
    (delete-file "testfile-cwl-b13.wpo")
    (delete-file "testfile-cwl-c13.so")
    (delete-file "testfile-cwl-c13.wpo")
    (with-output-to-file "testfile-cwl-a13.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-a13)
             (export a)
             (import (chezscheme))
             (define-syntax a (identifier-syntax f))
             (define f (lambda (x) (* x 17))))))
      'replace)
    (with-output-to-file "testfile-cwl-b13.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-cwl-b13)
             (export g a)
             (import (chezscheme) (testfile-cwl-a13))
             (define g (lambda (x) (a x))))))
      'replace)
    (with-output-to-file "testfile-cwl-c13.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme) (testfile-cwl-b13))
            (pretty-print (list (g 3) (a 5) (eval '(a 7) (environment '(testfile-cwl-a13))))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-library x)))
      'cwl-a13)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-library x)))
      'cwl-b13)
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-program x)))
      'cwl-c13)
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a.so" x)))
      'cwl-c13)
    #t)
  
  (equal?
    (separate-eval '(load-program "testfile-cwl-c13.so"))
    "(51 85 119)\n")

  (begin
    (with-output-to-file "testfile-wpo-extlib-1.chezscheme.sls"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-extlib-1)
             (export magic)
             (import (rnrs))
             (define magic (cons 9 5)))))
      'replace)
    (with-output-to-file "testfile-wpo-extlib-2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-extlib-2)
             (export p)
             (import (chezscheme) (testfile-wpo-extlib))
             (define p
               (lambda ()
                 (pretty-print magic))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-library x)))
      'wpo-extlib-2)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #t])
           (compile-whole-library (format "~a.wpo" x) (format "~a-all.so" x))))
      'wpo-extlib-2)
    "()\n")

  (equal?
    (separate-eval '(let () (import (testfile-wpo-extlib-2)) (p)))
    "(9 . 5)\n")

  ;; regression tests from @owaddell generated to fix problems he encountered
  ;; with compile-whole-library from a test generator.
  (begin
    (with-output-to-file "testfile-wpo-coconut.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-coconut)
             (export coconut apple->coconut)
             (import (scheme))
             (define $init (list '_))
             (define apple->coconut (cons 'apple->coconut $init))
             (define coconut (list 'coconut apple->coconut $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-banana.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-banana)
             (export banana apple->banana)
             (import (scheme))
             (define $init (list '_))
             (define apple->banana (cons 'apple->banana $init))
             (define banana (list 'banana apple->banana $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-apple.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-apple)
             (export apple)
             (import (scheme) (testfile-wpo-banana) (testfile-wpo-coconut))
             (define $init
               (list
                 '_
                 (cons 'apple->banana apple->banana)
                 (cons 'apple->coconut apple->coconut)))
             (define apple (list 'apple $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-main.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-wpo-banana) (testfile-wpo-coconut) (testfile-wpo-apple)))
        (pretty-print '(pretty-print banana))
        (pretty-print '(pretty-print coconut))
        (pretty-print '(pretty-print apple)))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-main)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-coconut)
    "()\n")

  (begin
    (delete-file "testfile-wpo-coconut.wpo")
    #t)
  
  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-apple)
    "((testfile-wpo-coconut))\n")

  (begin
    (delete-file "testfile-wpo-banana.wpo")
    (delete-file "testfile-wpo-apple.wpo")
    (delete-file "testfile-wpo-banana.so")
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([library-search-handler
                         (let ([lsh (library-search-handler)])
                           (lambda (who name dirs exts)
                             (lsh who (if (equal? name '(testfile-wpo-banana))
                                          '(testfile-wpo-apple)
                                          name)
                               dirs exts)))])
           (compile-whole-program (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-main)
    "((testfile-wpo-apple)\n  (testfile-wpo-banana)\n  (testfile-wpo-coconut))\n")

  (equal?
    (separate-eval 
      '(parameterize ([library-search-handler
                        (let ([lsh (library-search-handler)])
                          (lambda (who name dirs exts)
                            (lsh who (if (equal? name '(testfile-wpo-banana))
                                         '(testfile-wpo-apple)
                                         name)
                                 dirs exts)))])
         (load-program "testfile-wpo-main.so")))
    (string-append
      "(banana (apple->banana _) (_))\n"
      "(coconut (apple->coconut _) (_))\n"
      "(apple\n  (_ (apple->banana apple->banana _)\n     (apple->coconut apple->coconut _)))\n"))

  (begin
    ;; clean-up to make sure previous builds don't get in the way.
    (delete-file "testfile-wpo-coconut.ss")
    (delete-file "testfile-wpo-coconut.so")
    (delete-file "testfile-wpo-coconut.wpo")

    (delete-file "testfile-wpo-banana.ss")
    (delete-file "testfile-wpo-banana.so")
    (delete-file "testfile-wpo-banana.wpo")

    (delete-file "testfile-wpo-apple.ss")
    (delete-file "testfile-wpo-apple.so")
    (delete-file "testfile-wpo-apple.wpo")

    (delete-file "testfile-wpo-main.ss")
    (delete-file "testfile-wpo-main.so")
    (delete-file "testfile-wpo-main.wpo")

    #t)

  (begin
    (with-output-to-file "testfile-wpo-coconut.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-coconut)
             (export coconut banana->coconut apple->coconut)
             (import (scheme))
             (define $init (list '_))
             (define banana->coconut (cons 'banana->coconut $init))
             (define apple->coconut (cons 'apple->coconut $init))
             (define coconut
               (list 'coconut banana->coconut apple->coconut $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-date.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-date)
             (export date apple->date)
             (import (scheme))
             (define $init (list '_))
             (define apple->date (cons 'apple->date $init))
             (define date (list 'date apple->date $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-apple.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-apple)
             (export apple)
             (import (scheme) (testfile-wpo-date) (testfile-wpo-coconut))
             (define $init
               (list
                 '_
                 (cons 'apple->date apple->date)
                 (cons 'apple->coconut apple->coconut)))
             (define apple (list 'apple $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-banana.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-banana)
             (export banana)
             (import (scheme) (testfile-wpo-coconut))
             (define $init
               (list '_ (cons 'banana->coconut banana->coconut)))
             (define banana (list 'banana $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-main.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-wpo-date)
                         (testfile-wpo-banana) (testfile-wpo-coconut)
                         (testfile-wpo-apple)))
        (pretty-print '(pretty-print date))
        (pretty-print '(pretty-print banana))
        (pretty-print '(pretty-print coconut))
        (pretty-print '(pretty-print apple)))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-main)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-coconut)
    "()\n")

  (begin
    (delete-file "testfile-wpo-coconut.wpo")
    #t)
  
  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-apple)
    "((testfile-wpo-coconut))\n")

  (begin
    (delete-file "testfile-wpo-date.wpo")
    (delete-file "testfile-wpo-apple.wpo")
    (delete-file "testfile-wpo-date.so")
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([library-search-handler
                         (let ([lsh (library-search-handler)])
                           (lambda (who name dirs exts)
                             (lsh who (if (equal? name '(testfile-wpo-date))
                                          '(testfile-wpo-apple)
                                          name)
                               dirs exts)))])
           (compile-whole-program (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-main)
    "((testfile-wpo-apple)\n  (testfile-wpo-date)\n  (testfile-wpo-coconut))\n")

  (equal?
    (separate-eval 
      '(parameterize ([library-search-handler
                       (let ([lsh (library-search-handler)])
                         (lambda (who name dirs exts)
                           (lsh who (if (equal? name '(testfile-wpo-date))
                                        '(testfile-wpo-apple)
                                        name)
                             dirs exts)))])
         (load-program "testfile-wpo-main.so")))
    (string-append
      "(date (apple->date _) (_))\n"
      "(banana (_ (banana->coconut banana->coconut _)))\n"
      "(coconut (banana->coconut _) (apple->coconut _) (_))\n"
      "(apple\n"
      "  (_ (apple->date apple->date _)\n"
      "     (apple->coconut apple->coconut _)))\n"))

  (begin
    ;; clean-up to make sure previous builds don't get in the way.
    (delete-file "testfile-wpo-coconut.ss")
    (delete-file "testfile-wpo-coconut.so")
    (delete-file "testfile-wpo-coconut.wpo")

    (delete-file "testfile-wpo-date.ss")
    (delete-file "testfile-wpo-date.so")
    (delete-file "testfile-wpo-date.wpo")

    (delete-file "testfile-wpo-banana.ss")
    (delete-file "testfile-wpo-banana.so")
    (delete-file "testfile-wpo-banana.wpo")

    (delete-file "testfile-wpo-apple.ss")
    (delete-file "testfile-wpo-apple.so")
    (delete-file "testfile-wpo-apple.wpo")

    (delete-file "testfile-wpo-main.ss")
    (delete-file "testfile-wpo-main.so")
    (delete-file "testfile-wpo-main.wpo")

    #t)

  (begin
    (with-output-to-file "testfile-wpo-date.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-date)
             (export date apple->date)
             (import (scheme))
             (define $init (list '_))
             (define apple->date (cons 'apple->date $init))
             (define date (list 'date apple->date $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-eel.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-eel)
             (export eel coconut->eel apple->eel)
             (import (scheme))
             (define $init (list '_))
             (define coconut->eel (cons 'coconut->eel $init))
             (define apple->eel (cons 'apple->eel $init))
             (define eel (list 'eel coconut->eel apple->eel $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-coconut.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-coconut)
             (export coconut banana->coconut apple->coconut)
             (import (scheme) (testfile-wpo-eel))
             (define $init (list '_ (cons 'coconut->eel coconut->eel)))
             (define banana->coconut (cons 'banana->coconut $init))
             (define apple->coconut (cons 'apple->coconut $init))
             (define coconut
               (list 'coconut banana->coconut apple->coconut $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-apple.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-apple)
             (export apple)
             (import (scheme) (testfile-wpo-date) (testfile-wpo-coconut)
               (testfile-wpo-eel))
             (define $init
               (list
                 '_
                 (cons 'apple->date apple->date)
                 (cons 'apple->coconut apple->coconut)
                 (cons 'apple->eel apple->eel)))
             (define apple (list 'apple $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-banana.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-wpo-banana)
             (export banana)
             (import (scheme) (testfile-wpo-coconut))
             (define $init
               (list '_ (cons 'banana->coconut banana->coconut)))
             (define banana (list 'banana $init)))))
      'replace)
    (with-output-to-file "testfile-wpo-main.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-wpo-date)
                         (testfile-wpo-banana) (testfile-wpo-coconut)
                         (testfile-wpo-apple) (testfile-wpo-eel)))
        (pretty-print '(pretty-print date))
        (pretty-print '(pretty-print banana))
        (pretty-print '(pretty-print coconut))
        (pretty-print '(pretty-print apple))
        (pretty-print '(pretty-print eel)))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t]
                        [generate-wpo-files #t])
           (compile-program x)))
      'wpo-main)
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-coconut)
    "()\n")

  (begin
    (delete-file "testfile-wpo-eel.wpo")
    (delete-file "testfile-wpo-coconut.wpo")
    (delete-file "testfile-wpo-eel.so")
    #t)
  
  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-wpo-files #f]
                        [library-search-handler
                         (let ([lsh (library-search-handler)])
                           (lambda (who name dirs exts)
                             (lsh who (if (equal? name '(testfile-wpo-eel))
                                          '(testfile-wpo-coconut)
                                          name)
                               dirs exts)))])
           (compile-whole-library (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-apple)
    "((testfile-wpo-coconut) (testfile-wpo-eel))\n")

  (begin
    (delete-file "testfile-wpo-date.wpo")
    (delete-file "testfile-wpo-apple.wpo")
    (delete-file "testfile-wpo-date.so")
    #t)

  (equal?
    (separate-compile
      '(lambda (x)
         (parameterize ([library-search-handler
                         (let ([lsh (library-search-handler)])
                           (lambda (who name dirs exts)
                             (lsh who (cond
                                        [(equal? name '(testfile-wpo-date)) '(testfile-wpo-apple)]
                                        [(equal? name '(testfile-wpo-eel)) '(testfile-wpo-coconut)]
                                        [else name])
                               dirs exts)))])
           (compile-whole-program (format "~a.wpo" x) (format "~a.so" x))))
      'wpo-main)
    "((testfile-wpo-apple)\n  (testfile-wpo-date)\n  (testfile-wpo-coconut)\n  (testfile-wpo-eel))\n")

  (equal?
    (separate-eval 
      '(parameterize ([library-search-handler
                       (let ([lsh (library-search-handler)])
                         (lambda (who name dirs exts)
                           (lsh who (cond
                                      [(equal? name '(testfile-wpo-date)) '(testfile-wpo-apple)]
                                      [(equal? name '(testfile-wpo-eel)) '(testfile-wpo-coconut)]
                                      [else name])
                             dirs exts)))])
         (load-program "testfile-wpo-main.so")))
    (string-append
      "(date (apple->date _) (_))\n"
      "(banana\n"
      "  (_ (banana->coconut\n"
      "       banana->coconut\n"
      "       _\n"
      "       (coconut->eel coconut->eel _))))\n"
      "(coconut\n"
      "  (banana->coconut _ (coconut->eel coconut->eel _))\n"
      "  (apple->coconut _ (coconut->eel coconut->eel _))\n"
      "  (_ (coconut->eel coconut->eel _)))\n"
      "(apple\n"
      "  (_ (apple->date apple->date _)\n"
      "     (apple->coconut\n"
      "       apple->coconut\n"
      "       _\n"
      "       (coconut->eel coconut->eel _))\n"
      "     (apple->eel apple->eel _)))\n"
      "(eel (coconut->eel _) (apple->eel _) (_))\n"))

  (begin
    ;; clean-up to make sure previous builds don't get in the way.
    (delete-file "testfile-wpo-coconut.ss")
    (delete-file "testfile-wpo-coconut.so")
    (delete-file "testfile-wpo-coconut.wpo")

    (delete-file "testfile-wpo-eel.ss")
    (delete-file "testfile-wpo-eel.so")
    (delete-file "testfile-wpo-eel.wpo")

    (delete-file "testfile-wpo-date.ss")
    (delete-file "testfile-wpo-date.so")
    (delete-file "testfile-wpo-date.wpo")

    (delete-file "testfile-wpo-banana.ss")
    (delete-file "testfile-wpo-banana.so")
    (delete-file "testfile-wpo-banana.wpo")

    (delete-file "testfile-wpo-apple.ss")
    (delete-file "testfile-wpo-apple.so")
    (delete-file "testfile-wpo-apple.wpo")

    (delete-file "testfile-wpo-main.ss")
    (delete-file "testfile-wpo-main.so")
    (delete-file "testfile-wpo-main.wpo")

    #t)

  (begin
    (with-output-to-file "testfile-deja-vu-one.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-deja-vu-one)
            (export a)
            (import (scheme))
            (define a 3))))
      'replace)
    (with-output-to-file "testfile-deja-vu-two.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-deja-vu-two)
            (export b)
            (import (scheme) (testfile-deja-vu-one))
            (define b (list 'b a)))))
      'replace)
    (with-output-to-file "testfile-deja-vu-dup.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-deja-vu-dup)
            (export d)
            (import (scheme) (testfile-deja-vu-one))
            (define d (list a 'd)))))
      'replace)
    (with-output-to-file "testfile-deja-vu-main.ss"
      (lambda ()
        (for-each pretty-print
          '((import (scheme) (testfile-deja-vu-one) (testfile-deja-vu-two) (testfile-deja-vu-dup))
            (pretty-print (list a b d)))))
      'replace)
    (separate-eval
     '(parameterize ([generate-wpo-files #t])
        (compile-library "testfile-deja-vu-one")
        (compile-library "testfile-deja-vu-two")
        (compile-library "testfile-deja-vu-dup")
        (compile-program "testfile-deja-vu-main")
        (compile-whole-library "testfile-deja-vu-one.wpo" "testfile-deja-vu-one.done")
        (compile-whole-library "testfile-deja-vu-two.wpo" "testfile-deja-vu-two.done")
        (compile-whole-library "testfile-deja-vu-dup.wpo" "testfile-deja-vu-dup.done")))
    #t)

  (error?
   (separate-eval
    '(compile-whole-program "testfile-deja-vu-main.wpo" "testfile-deja-vu-main.done")))

  (begin
    (do ([stem '("one" "two" "dup" "main") (cdr stem)]) ((null? stem))
      (do ([ext '("ss" "so" "wpo" "done") (cdr ext)]) ((null? ext))
        (delete-file (format "testfile-deja-vu-~a.~a" (car stem) (car ext)))))
    #t)

  ; verify compatibility of generate-covin-files and generate-wpo-files
  (begin
    (mkfile "testfile-cwl-a14.ss"
      '(library (testfile-cwl-a14) (export a) (import (scheme)) (define a 123)))
    (parameterize ([generate-covin-files #t]
                   [generate-wpo-files #t])
      (compile-library "testfile-cwl-a14")
      (compile-whole-library "testfile-cwl-a14.wpo" "testfile-cwl-a14.library"))
    #t)

  (file-exists? "testfile-cwl-a14.covin")

  (eqv?
    (let () (import (testfile-cwl-a14)) a)
    123)

  (eqv?
    (separate-eval
      '(verify-loadability 'load "testfile-cwl-a14.library"))
    "")
)

(mat maybe-compile-whole
  (begin
    (delete-file "testfile-mcw-a1.so")
    (delete-file "testfile-mcw-a1.wpo")
    (delete-file "testfile-mcw-b1.so")
    (delete-file "testfile-mcw-b1.wpo")
    (delete-file "testfile-mcw-c1.so")
    (delete-file "testfile-mcw-c1.wpo")
    (with-output-to-file "testfile-mcw-ha1.ss"
      (lambda ()
        (pretty-print
          '(define minor-msg-number 97)))
      'replace)
    (with-output-to-file "testfile-mcw-hb1.ss"
      (lambda ()
        (pretty-print
          '(define major-msg-number 113)))
      'replace)
    (with-output-to-file "testfile-mcw-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-a1)
             (export a)
             (import (chezscheme))
             (define a "hello from a"))))
      'replace)
    (with-output-to-file "testfile-mcw-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-b1)
             (export b)
             (import (chezscheme) (testfile-mcw-a1))
             (include "testfile-mcw-ha1.ss")
             (define b (lambda () (format "~a and b [~s]" a minor-msg-number))))))
      'replace)
    (with-output-to-file "testfile-mcw-c1.ss"
      (lambda ()
        (for-each pretty-print
          '((import (chezscheme) (testfile-mcw-b1))
            (include "testfile-mcw-hb1.ss")
            (printf "~a and c [~s]\n" (b) major-msg-number))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (compile-program x)))
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "hello from a and b [97] and c [113]\n")

  (begin
    (with-output-to-file "testfile-mcw-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-a1)
             (export a)
             (import (chezscheme))
             (define a "greetings from a"))))
      'replace)
    (touch "testfile-mcw-a1.so" "testfile-mcw-a1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (maybe-compile-program x)))
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "greetings from a and b [97] and c [113]\n")

  (begin
    (separate-compile
      '(lambda (x)
         (compile-whole-program (format "~a.wpo" x) (format "~a.so" x)) #f)
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "greetings from a and b [97] and c [113]\n")

  (begin
    (with-output-to-file "testfile-mcw-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-a1)
             (export a)
             (import (chezscheme))
             (define a "salutations from a"))))
      'replace)
    (touch "testfile-mcw-a1.so" "testfile-mcw-a1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (parameterize ([compile-program-handler
                            (lambda (ifn ofn)
                              (compile-program ifn ofn)
                              (compile-whole-program (format "~a.wpo" (path-root ofn)) ofn #t))])
             (maybe-compile-program x))))
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "salutations from a and b [97] and c [113]\n")

  (begin
    (with-output-to-file "testfile-mcw-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-a1)
             (export a)
             (import (chezscheme))
             (define a "goodbye from a"))))
      'replace)
    (touch "testfile-mcw-a1.so" "testfile-mcw-a1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (parameterize ([compile-program-handler
                            (lambda (ifn ofn)
                              (compile-program ifn ofn)
                              (compile-whole-program (format "~a.wpo" (path-root ofn)) ofn #t))])
             (maybe-compile-program x))))
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "goodbye from a and b [97] and c [113]\n")

  (begin
    (with-output-to-file "testfile-mcw-hb1.ss"
      (lambda ()
        (pretty-print
          '(define major-msg-number 773)))
      'replace)
    (touch "testfile-mcw-c1.so" "testfile-mcw-hb1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (parameterize ([compile-program-handler
                            (lambda (ifn ofn)
                              (compile-program ifn ofn)
                              (compile-whole-program (format "~a.wpo" (path-root ofn)) ofn #t))])
             (maybe-compile-program x))))
      'mcw-c1)
    #t)

  (equal?
    (separate-eval '(load-program "testfile-mcw-c1.so"))
    "goodbye from a and b [97] and c [773]\n")

  (begin
    (with-output-to-file "testfile-mcw-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mcw-a1)
             (export a)
             (import (chezscheme))
             (define a "hello again from a"))))
      'replace)
    (touch "testfile-mcw-a1.so" "testfile-mcw-a1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (parameterize ([compile-library-handler
                            (lambda (ifn ofn)
                              (compile-library ifn ofn)
                              (compile-whole-library (format "~a.wpo" (path-root ofn)) ofn))])
             (maybe-compile-library x))))
      'mcw-b1)
    #t)

  (equal?
    (separate-eval '(let () (import (testfile-mcw-b1)) (printf "~a\n" (b))))
    "hello again from a and b [97]\n")

  (begin
    (with-output-to-file "testfile-mcw-ha1.ss"
      (lambda ()
        (pretty-print
          '(define minor-msg-number -53)))
      'replace)
    (touch "testfile-mcw-a1.so" "testfile-mcw-a1.ss")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
           (parameterize ([compile-library-handler
                            (lambda (ifn ofn)
                              (compile-library ifn ofn)
                              (compile-whole-library (format "~a.wpo" (path-root ofn)) ofn))])
             (maybe-compile-library x))))
      'mcw-b1)
    #t)

  (equal?
    (separate-eval '(let () (import (testfile-mcw-b1)) (printf "~a\n" (b))))
    "hello again from a and b [-53]\n")
)

(mat library-manager
  (begin
    (with-output-to-file "testfile-lm-a.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lm-a)
            (export ct-a rt-a)
            (import (scheme))
            (meta define ct-a (begin (display "ct-a rhs\n") 123))
            (define rt-a (begin (display "rt-a rhs\n") 456)))))
      'replace)
    (with-output-to-file "testfile-lm-b.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lm-b)
            (export b)
            (import (scheme) (testfile-lm-a))
            (define-syntax (use-ct-val x) (if (odd? ct-a) #'"odd" #'"even"))
            (define b use-ct-val))))
      'replace)
    (with-output-to-file "testfile-lm-c.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lm-c)
            (export c)
            (import (scheme) (testfile-lm-a))
            (define use-rt-val rt-a)
            (define c use-rt-val))))
      'replace)
    (with-output-to-file "testfile-lm-combined.ss"
      (lambda ()
        (pretty-print
         '(begin
            (include "testfile-lm-a.ss")
            (include "testfile-lm-b.ss")
            (include "testfile-lm-c.ss"))))
      'replace)
    (with-output-to-file "testfile-lm-use-b.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lm-use-b)
            (export x)
            (import (scheme) (testfile-lm-b))
            (meta define x b))))
      'replace)
    (with-output-to-file "testfile-lm-use-c.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lm-use-c)
            (export x)
            (import (scheme) (testfile-lm-c))
            (define-syntax (x x) c))))
      'replace)
    #t)
  (equal?
   (separate-eval
    '(import-notify #t)
    '(compile-library "testfile-lm-a"))
   (string-append
    "compiling testfile-lm-a.ss with output to testfile-lm-a.so\n"
    "ct-a rhs\n"))
  (equal?
   (separate-eval
    '(import-notify #t)
    '(library-extensions '((".ss" . ".so")))
    '(compile-library "testfile-lm-b")
    '(printf "b = ~s\n" (let () (import (testfile-lm-b)) b)))
   (string-append
    "compiling testfile-lm-b.ss with output to testfile-lm-b.so\n"
    "import: found source file \"testfile-lm-a.ss\"\n"
    "import: found corresponding object file \"testfile-lm-a.so\"\n"
    "import: object file is not older\n"
    "import: visiting object file \"testfile-lm-a.so\"\n"
    "ct-a rhs\n"
    "b = \"odd\"\n"))
  (equal?
   (separate-eval
    '(import-notify #t)
    '(library-extensions '((".ss" . ".so")))
    '(compile-library "testfile-lm-c")
    '(printf "c = ~s\n" (let () (import (testfile-lm-c)) c)))
   (string-append
    "compiling testfile-lm-c.ss with output to testfile-lm-c.so\n"
    "import: found source file \"testfile-lm-a.ss\"\n"
    "import: found corresponding object file \"testfile-lm-a.so\"\n"
    "import: object file is not older\n"
    "import: visiting object file \"testfile-lm-a.so\"\n"
    "attempting to 'revisit' previously 'visited' \"testfile-lm-a.so\" for library (testfile-lm-a) run-time info\n"
    "rt-a rhs\n"
    "c = 456\n"))
  (equal?
   ;; library manager revisits object file containing a single library
   ;; to resolve dependencies after earlier visit
   (separate-eval
    '(import-notify #t)
    '(library-extensions '((".ss" . ".so")))
    '(visit "testfile-lm-a.so")
    '(let () (import (testfile-lm-c)) c))
   (string-append
    "import: found source file \"testfile-lm-c.ss\"\n"
    "import: found corresponding object file \"testfile-lm-c.so\"\n"
    "import: object file is not older\n"
    "import: visiting object file \"testfile-lm-c.so\"\n"
    "import: attempting to 'revisit' previously 'visited' \"testfile-lm-c.so\" for library (testfile-lm-c) run-time info\n"
    "import: attempting to 'revisit' previously 'visited' \"testfile-lm-a.so\" for library (testfile-lm-a) run-time info\n"
    "rt-a rhs\n"
    "456\n"))
  (equal?
   ;; library manager visits object file containing a single library
   ;; to resolve dependencies after earlier revisit
   (separate-eval
    '(import-notify #t)
    '(library-extensions '((".ss" . ".so")))
    '(revisit "testfile-lm-a.so")
    '(let () (import (testfile-lm-b)) b))
   (string-append
    "import: found source file \"testfile-lm-b.ss\"\n"
    "import: found corresponding object file \"testfile-lm-b.so\"\n"
    "import: object file is not older\n"
    "import: visiting object file \"testfile-lm-b.so\"\n"
    "import: attempting to 'visit' previously 'revisited' \"testfile-lm-a.so\" for library (testfile-lm-a) compile-time info\n"
    "import: attempting to 'revisit' previously 'visited' \"testfile-lm-b.so\" for library (testfile-lm-b) run-time info\n"
    "\"odd\"\n"))
  (equal?
   (separate-eval
    '(import-notify #t)
    '(library-extensions '((".ss" . ".so")))
    '(compile-file "testfile-lm-combined"))
   (string-append
    "compiling testfile-lm-combined.ss with output to testfile-lm-combined.so\n"
    "ct-a rhs\n"))
  (equal?
   ;; library manager revisits object file containing related libraries
   ;; to resolve dependencies after earlier visit
   (separate-eval
    '(import-notify #t)
    '(visit "testfile-lm-combined.so")
    '(let ()
       (import (testfile-lm-a))
       (define-syntax (foo x) ct-a)
       (printf "foo = ~s\n" foo))
    '(let () (import (testfile-lm-c)) c))
   (string-append
    "ct-a rhs\n"
    "foo = 123\n"
    "import: attempting to 'revisit' previously 'visited' \"testfile-lm-combined.so\" for library (testfile-lm-c) run-time info\n"
    "rt-a rhs\n"
    "456\n"))
  (equal?
   ;; library manager visits object file containing related libraries
   ;; to resolve dependencies after earlier revisit
   (separate-eval
    '(import-notify #t)
    '(revisit "testfile-lm-combined.so")
    '(let ()
       (import (testfile-lm-a))
       (define foo rt-a)
       (printf "foo = ~s\n" foo))
    '(let () (import (testfile-lm-b)) b))
   (string-append
    "import: attempting to 'visit' previously 'revisited' \"testfile-lm-combined.so\" for library (testfile-lm-a) compile-time info\n"
    "rt-a rhs\n"
    "foo = 456\n"
    "\"odd\"\n"))
  (equal?
   ;; library manager does not revisit due to earlier load
   (separate-eval
    '(import-notify #t)
    '(load "testfile-lm-combined.so")
    '(let ()
       (import (testfile-lm-a))
       (define-syntax (foo x) ct-a)
       (printf "foo = ~s\n" foo))
    '(let () (import (testfile-lm-c)) c))
   (string-append
    "ct-a rhs\n"
    "foo = 123\n"
    "rt-a rhs\n"
    "456\n"))
  (equal?
   ;; library manager does not revisit due to earlier load
   (separate-eval
    '(import-notify #t)
    '(load "testfile-lm-combined.so")
    '(let ()
       (import (testfile-lm-a))
       (define foo rt-a)
       (printf "foo = ~s\n" foo))
    '(let () (import (testfile-lm-b)) b))
   (string-append
    "rt-a rhs\n"
    "foo = 456\n"
    "\"odd\"\n"))
  )

(mat verify-loadability
  (error? ; invalid argument
    (verify-loadability 'never))
  (error? ; invalid argument
    (verify-loadability 'never "hello.so"))
  (error? ; invalid argument
    (verify-loadability #f "hello.so" "goodbye.so"))
  (error? ; invalid argument
    (verify-loadability 'load 'hello))
  (error? ; invalid argument
    (verify-loadability 'load '(a . "testdir")))
  (error? ; invalid argument
    (verify-loadability 'load '#("a" "testdir")))
  (error? ; invalid argument
    (verify-loadability 'load "testfile1.so" "testfile2.so" 'hello))
  (error? ; invalid argument
    (verify-loadability 'load "testfile1.so" "testfile2.so" '(a . "testdir")))
  (error? ; invalid argument
    (verify-loadability 'load '("a" . hello)))
  (error? ; invalid argument
    (verify-loadability 'load '("a" . ("src" . "obj"))))
  (error? ; invalid argument
    (verify-loadability 'load '("a" . (("src" "obj")))))
  (error? ; invalid argument
    (verify-loadability 'load '("a" . ((("src" "obj"))))))
  (begin
    (define install
      (lambda (dir . fn*)
        (for-each
          (lambda (fn)
            (call-with-port (open-file-input-port fn)
              (lambda (ip)
                (call-with-port (open-file-output-port (format "~a/~a" dir (path-last fn)))
                  (lambda (op)
                    (put-bytevector op (get-bytevector-all ip)))))))
          fn*)))
    #t)
  (eq? (verify-loadability 'visit) (void))
  (eq? (verify-loadability 'revisit) (void))
  (eq? (verify-loadability 'load) (void))
  (error? ; not found
    (verify-loadability 'load "probably not found"))
  (begin
    (mkfile "testfile-clA.ss"
      '(import (chezscheme) (testfile-clB) (testfile-clC))
      '(printf "~a, ~a\n" b c))
    (mkfile "testfile-clB.ss"
      '(library (testfile-clB)
         (export b)
         (import (chezscheme) (testfile-clB1))
         (define-syntax go (lambda (x) (datum->syntax #'* (b1))))
         (define b (go))))
    (mkfile "testfile-clB1.ss"
      '(library (testfile-clB1)
         (export b1)
         (import (chezscheme))
         (define b1 (lambda () "hello from B1"))))
    (mkfile "testfile-clC.ss"
      '(library (testfile-clC)
         (export c)
         (import (chezscheme) (testfile-clC1))
         (define c (c1))))
    (mkfile "testfile-clC1.ss"
      '(library (testfile-clC1)
         (export c1)
         (import (chezscheme))
         (define-syntax c1 (syntax-rules () [(_) "hello from C1"]))))
    (rm-rf "testdir-obj1")
    (rm-rf "testdir-obj2")
    (mkdir "testdir-obj1")
    (mkdir "testdir-obj2")
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj1"))] [compile-imported-libraries #t])
         (compile-program "testfile-clA.ss" "testdir-obj1/testfile-clA.so")))
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj2"))] [compile-imported-libraries #t])
         (compile-program "testfile-clA.ss" "testdir-obj2/testfile-clA.so")))
    #t)
  (begin
    (rm-rf "testdir-dist1")
    (mkdir "testdir-dist1")
    (install "testdir-dist1" "testdir-obj1/testfile-clA.so" "testdir-obj1/testfile-clB.so" "testdir-obj1/testfile-clC.so")
    #t)
  (eqv?
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (verify-loadability 'visit "testfile-clA.so")
         (verify-loadability 'revisit "testfile-clA.so")
         (verify-loadability 'load "testfile-clA.so")))
    "")
  (equal?
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (load-program "testfile-clA.so")))
    "hello from B1, hello from C1\n")
  (error? ; missing B1
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (verify-loadability 'visit "testfile-clB.so"))))
  (error? ; missing B1
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (verify-loadability 'load "testfile-clB.so"))))
  (error? ; missing C1
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (verify-loadability 'visit "testfile-clC.so"))))
  (error? ; missing C1
    (separate-eval
      '(parameterize ([cd "testdir-dist1"])
         (verify-loadability 'load "testfile-clC.so"))))
  (begin
    (rm-rf "testdir-dist2")
    (mkdir "testdir-dist2")
    (install "testdir-dist2" "testdir-obj2/testfile-clA.so" "testdir-obj2/testfile-clB.so" "testdir-obj2/testfile-clC.so")
    #t)
  (equal?
    (separate-eval
      '(parameterize ([cd "testdir-dist2"])
         (load-program "testfile-clA.so")))
    "hello from B1, hello from C1\n")
  (error? ; mismatched compilation instance
    (separate-eval
      '(verify-loadability 'revisit
         '("testdir-dist1/testfile-clA.so" . "testdir-dist1")
         '("testdir-dist2/testfile-clA.so" . "testdir-dist2"))))
  (error? ; mismatched compilation instance
    (separate-eval
      '(verify-loadability 'load
         '("testdir-dist1/testfile-clA.so" . "testdir-dist1")
         '("testdir-dist2/testfile-clA.so" . "testdir-dist2"))))
  (begin
    (rm-rf "testdir-dist3")
    (mkdir "testdir-dist3")
    (install "testdir-dist3" "testdir-obj1/testfile-clA.so" "testdir-obj1/testfile-clB.so" "testdir-obj2/testfile-clC.so")
    #t)
  (error? ; mismatched compilation instance
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (load-program "testfile-clA.so"))))
  (eqv? ; no compile-time requirements, so no problem
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (verify-loadability 'visit "testfile-clA.so")))
    "")
  (error? ; mismatched compilation instance
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (verify-loadability 'revisit "testfile-clA.so"))))
  (error? ; mismatched compilation instance
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (verify-loadability 'load "testfile-clA.so"))))
  (equal?
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (unless (guard (c [else (printf "yes\n")]) (verify-loadability 'load "testfile-clA.so") #f)
           (errorf #f "oops")))
      '(parameterize ([cd "testdir-dist1"])
         (printf "~s\n" (verify-loadability 'load "testfile-clA.so")))
      '(parameterize ([cd "testdir-dist2"])
         (printf "~s\n" (verify-loadability 'load "testfile-clA.so")))
      '(let* ([ls (library-list)] [f (lambda (x) (and (member x ls) #t))])
         (printf "~s\n" (map f '((testfile-clB) (testfile-clB1) (testfile-clC) (testfile-clC1)))))
      '(parameterize ([cd "testdir-dist1"])
         (load-program "testfile-clA.so"))
      '(let* ([ls (library-list)] [f (lambda (x) (and (member x ls) #t))])
         (printf "~s\n" (map f '((testfile-clB) (testfile-clB1) (testfile-clC) (testfile-clC1))))))
    "yes\n#<void>\n#<void>\n(#f #f #f #f)\nhello from B1, hello from C1\n(#t #f #t #f)\n")
  (equal?
    (separate-eval
      '(parameterize ([cd "testdir-dist3"])
         (unless (guard (c [else (printf "yes\n")]) (verify-loadability 'load "testfile-clA.so") #f)
           (errorf #f "oops")))
      '(parameterize ([cd "testdir-dist1"])
         (printf "~s\n" (verify-loadability 'load "testfile-clA.so")))
      '(parameterize ([cd "testdir-dist2"])
         (printf "~s\n" (verify-loadability 'load "testfile-clA.so")))
      '(let* ([ls (library-list)] [f (lambda (x) (and (member x ls) #t))])
         (printf "~s\n" (map f '((testfile-clB) (testfile-clB1) (testfile-clC) (testfile-clC1)))))
      '(parameterize ([cd "testdir-dist2"])
         (load-program "testfile-clA.so"))
      '(let* ([ls (library-list)] [f (lambda (x) (and (member x ls) #t))])
         (printf "~s\n" (map f '((testfile-clB) (testfile-clB1) (testfile-clC) (testfile-clC1))))))
    "yes\n#<void>\n#<void>\n(#f #f #f #f)\nhello from B1, hello from C1\n(#t #f #t #f)\n")
  (error? ; mismatched compilation instance
    (separate-eval
     '(guard [x [#t (let* ([ps (condition-irritants x)]
                           ;; rewrite error message to hide Window separator difference
                           [new-ps (map (lambda (p)
                                          (if (string? p)
                                              (list->string (subst #\/ #\\ (string->list p)))
                                              p))
                                        ps)])
                      (raise (condition (make-irritants-condition new-ps) x)))]]
             (parameterize ([library-directories '(("testdir-dist1" . "testdir-dist1"))])
               (verify-loadability 'load "testdir-dist2/testfile-clA.so")))))
  (error? ; mismatched compilation instance
    (separate-eval
      '(parameterize ([library-directories '(("testdir-dist1" . "testdir-dist1"))])
         (verify-loadability 'load "testdir-dist1/testfile-clA.so" "testdir-dist2/testfile-clA.so"))))
  (begin
    (mkfile "testfile-clPD.ss"
      '(import (chezscheme) (testfile-clD))
      '(printf "~s\n" (make-Q)))
    (mkfile "testfile-clPE.ss"
      '(import (chezscheme) (testfile-clE))
      '(printf "~s\n" (make-Q 73)))
    (mkfile "testfile-clD.ss"
      '(library (testfile-clD) (export make-Q Q? Q-x) (import (chezscheme) (testfile-clF))
         (define-record-type Q
           (nongenerative Q)
           (fields x)
           (protocol (lambda (new) (lambda () (new f)))))))
    (mkfile "testfile-clE.ss"
      '(library (testfile-clE) (export make-Q Q? Q-x) (import (chezscheme) (testfile-clG))
         (define-record-type Q
           (nongenerative Q)
           (fields x y)
           (protocol (lambda (new) (lambda (y) (new g y)))))))
    (mkfile "testfile-clF.ss"
      '(library (testfile-clF) (export f) (import (chezscheme)) (define f 77)))
    (mkfile "testfile-clG.ss"
      '(library (testfile-clG) (export g) (import (chezscheme)) (define g 123)))
    (rm-rf "testdir-obj")
    (mkdir "testdir-obj")
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj"))] [compile-imported-libraries #t])
         (compile-program "testfile-clPD.ss" "testdir-obj/testfile-clPD.so")))
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj"))] [compile-imported-libraries #t])
         (compile-program "testfile-clPE.ss" "testdir-obj/testfile-clPE.so")))
    #t)
  (begin
    (rm-rf "testdir-dist")
    (mkdir "testdir-dist")
    (install "testdir-dist" "testdir-obj/testfile-clPD.so" "testdir-obj/testfile-clD.so" "testdir-obj/testfile-clF.so")
    (install "testdir-dist" "testdir-obj/testfile-clPE.so" "testdir-obj/testfile-clE.so" "testdir-obj/testfile-clG.so")
    #t)
  (error? ; incompatible record-type Q
    (separate-eval
      '(cd "testdir-dist")
      '(load-program "testfile-clPD.so")
      '(load-program "testfile-clPE.so")))
  (equal?
    (separate-eval
      '(cd "testdir-dist")
      '(verify-loadability 'visit "testfile-clPD.so" "testfile-clPE.so")
      '(verify-loadability 'visit "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'visit "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'revisit "testfile-clPD.so" "testfile-clPE.so")
      '(verify-loadability 'revisit "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'revisit "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clPE.so")
      '(verify-loadability 'load "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'load "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clPE.so" "testfile-clD.so" "testfile-clE.so" "testfile-clF.so" "testfile-clG.so")
      '(load-program "testfile-clPD.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clE.so"))
    "#[Q 77]\n")
  (equal?
    (separate-eval
      '(cd "testdir-dist")
      '(verify-loadability 'visit "testfile-clPD.so" "testfile-clE.so")
      '(verify-loadability 'visit "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'visit "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'revisit "testfile-clPD.so" "testfile-clE.so")
      '(verify-loadability 'revisit "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'revisit "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clE.so")
      '(verify-loadability 'load "testfile-clD.so" "testfile-clE.so")
      '(verify-loadability 'load "testfile-clF.so" "testfile-clG.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clPE.so" "testfile-clD.so" "testfile-clE.so" "testfile-clF.so" "testfile-clG.so")
      '(load-program "testfile-clPE.so")
      '(verify-loadability 'load "testfile-clPD.so" "testfile-clE.so"))
    "#[Q 123 73]\n")
  (begin
    (mkfile "testfile-clH0.ss"
      '(library (testfile-clH0) (export h0) (import (chezscheme))
         (define h0 (lambda (x) (cons x 'a)))))
    (mkfile "testfile-clH1.ss"
      '(top-level-program
         (import (chezscheme) (testfile-clH0))
         (printf "~s\n" (h0 73))))
    (mkfile "testfile-clH2.ss"
      '(include "testfile-clH0.ss")
      '(top-level-program
         (import (chezscheme) (testfile-clH0))
         (printf "~s\n" (h0 37))))
    (rm-rf "testdir-obj")
    (mkdir "testdir-obj")
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj"))] [compile-imported-libraries #t])
         (compile-file "testfile-clH1.ss" "testdir-obj/testfile-clH1.so")))
    (separate-eval
      '(parameterize ([library-directories '(("." . "testdir-obj"))] [compile-imported-libraries #t])
         (compile-file "testfile-clH2.ss" "testdir-obj/testfile-clH2.so")))
    #t)
  (equal?
    (separate-eval
      '(parameterize ([library-directories '(("testdir-obj" . "testdir-obj"))])
         (revisit "testdir-obj/testfile-clH1.so")))
    "(73 . a)\n")
  (equal?
    (separate-eval
      '(parameterize ([library-directories '(("testdir-obj" . "testdir-obj"))])
         (revisit "testdir-obj/testfile-clH2.so")))
    "(37 . a)\n")
  (eqv?
    (separate-eval
      '(let ([libdirs '(("testdir-obj" . "testdir-obj"))])
         (verify-loadability 'revisit (cons "testdir-obj/testfile-clH1.so" libdirs) (cons "testdir-obj/testfile-clH2.so" libdirs))))
    "")
  (error? ; mismatched compilation instance
    (separate-eval
      '(let ([libdirs '(("testdir-obj" . "testdir-obj"))])
         (verify-loadability 'revisit (cons "testdir-obj/testfile-clH2.so" libdirs) (cons "testdir-obj/testfile-clH1.so" libdirs)))))

  ; make sure verify-loadability respects eval-when forms
  (begin
    (mkfile "testfile-clI0.ss"
      '(library (testfile-clI0) (export x) (import (chezscheme)) (define x 10) (printf "invoking I0\n")))
    (mkfile "testfile-clI1.ss"
      '(eval-when (visit)
         (top-level-program
           (import (chezscheme) (testfile-clI0))
           (printf "running I1, x = ~s\n" x))))
    (separate-eval
      '(parameterize ([compile-imported-libraries #t])
         (compile-file "testfile-clI1")))
    #t)
  (equal?
    (separate-eval '(visit "testfile-clI1.so"))
    "invoking I0\nrunning I1, x = 10\n")
  (equal?
    (separate-eval '(revisit "testfile-clI1.so"))
    "")
  (equal?
    (separate-eval '(load "testfile-clI1.so"))
    "invoking I0\nrunning I1, x = 10\n")
  (eq?
    (verify-loadability 'visit "testfile-clI1.so")
    (void))
  (eq?
    (verify-loadability 'revisit "testfile-clI1.so")
    (void))
  (eq?
    (verify-loadability 'load "testfile-clI1.so")
    (void))
  (delete-file "testfile-clI0.ss")
  (delete-file "testfile-clI0.so")
  (error?
    (verify-loadability 'visit "testfile-clI1.so"))
  (eq?
    (verify-loadability 'revisit "testfile-clI1.so")
    (void))
  (error?
    (verify-loadability 'load "testfile-clI1.so"))

  ; make sure compile-whole-program preserves the information verify-loadability needs
  (begin
    (mkfile "testfile-clJ0.ss"
      '(library (testfile-clJ0) (export x0) (import (chezscheme)) (define x0 'eat) (printf "invoking J0\n")))
    (mkfile "testfile-clJ1.ss"
      '(library (testfile-clJ1) (export x1) (import (chezscheme) (testfile-clJ0)) (define x1 (list x0 'oats)) (printf "invoking J1\n")))
    (mkfile "testfile-clJ2.ss"
      '(library (testfile-clJ2) (export x2) (import (chezscheme) (testfile-clJ1)) (define x2 (cons 'mares x1)) (printf "invoking J2\n")))
    (mkfile "testfile-clJ3.ss"
      '(import (chezscheme) (testfile-clJ2))
      '(printf "running J3, x2 = ~s\n" x2))
    (separate-eval
      '(parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
         (compile-program "testfile-clJ3")))
    #t)

  (equal?
    (separate-eval '(verify-loadability 'load "testfile-clJ3.so"))
    "")

  (equal?
    (separate-eval '(load-program "testfile-clJ3.so"))
    "invoking J0\ninvoking J1\ninvoking J2\nrunning J3, x2 = (mares eat oats)\n")

  (delete-file "testfile-clJ0.ss")
  (delete-file "testfile-clJ0.wpo")
  (delete-file "testfile-clJ2.ss")
  (delete-file "testfile-clJ2.wpo")

  ((lambda (x ls) (and (member x ls) #t))
    (separate-eval
      '(compile-whole-program "testfile-clJ3.wpo" "testfile-clJ3-all.so"))
    '("((testfile-clJ0) (testfile-clJ2))\n"
      "((testfile-clJ2) (testfile-clJ0))\n"))

  (delete-file "testfile-clJ1.ss")
  (delete-file "testfile-clJ1.wpo")
  (delete-file "testfile-clJ1.so")

  (equal?
    (separate-eval '(verify-loadability 'load "testfile-clJ3-all.so"))
    "")

  (equal?
    (separate-eval '(load-program "testfile-clJ3-all.so"))
    "invoking J0\ninvoking J1\ninvoking J2\nrunning J3, x2 = (mares eat oats)\n")

  (eq?
    (rename-file "testfile-clJ0.so" "testfile-clJ0.sav")
    (void))

  (error? ; missing testfile-clJ0.so
    (separate-eval '(verify-loadability 'load "testfile-clJ3-all.so")))

  (error? ; missing testfile-clJ0.so
    (separate-eval '(load-program "testfile-clJ3-all.so")))

  (eq?
    (rename-file "testfile-clJ0.sav" "testfile-clJ0.so")
    (void))

  (delete-file "testfile-clJ2.so")

  (error? ; missing testfile-clJ2.so
    (separate-eval '(verify-loadability 'load "testfile-clJ3-all.so")))

  (error? ; missing testfile-clJ2.so
    (separate-eval '(load-program "testfile-clJ3-all.so")))

  (begin
    (mkfile "testfile-clK0.ss"
      '(library (testfile-clK0) (export x0) (import (chezscheme)) (define x0 "chocolate") (printf "invoking K0\n")))
    (mkfile "testfile-clK1.ss"
      '(library (testfile-clK1) (export x1) (import (chezscheme) (testfile-clK0)) (define x1 (format "~a chip" x0)) (printf "invoking K1\n")))
    (mkfile "testfile-clK2.ss"
      '(import (chezscheme) (testfile-clK1))
      '(printf "running K2, x1 = ~s\n" x1))
    (separate-eval
      '(parameterize ([compile-imported-libraries #t])
         (compile-program "testfile-clK2")))
    #t)
  (eq?
    (verify-loadability 'visit "testfile-clK1.so")
    (void))
  (eq?
    (verify-loadability 'revisit "testfile-clK1.so")
    (void))
  (eq?
    (verify-loadability 'load "testfile-clK1.so")
    (void))
  (eq?
    (verify-loadability 'visit "testfile-clK1.so" "testfile-clK2.so")
    (void))
  (eq?
    (verify-loadability 'revisit "testfile-clK1.so" "testfile-clK2.so")
    (void))
  (eq?
    (verify-loadability 'load "testfile-clK1.so" "testfile-clK2.so")
    (void))
  (eq?
    (verify-loadability 'visit "testfile-clK2.so" "testfile-clK1.so")
    (void))
  (eq?
    (verify-loadability 'revisit "testfile-clK2.so" "testfile-clK1.so")
    (void))
  (eq?
    (verify-loadability 'load "testfile-clK2.so" "testfile-clK1.so")
    (void))
  (equal?
    (separate-eval
      '(visit "testfile-clK1.so")
      '(let () (import (testfile-clK1)) x1))
    "invoking K0\ninvoking K1\n\"chocolate chip\"\n")
  (equal?
    (separate-eval '(revisit "testfile-clK2.so"))
    "invoking K0\ninvoking K1\nrunning K2, x1 = \"chocolate chip\"\n")
  (eq?
    (strip-fasl-file "testfile-clK0.so" "testfile-clK0.so"
      (fasl-strip-options compile-time-information))
    (void))
  (error? ; missing compile-time info for K0
    (verify-loadability 'visit "testfile-clK1.so"))
  (eq?
    (verify-loadability 'revisit "testfile-clK1.so")
    (void))
  (error? ; missing compile-time info for K0
    (verify-loadability 'load "testfile-clK1.so"))
  (error? ; missing compile-time info
    (separate-eval
      '(visit "testfile-clK1.so")
      '(let () (import (testfile-clK1)) x1)))
  (equal?
    (separate-eval '(revisit "testfile-clK2.so"))
    "invoking K0\ninvoking K1\nrunning K2, x1 = \"chocolate chip\"\n")
)

(mat concatenate-object-files
  (begin
    (define install
      (lambda (dir . fn*)
        (for-each
          (lambda (fn)
            (call-with-port (open-file-input-port fn)
              (lambda (ip)
                (call-with-port (open-file-output-port (format "~a/~a" dir (path-last fn)))
                  (lambda (op)
                    (put-bytevector op (get-bytevector-all ip)))))))
          fn*)))
    (define test-isolated-load
      (lambda (fn lib val)
        (rm-rf "testdir-isolated")
        (mkdir "testdir-isolated")
        (install "testdir-isolated" fn)
        (separate-eval
          `(cd "testdir-isolated")
          `(load ,fn)
          `(let ()
             (import ,lib)
             ,val))))
    #t)
  (begin
    (mkfile "testfile-catlibA.ss"
      '(library (testfile-catlibA)
         (export a)
         (import (chezscheme))
         (define a 1)))
    (mkfile "testfile-catlibB.ss"
      '(library (testfile-catlibB)
         (export a b)
         (import (chezscheme) (testfile-catlibA))
         (define b 2)))
    (mkfile "testfile-catlibC.ss"
      '(library (testfile-catlibC)
         (export c)
         (import (chezscheme) (testfile-catlibB))
         (define c (+ a b))))
    (separate-eval
      '(compile-library "testfile-catlibA.ss" "testfile-catlibA.so"))
    (separate-eval
      '(compile-library "testfile-catlibB.ss" "testfile-catlibB.so"))
    (separate-eval
      '(compile-library "testfile-catlibC.ss" "testfile-catlibC.so"))
    #t)
  (eqv?
    (separate-eval
      '(begin
         (concatenate-object-files "testfile-catlibAB.so" "testfile-catlibA.so" "testfile-catlibB.so")
         (concatenate-object-files "testfile-catlibBC.so" "testfile-catlibB.so" "testfile-catlibC.so")
         (concatenate-object-files "testfile-catlibABC.so" "testfile-catlibA.so" "testfile-catlibB.so" "testfile-catlibC.so")))
    "")
  (equal?
    (test-isolated-load "testfile-catlibA.so" '(testfile-catlibA) 'a)
    "1\n")
  (error? ; can't find (testfile-catlibA)
    (test-isolated-load "testfile-catlibB.so" '(testfile-catlibB) 'b))
  (error? ; can't find (testfile-catlibA)
    (test-isolated-load "testfile-catlibBC.so" '(testfile-catlibC) 'c))
  (equal?
    (test-isolated-load "testfile-catlibABC.so" '(testfile-catlibA) 'a)
    "1\n")
  (equal?
    (test-isolated-load "testfile-catlibABC.so" '(testfile-catlibB) 'b)
    "2\n")
  (equal?
    (test-isolated-load "testfile-catlibABC.so" '(testfile-catlibC) 'c)
    "3\n")
  (equal?
    (test-isolated-load "testfile-catlibAB.so" '(testfile-catlibB) 'b)
    "2\n")
  (begin
    (mkfile "testfile-cof1A.ss"
      '(library (testfile-cof1A) (export a) (import (chezscheme))
         (define-syntax a (identifier-syntax 45))))
    (mkfile "testfile-cof1B.ss"
      '(library (testfile-cof1B) (export b) (import (chezscheme) (testfile-cof1A))
         (define b (lambda () (* a 2)))))
    (mkfile "testfile-cof1P.ss"
      '(import (chezscheme) (testfile-cof1A) (testfile-cof1B))
      '(printf "a = ~s, (b) = ~s\n" a (b)))
    (mkfile "testfile-cof1foo.ss"
      '(printf "hello from foo!\n"))
    (mkfile "testfile-cof1bar.ss"
      '(printf "hello from bar!\n"))
    (parameterize ([compile-imported-libraries #t]) (compile-program "testfile-cof1P"))
    (compile-file "testfile-cof1foo")
    (compile-file "testfile-cof1bar")
    (let ()
      (define fake-concatenate-object-files
        (lambda (outfn infn . infn*)
          (call-with-port (open-file-output-port outfn (file-options #;compressed replace))
            (lambda (op)
              (for-each
                (lambda (infn)
                  (put-bytevector op
                    (call-with-port (open-file-input-port infn (file-options #;compressed)) get-bytevector-all)))
                (cons infn infn*))))))
      (fake-concatenate-object-files "testfile-cof1fooP.so" "testfile-cof1foo.so" "testfile-cof1P.so")
      (fake-concatenate-object-files "testfile-cof1barB.so" "testfile-cof1bar.so" "testfile-cof1B.so"))
    #t)
  ; using separate-eval since A and B already loaded in the compiling process:
  (equal?
    (separate-eval '(load "testfile-cof1fooP.so"))
    "hello from foo!\na = 45, (b) = 90\n")
  (equal?
    (separate-eval
      '(load "testfile-cof1barB.so")
      '(printf "~s\n" (and (member '(testfile-cof1B) (library-list)) 'yes)))
    "hello from bar!\nyes\n")
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1barB.so")) "")
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1barB.so")) "")
  (delete-file "testfile-cof1A.so")
  ; NB: this should be an error, but isn't because we're using the fake concatenate-object-files
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1barB.so")) "") ; requires testfile-cof1A.so
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1barB.so")) "") ; doesn't require testfile-cof1A.so
  
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so
  (delete-file "testfile-cof1B.so")
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so or testfile-cof1B.so
  ; NB: this should be an error, but isn't because we're using the fake concatenate-object-files
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1fooP.so")) "") ; requires testfile-cof1B.so

  ; now with the real concatenate-object-files
  (begin
    (separate-eval '(parameterize ([compile-imported-libraries #t]) (compile-program "testfile-cof1P")))
    (concatenate-object-files "testfile-cof1fooP.so" "testfile-cof1foo.so" "testfile-cof1P.so")
    (concatenate-object-files "testfile-cof1barB.so" "testfile-cof1bar.so" "testfile-cof1B.so")
    #t)
  ; using separate-eval since A and B already loaded in the compiling process:
  (equal?
    (separate-eval '(load "testfile-cof1fooP.so"))
    "hello from foo!\na = 45, (b) = 90\n")
  (equal?
    (separate-eval
      '(load "testfile-cof1barB.so")
      '(printf "~s\n" (and (member '(testfile-cof1B) (library-list)) 'yes)))
    "hello from bar!\nyes\n")
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1barB.so")) "")
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1barB.so")) "")
  (delete-file "testfile-cof1A.so")
  (error? (separate-eval '(verify-loadability 'visit "testfile-cof1barB.so"))) ; requires testfile-cof1A.so
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1barB.so")) "") ; doesn't require testfile-cof1A.so
  
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so
  (equal? (separate-eval '(verify-loadability 'revisit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so
  (delete-file "testfile-cof1B.so")
  (equal? (separate-eval '(verify-loadability 'visit "testfile-cof1fooP.so")) "") ; doesn't require testfile-cof1A.so or testfile-cof1B.so
  (error? (separate-eval '(verify-loadability 'revisit "testfile-cof1fooP.so"))) ; requires testfile-cof1B.so
)

;;; section 7.2:

(mat top-level-value-functions
  (error? (top-level-bound? "hello"))
  (error? (top-level-bound?))
  (error? (top-level-bound? 45 'hello))
  (error? (top-level-bound? 'hello 'hello))
  (error? (top-level-bound? (scheme-environment) (scheme-environment)))
  (error? (top-level-mutable? "hello"))
  (error? (top-level-mutable?))
  (error? (top-level-mutable? 45 'hello))
  (error? (top-level-mutable? 'hello 'hello))
  (error? (top-level-mutable? (scheme-environment) (scheme-environment)))
  (error? (top-level-value "hello"))
  (error? (top-level-value))
  (error? (top-level-value 'hello 'hello))
  (error? (top-level-value (scheme-environment) (scheme-environment)))
  (error? (set-top-level-value! "hello" "hello"))
  (error? (set-top-level-value!))
  (error? (set-top-level-value! 15))
  (error? (set-top-level-value! 'hello 'hello 'hello))
  (error? (set-top-level-value! (scheme-environment) (scheme-environment) (scheme-environment)))
  (error? (define-top-level-value "hello" "hello"))
  (error? (define-top-level-value))
  (error? (define-top-level-value 15))
  (error? (define-top-level-value 'hello 'hello 'hello))
  (error? (define-top-level-value (scheme-environment) (scheme-environment) (scheme-environment)))

  (top-level-bound? 'cons (scheme-environment))
  (not (top-level-mutable? 'cons (scheme-environment)))
  (eq? (top-level-bound? 'probably-not-bound (scheme-environment)) #f)
  (equal? (top-level-value 'top-level-value) top-level-value)
  (equal?
    (parameterize ([interaction-environment
                    (copy-environment (scheme-environment) #t)])
      (eval '(define cons *))
      (eval
        '(list
           (cons 3 4)
           (fluid-let ([cons list])
             (list (cons 1 2)
                   ((top-level-value 'cons) 1 2)
                   ((top-level-value 'cons (scheme-environment)) 1 2)
                   (top-level-mutable? 'cons)
                   (top-level-mutable? 'cons (scheme-environment))
                   (top-level-mutable? 'car)
                   (top-level-mutable? 'car (scheme-environment)))))))
    '(12 ((1 2) (1 2) (1 . 2) #t #f #f #f)))
  (let ([abcde 4])
     (and (not (top-level-bound? 'abcde))
          (begin (define-top-level-value 'abcde 3)
                 (eqv? (top-level-value 'abcde) 3))
          (top-level-bound? 'abcde)
          (begin (set-top-level-value! 'abcde 9)
                 (eqv? (top-level-value 'abcde) 9))
          (eqv? abcde 4)))
  (eqv? abcde 9)
  (let ([x (gensym)])
     (and (not (top-level-bound? x))
          (begin (define-top-level-value x 'hi)
                 (eq? (top-level-value x) 'hi))
          (top-level-bound? x)
          (begin (set-top-level-value! x 'there)
                 (eq? (top-level-value x) 'there))
          (eq? (eval x) 'there)))
  (error? (top-level-value 'i-am-not-bound-i-hope))
  (error? (top-level-value 'let))
  (equal?
    (parameterize ([interaction-environment
                    (copy-environment (scheme-environment) #t)])
      (eval '(define cons (let () (import scheme) cons)))
      (eval
        '(fluid-let ([cons 'notcons])
           (list (top-level-value 'cons)
                 (parameterize ([optimize-level 0]) (eval 'cons))
                 (parameterize ([interaction-environment (scheme-environment)])
                   ((top-level-value 'cons) 3 4))))))
    '(notcons notcons (3 . 4)))
  (error? (set-top-level-value! 'let 45))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(define let 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(set! let 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (define-top-level-value 'let 45)))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (set-top-level-value! 'let 45)))
  (error? (define-top-level-value 'let 45 (scheme-environment)))
  (error? (set-top-level-value! 'let 45 (scheme-environment)))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(define cons 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(set! cons 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (define-top-level-value 'cons 45)))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (set-top-level-value! 'cons 45)))
  (error? (define-top-level-value 'cons 45 (scheme-environment)))
  (error? (set-top-level-value! 'cons 45 (scheme-environment)))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(define foo 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (eval '(set! foo 45) (scheme-environment))))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (define-top-level-value 'foo 45)))
  (error? (parameterize ([interaction-environment (scheme-environment)])
            (set-top-level-value! 'foo 45)))
  (error? (define-top-level-value 'foo 45 (scheme-environment)))
  (error? (set-top-level-value! 'foo 45 (scheme-environment)))
  (begin
    (define-syntax $let (identifier-syntax let))
    (equal?
      ($let ((x 3) (y 4)) (cons x y))
      '(3 . 4)))
  (eqv? (define-top-level-value '$let 76) (void))
  (eqv? (top-level-value '$let) 76)
  (eqv? $let 76)

 ; make sure implicit treatment of top-level identifiers as variables
 ; works when assignment occurs in loaded object file
  (equal?
    (begin
      (with-output-to-file "testfile.ss"
        (lambda () (pretty-print '(set! $fribblefratz 17)))
        'replace)
      (compile-file "testfile")
      (load "testfile.so")
      (list (top-level-bound? '$fribblefratz) (top-level-value '$fribblefratz)))
    '(#t 17))
  (eqv? $fribblefratz 17)
  (equal?
    (begin
      (with-output-to-file "testfile.ss"
        (lambda () (pretty-print '(set! $notfribblefratz -17)))
        'replace)
     ; compile in a separate Scheme process
      (if (windows?)
          (system (format "echo (compile-file \"testfile\") | ~a" (patch-exec-path *scheme*)))
          (system (format "echo '(compile-file \"testfile\")' | ~a" *scheme*)))
      (load "testfile.so")
      (list (top-level-bound? '$notfribblefratz) (top-level-value '$notfribblefratz)))
    '(#t -17))
  (eqv? $notfribblefratz -17)
 )

;;; section 7.3:

(mat new-cafe
   (procedure? new-cafe)
   (equal?
     (guard (c [else #f])
       (let ([ip (open-string-input-port "(+ 3 4)")])
         (let-values ([(op get) (open-string-output-port)])
           (parameterize ([console-input-port ip]
                          [console-output-port op]
                          [console-error-port op]
                          [#%$cafe 0]
                          [waiter-prompt-string "Huh?"])
             (new-cafe))
           (get))))
     "Huh? 7\nHuh? \n")
   (equal?
     (guard (c [else #f])
       (let ([ip (open-string-input-port "(if)")])
         (let-values ([(op get) (open-string-output-port)])
           (parameterize ([console-input-port ip]
                          [console-output-port op]
                          [console-error-port op]
                          [#%$cafe 0]
                          [waiter-prompt-string "Huh?"])
             (new-cafe))
           (get))))
     "Huh? \nException: invalid syntax (if)\nHuh? \n")
   (equal?
     (separate-eval
       `(let ([ip (open-string-input-port "
                    (base-exception-handler
                      (lambda (c)
                        (fprintf (console-output-port) \"~%>>> \")
                        (display-condition c (console-output-port))
                        (fprintf (console-output-port) \" <<<~%\")
                        (reset)))
                    (if)")])
          (let-values ([(op get) (open-string-output-port)])
            (parameterize ([console-input-port ip]
                           [console-output-port op]
                           [console-error-port op]
                           [#%$cafe 0]
                           [waiter-prompt-string "Huh?"])
              (new-cafe))
            (get))))
     "\"Huh? Huh? \\n>>> Exception: invalid syntax (if) <<<\\nHuh? \\n\"\n")
 )

(mat reset
  (procedure? (reset-handler))
  (eqv?
    (call/cc
      (lambda (k)
        (parameterize ([reset-handler (lambda () (k 17))])
          (reset))))
    17)
  (error? ; unexpected return from handler
    (guard (c [else (raise-continuable c)])
      (parameterize ([reset-handler values])
        (reset))))
 )

(mat exit
  (procedure? (exit-handler))
  (eqv?
    (call/cc
      (lambda (k)
        (parameterize ([exit-handler (lambda () (k 17))])
          (exit))))
    17)
  (eqv?
    (call/cc
      (lambda (k)
        (parameterize ([exit-handler (lambda (q) (k 17))])
          (exit -1))))
    17)
  (error? ; unexpected return from handler
    (parameterize ([exit-handler values])
      (exit)))
  (error? ; unexpected return from handler
    (parameterize ([exit-handler values])
      (exit 5)))
  (begin
    (define (exit-code expr)
      (if (windows?)
          (system (format "echo ~s | ~a -q" expr (patch-exec-path *scheme*)))
          (system (format "echo '~s' | ~a -q" expr *scheme*))))
    #t)
  (eqv? (exit-code '(exit)) 0)
  (eqv? (exit-code '(exit 15)) 15)
  (eqv? (exit-code '(exit 0)) 0)
  (eqv? (exit-code '(exit 24 7)) 24)
  (eqv? (exit-code '(exit 0 1 2)) 0)
  (eqv? (exit-code '(exit 3.14)) 1)
  (eqv? (exit-code '(exit 9.8 3.14)) 1)
  (begin
    (with-output-to-file "testfile-exit.ss"
      (lambda ()
        (for-each pretty-print
          '((import (scheme))
            (apply exit (map string->number (command-line-arguments))))))
      'replace)
    #t)
  (eqv? (system (format "~a --script testfile-exit.ss" (patch-exec-path *scheme*))) 0)
  (eqv? (system (format "~a --script testfile-exit.ss 5" (patch-exec-path *scheme*))) 5)
  (eqv? (system (format "~a --script testfile-exit.ss 0 1 2" (patch-exec-path *scheme*))) 0)
  (eqv? (system (format "~a --script testfile-exit.ss 3 4 5" (patch-exec-path *scheme*))) 3)
  (eqv? (system (format "~a --program testfile-exit.ss" (patch-exec-path *scheme*))) 0)
  (eqv? (system (format "~a --program testfile-exit.ss 2" (patch-exec-path *scheme*))) 2)
  (eqv? (system (format "~a --program testfile-exit.ss 0 1 2" (patch-exec-path *scheme*))) 0)
  (eqv? (system (format "~a --program testfile-exit.ss 6 7 8" (patch-exec-path *scheme*))) 6)
 )

(mat abort
  (procedure? (abort-handler))
  (eqv?
    (call/cc
      (lambda (k)
        (parameterize ([abort-handler (lambda () (k 17))])
          (abort))))
    17)
  (error? ; unexpected return from handler
    (parameterize ([abort-handler values])
      (abort)))
 )

(mat command-line
  (equal? (command-line) '(""))
  (equal? (r6rs:command-line) (command-line))
  (parameterize ([command-line '("cp" "x" "y")])
    (and (equal? (command-line) '("cp" "x" "y"))
         (equal? (r6rs:command-line) '("cp" "x" "y"))))
)

(mat command-line-arguments
  (null? (command-line-arguments))
  (parameterize ([command-line-arguments '("x" "y")])
    (equal? (command-line-arguments) '("x" "y")))
)

;;; section 7.4:

(mat transcript-on/transcript-off ; check output
  (begin
    (delete-file "testscript")
    (printf "***** expect transcript output:~%")
    (parameterize ([console-input-port (open-input-string "(transcript-off)\n")])
      (transcript-on "testscript")
      (let repl ()
        (display "OK, " (console-output-port))
        (let ([x (read (console-input-port))])
          (unless (eof-object? x)
            (let ([x (eval x)])
              (pretty-print x (console-output-port)))
            (repl)))))
    (not (eof-object? (with-input-from-file "testscript" read-char))))
 )

;;; section 7.5:

(mat collect
  (error? ; invalid generation
    (collect-maximum-generation -1))
  (error? ; invalid generation
    (collect-maximum-generation 10000))
  (error? ; invalid generation
    (collect-maximum-generation 'static))
  (error? ; invalid generation
    (release-minimum-generation -1))
  (error? ; invalid generation
    (release-minimum-generation (+ (collect-maximum-generation) 1)))
  (error? ; invalid generation
    (release-minimum-generation 'static))
  (let ([g (+ (collect-maximum-generation) 1)])
    (guard (c [(and (message-condition? c)
                    (equal? (condition-message c) "invalid generation ~s")
                    (irritants-condition? c)
                    (equal? (condition-irritants c) (list g)))])
      (collect g)
      #f))
  (let ([g (+ (collect-maximum-generation) 1)])
    (guard (c [(and (message-condition? c)
                    (equal? (condition-message c) "invalid target generation ~s for generation ~s")
                    (irritants-condition? c)
                    (equal? (condition-irritants c) (list g 0)))])
      (collect 0 g)
      #f))
  (error? (collect 0 -1))
  (error? (collect -1 0))
  (error? (collect 1 0))
  (error? (collect 'static))
  (with-interrupts-disabled
    (collect (collect-maximum-generation))
    (let ([b1 (bytes-allocated)])
      (let loop ([n 1000] [x '()])
        (or (= n 0) (loop (- n 1) (cons x x))))
      (let ([b2 (bytes-allocated)])
        (collect (collect-maximum-generation))
        (let ([b3 (bytes-allocated)])
          (and (> b2 b1) (< b3 b2))))))
  (error? ; invalid generation
    (collect 'static 1 'static))
  (error? ; invalid generation
    (collect 'static 1 'static))
  (error? ; invalid generation
    (parameterize ([collect-maximum-generation 4])
      (collect 17 1 17)))
  (error? ; invalid generation
    (collect -1 1 'static))
  (error? ; invalid maximum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 3 1 2)))
  (error? ; invalid maximum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 3 1 'dynamic)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 0 0 3)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 0 'static 3)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 0 2 1)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect 0 2 0)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect (collect-maximum-generation) 0 'static)))
  (error? ; invalid minimum target generation
    (parameterize ([collect-maximum-generation 4])
      (collect (collect-maximum-generation) -1 'static)))
  (parameterize ([collect-maximum-generation (max (collect-maximum-generation) 2)])
    (with-interrupts-disabled
      (collect (collect-maximum-generation))
      (let ([b0-0 (bytes-allocated 0)]
            [b1-0 (bytes-allocated 1)]
            [bm-0 (bytes-allocated (collect-maximum-generation))])
        (let* ([v (make-vector 20000)] [n (compute-size v)])
          (let ([b0-1 (bytes-allocated 0)]
                [b1-1 (bytes-allocated 1)]
                [bm-1 (bytes-allocated (collect-maximum-generation))])
            (unless (>= (- b0-1 b0-0) n) (errorf 'oops1 "b0-0 = ~s, b0-1 = ~s, b0-2 = ~s" b0-0 b0-1 b0-2))
            (unless (< (- b1-1 b1-0) n) (errorf 'oops2 "b1-0 = ~s, b1-1 = ~s, b1-2 = ~s" b1-0 b1-1 b1-2))
            (unless (< (- bm-1 bm-0) n) (errorf 'oops3 "bm-0 = ~s, bm-1 = ~s, bm-2 = ~s" bm-0 bm-1 bm-2))
            (collect (collect-maximum-generation) 1 (collect-maximum-generation))
            (let ([b0-2 (bytes-allocated 0)]
                  [b1-2 (bytes-allocated 1)]
                  [bm-2 (bytes-allocated (collect-maximum-generation))])
              (unless (< (- b0-2 b0-0) n) (errorf 'oops4 "b0-0 = ~s, b0-1 = ~s, b0-2 = ~s" b0-0 b0-1 b0-2))
              (unless (>= (- b1-2 b1-0) n) (errorf 'oops5 "b1-0 = ~s, b1-1 = ~s, b1-2 = ~s" b1-0 b1-1 b1-2))
              (unless (< (- bm-2 bm-0) n) (errorf 'oops6 "bm-0 = ~s, bm-1 = ~s, bm-2 = ~s" bm-0 bm-1 bm-2))
              (parameterize ([print-vector-length #t]) (pretty-print v))
              #t))))))
  (parameterize ([collect-maximum-generation 4]
                 [collect-generation-radix 2]
                 [collect-trip-bytes (expt 2 20)]
                 [collect-maximum-generation-threshold-factor 0])
    (collect (collect-maximum-generation))
    (let ([b0 (maximum-memory-bytes)])
      (define tail-spin
        (lambda (n)
          (do ([i 1 (fx+ i 1)] [next (cons 0 '()) (cdr next)])
              ((fx= i n))
            (set-cdr! next (cons i '())))))
      (tail-spin 50000000)
      (let ([b1 (maximum-memory-bytes)])
        (or (< (- b1 b0) (expt 2 24))
            (errorf #f "b0 = ~s, b1 = ~s, b1-b0 = ~s" b0 b1 (- b1 b0))))))
  (begin
    (define try-gc-threshold
      (lambda (gc-max?)
        (let* ([v (gensym)]
               [w (weak-cons v '())])
          (parameterize ([collect-maximum-generation 4]
                         [collect-generation-radix 4]
                         [collect-trip-bytes (expt 2 20)]
                         [collect-maximum-generation-threshold-factor (if gc-max? 0 1000)])
            (collect (collect-maximum-generation))
            (set! v #f)
            (let ()
              (define tail-spin
                (lambda (n)
                  (do ([i 1 (fx+ i 1)] [next (cons 0 '()) (cdr next)])
                      ((fx= i n))
                    (set-cdr! next (cons i '())))))
              (tail-spin 50000000)
              (if gc-max?
                  (eq? #!bwp (car w))
                  (symbol? (car w))))))))
    #t)
  (try-gc-threshold #t)
  (try-gc-threshold #f)
 )

(mat object-counts
  ; basic structural checks
  (let ([hc (object-counts)])
    (begin
      (assert (list? hc))
      (for-each (lambda (a) (assert (pair? a))) hc)
      (for-each (lambda (a) (assert (or (symbol? (car a)) (record-type-descriptor? (car a))))) hc)
      (for-each (lambda (a) (assert (list? (cdr a)))) hc)
      (for-each
        (lambda (a)
          (for-each
            (lambda (a)
              (and (or (and (fixnum? (car a)) (<= 0 (car a) (collect-maximum-generation)))
                       (eq? (car a) 'static))
                   (and (fixnum? (cadr a)) (>= (cadr a) 0))
                   (and (fixnum? (cddr a)) (>= (cddr a) (cadr a)))))
            (cdr a)))
        hc)
      (assert (assq 'pair hc))
      (assert (assq 'procedure hc))
      (assert (assq 'symbol hc))
      (assert (assp record-type-descriptor? hc))
      #t))
  ; a few idiot checks including verification of proper behavior when changing collect-maximum-generation
  (parameterize ([enable-object-counts #t] [collect-maximum-generation (collect-maximum-generation)])
    (pair?
      (with-interrupts-disabled
        (let ([cmg (collect-maximum-generation)])
          (collect-maximum-generation 3)
          (collect 3 3)
          (let ()
            (define (locate type gen ls)
              (cond
                [(assq type ls) =>
                 (lambda (a)
                   (cond
                     [(assv gen (cdr a)) => cadr]
                     [else #f]))]
                [else #f]))
            (define-record-type flub (fields x))
            (define q0 (make-flub 0))
            (define b0 (box 0))
            (collect 0 0)
            (let ([hc (object-counts)])
              (assert (locate 'box 0 hc))
              (assert (locate (record-type-descriptor flub) 0 hc))
              (collect-maximum-generation 6)
              (let ([hc (object-counts)])
                (assert (locate 'box 0 hc))
                (assert (locate (record-type-descriptor flub) 0 hc))
                (collect 6 6)
                (let ()
                  (define q1 (make-flub q0))
                  (define b1 (box b0))
                  (collect 5 5)
                  (let ()
                    (define q2 (make-flub q1))
                    (define b2 (box b1))
                    (collect 4 4)
                    (let ([hc (object-counts)])
                      (assert (locate 'box 4 hc))
                      (assert (locate 'box 5 hc))
                      (assert (locate 'box 6 hc))
                      (assert (locate (record-type-descriptor flub) 4 hc))
                      (assert (locate (record-type-descriptor flub) 5 hc))
                      (assert (locate (record-type-descriptor flub) 6 hc))
                      (collect-maximum-generation 4)
                      (let ([hc (object-counts)])
                        (assert (locate 'box 4 hc))
                        (assert (not (locate 'box 5 hc)))
                        (assert (not (locate 'box 6 hc)))
                        (assert (locate (record-type-descriptor flub) 4 hc))
                        (assert (not (locate (record-type-descriptor flub) 5 hc)))
                        (assert (not (locate (record-type-descriptor flub) 6 hc)))
                        (collect 4 4)
                        (let ([hc (object-counts)])
                          (assert (locate 'box 4 hc))
                          (assert (not (locate 'box 5 hc)))
                          (assert (not (locate 'box 6 hc)))
                          (assert (locate (record-type-descriptor flub) 4 hc))
                          (assert (not (locate (record-type-descriptor flub) 5 hc)))
                          (assert (not (locate (record-type-descriptor flub) 6 hc)))
                          (collect-maximum-generation cmg)
                          (collect cmg cmg)
                          (cons q2 b2)))))))))))))
  ; make sure we can handle turning enable-object-counts on and off
  (equal?
    (parameterize ([collect-request-handler void])
      (define-record-type frob (fields x))
      (define x (list (make-frob 3)))
      (parameterize ([enable-object-counts #t]) (collect 0 0))
      (parameterize ([enable-object-counts #f]) (collect 0 1))
      (do ([n 100000 (fx- n 1)])
        ((fx= n 0))
        (set! x (cons n x)))
      (parameterize ([enable-object-counts #t]) (collect 1 1))
      (cons (length x) (cadr (assq 1 (cdr (assq (record-type-descriptor frob) (object-counts)))))))
    `(100001 . 1))
  (let ([a (assq 'reloc-table (object-counts))])
    (or (not a) (not (assq 'static (cdr a)))))
)

(mat object-references
  (begin
    (define variable-whose-value-is-a-gensym (gensym))
    (define guardian-to-hold-gensyms (make-guardian))
    ;; works on tree-shaped objects, except that
    ;; weak/ephemeron pairs can create DAGs; if a weak pair has
    ;; a non-#!bwp in the `car`, it must be referenced
    ;; by a box or by `guardian-to-hold-gensyms`
    (define (check-references obj)
      (let ([backrefs (make-eq-hashtable)]
            [old-collect (collect-request-handler)])
        (enable-object-backreferences #t)
        (collect-request-handler void)
        (collect (collect-maximum-generation))
        (for-each (lambda (brs)
                    (for-each (lambda (br)
                                (hashtable-set! backrefs (car br) (cdr br)))
                              brs))
                  (object-backreferences))
        (enable-object-backreferences #f)
        (collect-request-handler old-collect)
        (and
         ;; Check the given object
         (let loop ([obj obj] [parent #f])
           (and (or (not parent)
                    (null? obj)
                    (boolean? obj)
                    (eq? parent (hashtable-ref backrefs obj #f)))
                (cond
                 [(pair? obj)
                  (and (cond
                        [(weak-pair? obj)
                         (let ([a (car obj)])
                           (or (eq? a #!bwp)
                               (let ([p (hashtable-ref backrefs a #f)])
                                 (or (box? p)
                                     ;; retained by `guardian-to-hold-gensyms`
                                     ;; means retains by it's tconc
                                     (and (pair? p)
                                          (eq? guardian-to-hold-gensyms
                                               (hashtable-ref backrefs p #f)))))))]
                        [(ephemeron-pair? obj) #t]
                        [else
                         (loop (car obj) obj)])
                       (loop (cdr obj) obj))]
                 [(vector? obj)
                  (let vloop ([i 0])
                    (or (= i (vector-length obj))
                        (and (loop (vector-ref obj i) obj)
                             (vloop (add1 i)))))]
                 [(box? obj)
                  (loop (unbox obj) obj)]
                 [(procedure? obj)
                  (let ([insp (inspect/object obj)])
                    (let ploop ([i 0])
                      (or (= i (insp 'length))
                          (and (loop (((insp 'ref i) 'ref) 'value) obj)
                               (ploop (add1 i))))))]
                 [else #t])))
         ;; Check a symbol binding
         (let ([var (hashtable-ref backrefs variable-whose-value-is-a-gensym #f)])
           (and (eq? 'symbol ((inspect/object var) 'type))
                (equal? "variable-whose-value-is-a-gensym"
                        (((inspect/object var) 'name) 'value)))))))
    #t)
  (check-references (list (gensym)
                          (vector (gensym) (box (cons (gensym) (gensym))) (gensym))
                          (if (eq? (current-eval) interpret)
                              'skip ; closures in interpreter don't capture only free variables
                              (let ([v (gensym)])
                                (lambda ()
                                  v)))
                          ;; make sure `weak-cons` doesn't retain
                          (weak-cons (gensym) #f)
                          (let ([v (gensym)])
                            ;; weak pair won't count as retaining reference
                            (weak-cons v
                                       ;; containing box will count
                                       (box v)))
                          (let ([v (gensym)])
                            (guardian-to-hold-gensyms v)
                            (weak-cons v #f))
                          (let ([v (gensym)])
                            (list v (ephemeron-cons v (gensym))))))
)

(mat collect-rendezvous
  (begin
    (define (check-working-gc collect)
      (with-interrupts-disabled
       (let ([p (weak-cons (gensym) #f)])
         (collect)
         (eq? (car p) #!bwp))))
    (and (check-working-gc collect)
         (check-working-gc collect-rendezvous)))
      
  (or (not (threaded?))
      (let ([m (make-mutex)]
            [c (make-condition)]
            [done? #f])
        (fork-thread
         (lambda ()
           (let loop ()
             (mutex-acquire m)
             (cond
              [done?
               (condition-signal c)
               (mutex-release m)]
              [else
               (mutex-release m)
               (loop)]))))
        (and (check-working-gc collect-rendezvous)
             ;; End thread:
             (begin
               (mutex-acquire m)
               (set! done? #t)
               (condition-wait c m)
               (mutex-release m)
               ;; Make sure the thread is really done
               (let loop ()
                 (unless (= 1 (#%$top-level-value '$active-threads))
                   (loop)))
               ;; Plain `collect` should work again:
               (check-working-gc collect)))))
  )

;;; section 7.6:

(mat time
   (begin (printf "***** expect time output (nonzero allocation):~%")
          (time (let loop ([n 1000] [x '()])
                   (or (= n 0) (loop (- n 1) (cons x x))))))
   (begin (printf "***** expect time output (nonzero cpu & real time):~%")
          (time (letrec ([tak (lambda (x y z)
                                 (if (>= y x)
                                     z
                                     (tak (tak (1- x) y z)
                                          (tak (1- y) z x)
                                          (tak (1- z) x y))))])
                   (tak 18 12 6)))
          #t)
   (begin (printf "***** expect time output (>= 2 collections):~%")
          (time (begin (collect) (collect)))
          #t)
 )

(mat sstats
  (begin 
    (define exact-integer? 
      (lambda (x) 
        (and (exact? x) (integer? x))))
    (define exact-nonnegative-integer? 
      (lambda (x) 
        (and (exact-integer? x) (nonnegative? x))))
    (define sstats-time?
      (lambda (t type)
        (and (time? t) (eq? (time-type t) type))))
    #t)
  (error? ; invalid cpu time
    (make-sstats 0 (make-time 'time-monotonic 0 0) 0 0 (make-time 'time-collector-cpu 0 0) (make-time 'time-collector-real 0 0) 0))
  (error? ; invalid real time
    (make-sstats (make-time 'time-duration 0 0) 0 0 0 (make-time 'time-collector-cpu 0 0) (make-time 'time-collector-real 0 0) 0))
  (error? ; invalid bytes
    (make-sstats (make-time 'time-duration 0 0) (make-time 'time-monotonic 0 0) 0.0 0 (make-time 'time-collector-cpu 0 0) (make-time 'time-collector-real 0 0) 0))
  (error? ; invalid gc-count
    (make-sstats (make-time 'time-duration 0 0) (make-time 'time-monotonic 0 0) 0 "oops" (make-time 'time-collector-cpu 0 0) (make-time 'time-collector-real 0 0) 0))
  (error? ; invalid gc-cpu
    (make-sstats (make-time 'time-duration 0 0) (make-time 'time-monotonic 0 0) 0 0 0 (make-time 'time-collector-real 0 0) 0))
  (error? ; invalid gc-real
    (make-sstats (make-time 'time-duration 0 0) (make-time 'time-monotonic 0 0) 0 0 (make-time 'time-collector-cpu 0 0) 0 0))
  (error? ; invalid gc-bytes
    (make-sstats (make-time 'time-duration 0 0) (make-time 'time-monotonic 0 0) 0 0 (make-time 'time-collector-cpu 0 0) (make-time 'time-collector-real 0 0) 0.0))
  (begin
    (define sstats
      (make-sstats
        (make-time 'time-process 0 0)
        (make-time 'time-monotonic 0 0)
        0
        0
        (make-time 'time-collector-cpu 0 0)
        (make-time 'time-collector-real 0 0)
        0))
    #t)
  (sstats? sstats)
  (error? ; not an sstats record
    (sstats-cpu 'it))
  (error? ; not an sstats record
    (sstats-real 'is))
  (error? ; not an sstats record
    (sstats-bytes 'fun))
  (error? ; not an sstats record
    (sstats-gc-count 'to))
  (error? ; not an sstats record
    (sstats-gc-cpu 'write))
  (error? ; not an sstats record
    (sstats-gc-real 'mats))
  (error? ; not an sstats record
    (sstats-gc-bytes '(not really)))
  (sstats-time? (sstats-cpu sstats) 'time-process)
  (sstats-time? (sstats-real sstats) 'time-monotonic)
  (eqv? (sstats-bytes sstats) 0)
  (eqv? (sstats-gc-count sstats) 0)
  (sstats-time? (sstats-gc-cpu sstats) 'time-collector-cpu)
  (sstats-time? (sstats-gc-real sstats) 'time-collector-real)
  (eqv? (sstats-gc-bytes sstats) 0)

  (error? ; not an sstats record
    (set-sstats-cpu! 'it (make-time 'time-duration 1 0)))
  (error? ; not an sstats record
    (set-sstats-real! 'is (make-time 'time-duration 1 0)))
  (error? ; not an sstats record
    (set-sstats-bytes! 'fun 11))
  (error? ; not an sstats record
    (set-sstats-gc-count! 'to 13))
  (error? ; not an sstats record
    (set-sstats-gc-cpu! 'write (make-time 'time-duration 1 0)))
  (error? ; not an sstats record
    (set-sstats-gc-real! 'mats (make-time 'time-duration 1 0)))
  (error? ; not an sstats record
    (set-sstats-gc-bytes! '(not really) 17))
  (error? ; 12 is not a time
    (set-sstats-cpu! sstats 12))
  (error? ; 12 is not a time
    (set-sstats-real! sstats 12))
  (error? ; 12 is not a time
    (set-sstats-gc-cpu! sstats 12))
  (error? ; 12 is not a time
    (set-sstats-gc-real! sstats 12))
  (error? ; #[time whatsit] is not a time
    (set-sstats-gc-real! sstats (make-assertion-violation)))
  (begin
    (set-sstats-cpu! sstats (make-time 'time-utc 12 3))
    (set-sstats-cpu! sstats (make-time 'time-monotonic 12 3))
    (set-sstats-cpu! sstats (make-time 'time-duration 12 3))
    (set-sstats-cpu! sstats (make-time 'time-thread 12 3))
    (set-sstats-cpu! sstats (make-time 'time-collector-cpu 12 3))
    (set-sstats-cpu! sstats (make-time 'time-collector-real 12 3))
    (set-sstats-real! sstats (make-time 'time-utc 12 3))
    (set-sstats-real! sstats (make-time 'time-duration 12 3))
    (set-sstats-real! sstats (make-time 'time-process 12 3))
    (set-sstats-real! sstats (make-time 'time-thread 12 3))
    (set-sstats-real! sstats (make-time 'time-collector-cpu 12 3))
    (set-sstats-real! sstats (make-time 'time-collector-real 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-utc 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-monotonic 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-duration 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-process 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-thread 12 3))
    (set-sstats-gc-cpu! sstats (make-time 'time-collector-real 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-utc 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-monotonic 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-duration 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-process 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-thread 12 3))
    (set-sstats-gc-real! sstats (make-time 'time-collector-cpu 12 3))
    #t)
  (eq? (set-sstats-cpu! sstats (make-time 'time-process 12 3)) (void))
  (eq? (set-sstats-real! sstats (make-time 'time-monotonic 12 3)) (void))
  (eq? (set-sstats-gc-cpu! sstats (make-time 'time-collector-cpu 12 3)) (void))
  (eq? (set-sstats-gc-real! sstats (make-time 'time-collector-real 12 3)) (void))

  (error? (set-sstats-bytes! sstats 12.3))
  (error? (set-sstats-bytes! sstats 12.0))
  (error? (set-sstats-gc-count! sstats 3+4i))
  (error? (set-sstats-gc-count! sstats #f))
  (error? (set-sstats-gc-bytes! sstats 8/3))
  (error? (set-sstats-gc-bytes! sstats 'twelve))
  (eq? (set-sstats-bytes! sstats 12) (void))
  (eq? (set-sstats-gc-count! sstats 3) (void))
  (eq? (set-sstats-gc-bytes! sstats 8) (void))

  (begin
    (define sstats-diff
      (sstats-difference
        (make-sstats
          (make-time 'time-process 83 5)
          (make-time 'time-monotonic 12 1)
          5
          23
          (make-time 'time-collector-cpu (expt 2 8) 0)
          (make-time 'time-collector-real 735 1000007)
          29)
        (make-sstats
          (make-time 'time-process 3 0)
          (make-time 'time-monotonic 10333221 2)
          20
          3
          (make-time 'time-collector-cpu 0 0)
          (make-time 'time-collector-real 0 0)
          4)))
    #t)
  (sstats? sstats-diff)
  (sstats-time? (sstats-cpu sstats-diff) 'time-duration)
  (time=? (sstats-cpu sstats-diff) (make-time 'time-duration 80 5))
  (sstats-time? (sstats-real sstats-diff) 'time-duration)
  (time=? (sstats-real sstats-diff) (make-time 'time-duration 989666791 -2))
  (eqv? (sstats-bytes sstats-diff) -15)
  (eqv? (sstats-gc-count sstats-diff) 20)
  (sstats-time? (sstats-gc-cpu sstats-diff) 'time-duration)
  (time=? (sstats-gc-cpu sstats-diff) (make-time 'time-duration (expt 2 8) 0))
  (sstats-time? (sstats-gc-real sstats-diff) 'time-duration)
  (time=? (sstats-gc-real sstats-diff) (make-time 'time-duration 735 1000007))
  (eqv? (sstats-gc-bytes sstats-diff) 25)

  (let ([sstats (statistics)])
    (and
      (sstats? sstats)
      (sstats-time? (sstats-cpu sstats) 'time-thread)
      (sstats-time? (sstats-real sstats) 'time-monotonic)
      (exact-nonnegative-integer? (sstats-bytes sstats))
      (exact-nonnegative-integer? (sstats-gc-count sstats))
      (sstats-time? (sstats-gc-cpu sstats) 'time-collector-cpu)
      (sstats-time? (sstats-gc-real sstats) 'time-collector-real)
      (exact-nonnegative-integer? (sstats-gc-bytes sstats))))

  (let ([sstats (sstats-difference (statistics) (statistics))])
    (and
      (sstats? sstats)
      (sstats-time? (sstats-cpu sstats) 'time-duration)
      (sstats-time? (sstats-real sstats) 'time-duration)
      (exact-integer? (sstats-bytes sstats))
      (exact-integer? (sstats-gc-count sstats))
      (sstats-time? (sstats-gc-cpu sstats) 'time-duration)
      (sstats-time? (sstats-gc-real sstats) 'time-duration)
      (exact-integer? (sstats-gc-bytes sstats))))
 )

(mat display-statistics ; check output
  (let ([s (with-output-to-string display-statistics)])
    (and (string? s) (> (string-length s) 50)))
 )

(mat cpu-time
   (> (cpu-time) 0)
   (let ([x (cpu-time)])
      (<= x (cpu-time)))
 )

(mat real-time
   (> (real-time) 0)
   (let ([x (real-time)])
      (<= x (real-time)))
 )

(mat bytes-allocated
   (error? (bytes-allocated 'yuk))
   (error? (bytes-allocated -1))
   (error? (bytes-allocated (+ (collect-maximum-generation) 1)))
   (error? (bytes-allocated (+ (most-positive-fixnum) 1)))
   (error? (bytes-allocated #f))
   (error? (bytes-allocated (+ (collect-maximum-generation) 1) 'new))
   (error? (bytes-allocated (+ (collect-maximum-generation) 1) #f))
   (error? (bytes-allocated 0 'gnu))
   (error? (bytes-allocated #f 'gnu))
   (error? (bytes-allocated 'static 'gnu))
   (> (bytes-allocated) 0)
   (andmap (lambda (g) (>= (bytes-allocated g) 0)) (iota (+ (collect-maximum-generation) 1)))
   (>= (bytes-allocated 'static) 0)
   (let ([x (bytes-allocated)])
      (<= x (bytes-allocated)))
   (>= (initial-bytes-allocated) 0)
   (>= (collections) 0)
   (>= (bytes-deallocated) 0)
   (let ([b (bytes-deallocated)] [c (collections)])
     (with-interrupts-disabled ; ensure allocated list stays in generation 0 until printed
      (let ([x (make-list 10 'a)])
        (pretty-print x))
      (collect))
     (and (> (collections) c) (or (eq? (current-eval) interpret)
                                  (> (bytes-deallocated) b))))
   (>= (bytes-allocated #f #f) 0)
   (andmap (lambda (space)
          (>= (bytes-allocated #f space) 0))
     (#%$spaces))
   (let ()
     (define fudge (if (or (positive? (heap-check-interval)) (> (collect-maximum-generation) 4)) 4000 2000))
     (define ~=
       (lambda (x y)
         (<= (abs (- x y)) fudge)))
     (define all-gen 
       (append (iota (+ (collect-maximum-generation) 1)) '(static)))
     (for-each
       (lambda (space)
         (critical-section
           (let ([n1 (bytes-allocated #f space)]
                 [n2 (fold-left (lambda (bytes gen)
                                  (+ bytes (bytes-allocated gen space)))
                       0
                       all-gen)])
             (unless (~= n1 n2)
               (errorf #f "discrepancy for space ~s: ~d vs ~d" space n1 n2)))))
       (#%$spaces))
     (for-each
       (lambda (gen)
         (critical-section
           (let ([n1 (bytes-allocated gen #f)]
                 [n2 (fold-left (lambda (bytes space)
                                  (+ bytes (bytes-allocated gen space)))
                       0
                       (#%$spaces))])
             (unless (~= n1 n2)
               (errorf #f "discrepancy for generation ~s: ~d vs ~d" gen n1 n2)))))
       all-gen)
     (critical-section
       (let ([n1 (bytes-allocated #f #f)]
             [n2 (fold-left (lambda (bytes gen)
                              (fold-left (lambda (bytes space)
                                           (+ bytes (bytes-allocated gen space)))
                                bytes
                                (#%$spaces)))
                   0
                   all-gen)])
         (unless (~= n1 n2)
           (errorf #f "discrepancy in bytes-allocated: ~d vs ~d" n1 n2))))
     #t)
 )

(mat memory-bytes
  (critical-section
    (let ([x (maximum-memory-bytes)])
      (<= (current-memory-bytes) x)))
  (critical-section
    (let ([x (maximum-memory-bytes)])
      (reset-maximum-memory-bytes!)
      (let ([y (maximum-memory-bytes)])
        (<= y x))))
)

(mat date-and-time
   (let ([s (date-and-time)])
      (printf "***** check date-and-time: ~s~%" s)
      (string? s))
 )

;;; section 7-7:

(mat trace-lambda ; check output
   (letrec ([fact (trace-lambda fact (x)
                     (if (= x 0)
                         1
                         (* x (fact (- x 1)))))])
      (printf "***** expect trace of (fact 3):~%")
      (eqv? (fact 3) 6))
 )

(mat trace-let ; check output
   (begin (printf "***** expect trace of (fib 3):~%")
          (eqv? (trace-let fib ([x 3])
                   (if (< x 2)
                       1
                       (+ (fib (- x 1)) (fib (- x 2)))))
                3))
 )

(mat trace/untrace
   (begin (set! lslen
             (lambda (ls)
                (if (null? ls)
                    0
                    (+ (lslen (cdr ls)) 1))))
          (and (equal? (trace lslen) '(lslen))
               (equal? (trace) '(lslen))
               (begin (printf "***** expect trace of (lslen '(a b c)):~%")
                      (eqv? (lslen '(a b c)) 3))
               (equal? (untrace lslen) '(lslen))
               (equal? (trace) '())
               (equal? (trace lslen) '(lslen))
               (equal? (trace lslen) '(lslen))
               (begin (set! lslen (lambda (x) x))
                      (printf "***** do *not* expect output:~%")
                      (eqv? (lslen 'a) 'a))
               (equal? (trace lslen) '(lslen))
               (begin (printf "***** expect trace of (lslen 'a):~%")
                      (eqv? (lslen 'a) 'a))
               (equal? (untrace) '(lslen))
               (equal? (trace) '())
               (begin (printf "***** do *not* expect output:~%")
                      (eqv? (lslen 'a) 'a))))
 )

;;; section 7-8:

(mat error
   (error? (errorf 'a "hit me!"))
   (error? (let f ([n 10]) (if (= n 0) (errorf 'f "n is ~s" n) (f (- n 1)))))
 )

(mat keyboard-interrupt-handler ; must be tested by hand
   (procedure? (keyboard-interrupt-handler))
 )

(mat collect-request-handler
   (procedure? (collect-request-handler))
   (call/cc
      (lambda (k)
         (parameterize ([collect-request-handler
                         (lambda ()
                            (collect)
                            (k #t))])
            (let f ([x '()]) (f (list-copy (cons 'a x)))))))
 )

(mat timer-interrupt-handler ; tested in mat set-timer below
   (procedure? (timer-interrupt-handler))
 )


;;; section 7-9:

(mat set-timer
   (let ([count1 0])
      (timer-interrupt-handler (lambda () (set! count1 (+ count1 1))))
      (set-timer (+ 10 (random 10)))
      (let loop2 ([count2 1])
         (cond
            [(= count2 100)]
            [(= count1 count2)
             (set-timer (+ 10 (random 10)))
             (loop2 (+ count2 1))]
            [else (loop2 count2)])))
 )

(mat disable-interrupts-enable-interrupts
   (and (= (disable-interrupts) 1)
        (= (disable-interrupts) 2)
        (= (enable-interrupts) 1)
        (= (enable-interrupts) 0))
   (call/cc
      (lambda (k)
         (timer-interrupt-handler (lambda () (k #t)))
         (disable-interrupts)
         (parameterize ([timer-interrupt-handler (lambda () (k #f))])
           (set-timer 1)
           (let loop ([n 1000]) (or (= n 0) (loop (- n 1)))))
         (enable-interrupts)
         (let loop ([n 1000]) (or (= n 0) (loop (- n 1))))
         #f))
 )

(mat with-interrupts-disabled
   (call/cc
      (lambda (k)
         (timer-interrupt-handler (lambda () (k #t)))
         (with-interrupts-disabled
            (parameterize ([timer-interrupt-handler (lambda () (k #f))])
              (set-timer 1)
              (let loop ([n 1000]) (or (= n 0) (loop (- n 1))))))
         (let loop ([n 1000]) (or (= n 0) (loop (- n 1))))
         #f))
  ; test old name
   (call/cc
      (lambda (k)
         (timer-interrupt-handler (lambda () (k #t)))
         (critical-section
            (parameterize ([timer-interrupt-handler (lambda () (k #f))])
              (set-timer 1)
              (let loop ([n 1000]) (or (= n 0) (loop (- n 1))))))
         (let loop ([n 1000]) (or (= n 0) (loop (- n 1))))
         #f))
 )
