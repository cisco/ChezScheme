;;; exceptions.ms
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

(mat exceptions
  (begin
    (define ($$capture thunk)
      (with-output-to-string
        (lambda ()
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (x) (printf "default handler: ~s\n" x) (k))
                (lambda () (printf "~s\n" (thunk)))))))))
    (define-syntax $capture
      (syntax-rules ()
        [(_ e1 e2 ...) ($$capture (lambda () e1 e2 ...))]))
    #t)
  (equal?
    ($capture 'hello)
    "hello\n")
  (begin
    (define ($ex-test1) (raise 'oops) (printf "finished\n"))
    (define ($ex-test2) (printf "handler returned: ~s\n" (raise-continuable 'oops)) 'done)
    #t)
  (equal?
    ($capture (list ($ex-test1)))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (printf "hello: ~s\n" arg))
          $ex-test1)))
    "hello: oops\ndefault handler: #<condition &non-continuable>\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (raise (list arg)))
          $ex-test1)))
    "default handler: (oops)\n")
  (equal?
    ($capture (list ($ex-test2)))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (printf "hello: ~s\n" arg) 17)
          $ex-test2)))
    "hello: oops\nhandler returned: 17\n(done)\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (raise (list arg)))
          $ex-test2)))
    "default handler: (oops)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise '()))))
    "(empty)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise '(a . b)))))
    "((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise 'oops))))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '()))))))
    "just passing through...\n(empty)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '(a . b)))))))
    "just passing through...\n((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise 'oops))))))
    "just passing through...\ndefault handler: oops\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo]
                    [else (raise 'hair)])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '(a . b)))))))
    "just passing through...\n((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo]
                    [else (raise 'hair)])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise 'oops))))))
    "just passing through...\ndefault handler: hair\n")
  (equal?
    ($capture
      (list
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (arg) (printf "outer handler: ~s\n" arg) (k 'fini))
              (lambda ()
                (guard (foo [(begin (printf "checking null\n") (null? foo)) 'empty]
                            [(begin (printf "checking pair\n") (pair? foo)) foo])
                  (dynamic-wind
                    (lambda () (printf "in\n"))
                    (lambda () (raise 'oops))
                    (lambda () (printf "out\n"))))))))))
    "in\nout\nchecking null\nchecking pair\nin\nouter handler: oops\nout\n(fini)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "returning...\n"))
            (lambda () (raise-continuable 'oops) 'continuing)))))
    "returning...\n(continuing)\n")
  (equal?
    ($capture
     ; test to make sure guard reraises with raise-continuable per r6rs errata
      (list
        (with-exception-handler
          (lambda (x) (printf "returning...\n"))
          (lambda ()
            (guard (foo [(null? foo) 'empty]
                        [(pair? foo) foo])
              (raise-continuable 'oops)
              'continuing)))))
    "returning...\n(continuing)\n")

  ;; check that a thread starts with the default exception-handler stack
  (or (not (threaded?))
      (let ([thread-exn-handled? #f])
        (let ([th
               (with-exception-handler
                (lambda (e) (set! thread-exn-handled? #t) (exit))
                (lambda ()
                  (parameterize ([base-exception-handler (lambda (exn) (exit))])
                    (fork-thread (lambda () (error 't ""))))))])
          (thread-join th)
          (not thread-exn-handled?))))

)

(mat assert
  (equal?
    (begin (assert #t) "yes")
    "yes")
  (equal?
    (assert (memq 'a '(1 2 a 3 4)))
    '(a 3 4))
  (error? ; assertion failed
    (assert (memq 'b '(1 2 a 3 4))))
  (equal?
    (begin (assert (< 3 4)) "yes")
    "yes")
  (equal?
    (guard (c [#t "yes"])
      (begin (assert #f) "no"))
    "yes")
  (equal?
    (guard (c [#t "yes"])
      (begin (assert (< 4 3)) "no"))
    "yes")
 ; make sure pattern variables and ellipses on RHS don't screw us up
  (equal?
    (guard (c [#t "oops"])
      (let-syntax ([q (lambda (x) #t)])
        (assert (q ...))
        "okay"))
    "okay")
  (equal?
    (guard (c [#t "oops"])
      (let-syntax ([q (lambda (x) #f)])
        (assert (q ...))
        "okay"))
    "oops")
  (error? ; assertion failed
    (let-syntax ([q (lambda (x) #f)])
      (assert (q ...))
      "okay"))
  (equal?
    (syntax-case '(a b c) ()
      [(x ...)
       (begin
         (assert (andmap symbol? #'(x ...)))
         #'((x . x) ...))])
    '((a . a) (b . b) (c . c)))
  (error? ; assertion failed
    (syntax-case '(a b 3) ()
      [(x ...)
       (begin
         (assert (andmap symbol? #'(x ...)))
         #'((x . x) ...))]))
)

(mat exceptions-r6rs ; r6rs examples
  (equal?
    ($capture
      (guard (con
               ((error? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "an error has occurred"))
                'error)
               ((violation? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "the program has a bug"))
                'violation))
        (raise
          (condition
            (make-error)
            (make-message-condition "I am an error")))))
    "I am an errorerror\n")
  (equal?
    ($capture
      (guard (con
               ((error? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "an error has occurred"))
                'error))
        (raise
          (condition
            (make-violation)
            (make-message-condition "I am an error")))))
    "default handler: #<compound condition>\n")
  (equal?
    ($capture
      (with-exception-handler
        (lambda (con)
          (cond
            ((not (warning? con))
             (raise con))
            ((message-condition? con)
             (display (condition-message con)))
            (else
             (display "a warning has been issued")))
          42)
        (lambda ()
          (+ (raise-continuable
               (condition
                 (make-warning)
                 (make-message-condition
                   "should be a number")))
             23))))
    "should be a number65\n")
)

(mat conditions-r6rs ; r6rs examples
  (begin
    (define-record-type ($co-&cond1 $co-make-cond1 $co-real-cond1?)
      (parent &condition)
      (fields (immutable x $co-real-cond1-x)))
    (define $co-cond1?
      (condition-predicate
        (record-type-descriptor $co-&cond1)))
    (define $co-cond1-x
      (condition-accessor
        (record-type-descriptor $co-&cond1)
        $co-real-cond1-x))
    (define $co-foo ($co-make-cond1 'foo))
    #t)
  (condition? $co-foo)
  ($co-cond1? $co-foo)
  (eq? ($co-cond1-x $co-foo) 'foo)
  (begin
    (define-record-type ($co-&cond2 $co-make-cond2 $co-real-cond2?)
      (parent &condition)
      (fields
       (immutable y $co-real-cond2-y)))
    (define $co-cond2?
      (condition-predicate
        (record-type-descriptor $co-&cond2)))
    (define $co-cond2-y
      (condition-accessor
         (record-type-descriptor $co-&cond2)
         $co-real-cond2-y))
    (define $co-bar ($co-make-cond2 'bar))
  #t)
  (condition? (condition $co-foo $co-bar))
  ($co-cond1? (condition $co-foo $co-bar))
  ($co-cond2? (condition $co-foo $co-bar))
  ($co-cond1? (condition $co-foo))
  (list?
    (memq
      ($co-real-cond1? (condition $co-foo))
      '(#t #f)))
  (not ($co-real-cond1? (condition $co-foo $co-bar)))
  (eq? ($co-cond1-x (condition $co-foo $co-bar)) 'foo)
  (eq? ($co-cond2-y (condition $co-foo $co-bar)) 'bar)
  (equal?
    (simple-conditions (condition $co-foo $co-bar))
    (list $co-foo $co-bar))
  (equal?
    (simple-conditions (condition $co-foo (condition $co-bar)))
    (list $co-foo $co-bar))
  (begin
    (define-condition-type $co-&c &condition $co-make-c $co-c? (x $co-c-x))
    (define-condition-type $co-&c1 $co-&c $co-make-c1 $co-c1? (a $co-c1-a))
    (define-condition-type $co-&c2 $co-&c $co-make-c2 $co-c2? (b $co-c2-b))
    (define $co-v1 ($co-make-c1 "V1" "a1"))
    #t)
  ($co-c? $co-v1)
  ($co-c1? $co-v1)
  (not ($co-c2? $co-v1))
  (equal? ($co-c-x $co-v1) "V1")
  (equal? ($co-c1-a $co-v1) "a1")
  (begin
    (define $co-v2 ($co-make-c2 "V2" "b2"))
    (define $co-v3 (condition ($co-make-c1 "V3/1" "a3") ($co-make-c2 "V3/2" "b3")))
    (define $co-v4 (condition $co-v1 $co-v2))
    (define $co-v5 (condition $co-v2 $co-v3))
    #t)
  ($co-c? $co-v2)
  (not ($co-c1? $co-v2))
  ($co-c2? $co-v2)
  (equal? ($co-c-x $co-v2) "V2")
  (equal? ($co-c2-b $co-v2) "b2")
  ($co-c? $co-v3)
  ($co-c1? $co-v3)
  ($co-c2? $co-v3)
  (equal? ($co-c-x $co-v3) "V3/1")
  (equal? ($co-c1-a $co-v3) "a3")
  (equal? ($co-c2-b $co-v3) "b3")
  ($co-c? $co-v4)
  ($co-c1? $co-v4)
  ($co-c2? $co-v4)
  (equal? ($co-c-x $co-v4) "V1")
  (equal? ($co-c1-a $co-v4) "a1")
  (equal? ($co-c2-b $co-v4) "b2")
  ($co-c? $co-v5)
  ($co-c1? $co-v5)
  ($co-c2? $co-v5)
  (equal? ($co-c-x $co-v5) "V2")
  (equal? ($co-c1-a $co-v5) "a3")
  (equal? ($co-c2-b $co-v5) "b2")
)

(mat system-exceptions
  (equal?
    ($capture
     ; from r6rs
      (guard (con
               ((error? con)
                (display "error opening file")
                #f))
        (call-with-input-file "/probably/not/here" read)))
    "error opening file#f\n")
  (guard (c [else (and (assertion-violation? c)
                       (not (implementation-restriction-violation? c)))])
    (let ()
      (define-record-type foo (fields x))
      (foo-x 17)))
)

(mat exception-state
  (#%$record? (current-exception-state))
  (not (record? (current-exception-state)))
  (eq?
    (call/cc
      (lambda (k)
        (parameterize ([current-exception-state
                        (create-exception-state
                          (lambda (x)
                            (if (eq? x 'oops)
                                (raise 'rats)
                                (k x))))])
          (raise 'oops))))
    'rats)
)
