;;; 6.ms
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

;;; sections 6-1 and 6-2:

(define prettytest.ss (format "~a/prettytest.ss" *mats-dir*))

(mat current-input-port
   (port? (current-input-port))
   (input-port? (current-input-port))
   (eq? (current-input-port) (console-input-port))
 )

(mat current-output-port
   (port? (current-output-port))
   (output-port? (current-output-port))
   (eq? (current-output-port) (console-output-port))
 )

(mat port-operations
   (error? (open-input-file "nonexistent file"))
   (error? (open-input-file "nonexistent file" 'compressed))
   (error? (open-output-file "/nonexistent/directory/nonexistent/file"))
   (error? (open-output-file "/nonexistent/directory/nonexistent/file" 'replace))
   (error? (open-input-output-file "/nonexistent/directory/nonexistent/file"))
   (error? (open-input-output-file "/nonexistent/directory/nonexistent/file" 'truncate))
   ; the following several clauses test various open-output-file options
   (let ([p (open-output-file "testfile.ss" 'truncate)])
      (and (port? p) (output-port? p) (begin (close-output-port p) #t)))
   (error? (open-output-file "testfile.ss"))
   (error? (open-output-file "testfile.ss" 'error))
   (let ([p (open-output-file "testfile.ss" 'replace)])
      (and (port? p) (output-port? p) (begin (close-output-port p) #t)))
   (let ([p (open-output-file "testfile.ss" 'truncate)])
      (and (port? p) (output-port? p) (begin (close-output-port p) #t)))
   (let ([p (open-output-file "testfile.ss" 'truncate)])
      (display "\"hello" p)
      (close-output-port p)
      (let ([p (open-output-file "testfile.ss" 'append)])
         (display " there\"" p)
         (close-output-port p)
         (let ([p (open-input-file "testfile.ss")])
            (and (equal? (read p) "hello there")
                 (eof-object? (read p))
                 (begin (close-input-port p) #t)))))
   ; the following tests open-output-file, close-output-port, write,
   ; display, and newline---and builds testfile.ss for the next test
   (let ([p (let loop () (if (file-exists? "testfile.ss")
                             (begin (delete-file "testfile.ss" #f) (loop))
                             (open-output-file "testfile.ss")))])
      (for-each (lambda (x) (write x p) (display " " p))
                '(a b c d e))
      (newline p)
      (close-output-port p)
      #t)
   ; the following tests open-input-file, close-input-port, read,
   ; and eof-object?
   (equal? (let ([p (open-input-file "testfile.ss")])
              (let f ([x (read p)])
                 (if (eof-object? x)
                     (begin (close-input-port p) '())
                     (cons x (f (read p))))))
           '(a b c d e))
   ; the following tests with-output-to-file, close-port,
   ; and write-char---and builds testfile.ss for the next test
   (equal? (call-with-values
             (lambda ()
               (with-output-to-file "testfile.ss" 
                 (lambda ()
                   (for-each (lambda (c) (write-char c))
                             (string->list "a b c d e"))
                  (values 1 2 3))
                 'replace))
             list)
           '(1 2 3))
   ; the following tests with-input-from-file, close-port,
   ; read-char, unread-char, and eof-object?
   (equal? (with-input-from-file "testfile.ss"
              (lambda ()
                 (list->string
                    (let f ()
                       (let ([c (read-char)])
                          (if (eof-object? c)
                              '()
                              (begin (unread-char c)
                                     (let ([c (read-char)])
                                        (cons c (f))))))))))
           "a b c d e")
   ; the following tests call-with-output-file, close-port,
   ; and write-char---and builds testfile.ss for the next test
   (equal? (call-with-values
             (lambda ()
               (call-with-output-file "testfile.ss" 
                 (lambda (p)
                   (for-each (lambda (c) (write-char c p))
                             (string->list "a b c d e"))
                   (close-port p)
                  (values 1 2 3))
                 'replace))
             list)
           '(1 2 3))
   ; the following tests call-with-input-file, close-port,
   ; read-char, unread-char, and eof-object?
   (equal? (call-with-input-file "testfile.ss"
              (lambda (p)
                 (list->string
                    (let f ()
                       (let ([c (read-char p)])
                          (if (eof-object? c)
                              (begin (close-port p) '())
                              (begin (unread-char c p)
                                     (let ([c (read-char p)])
                                        (cons c (f))))))))))
           "a b c d e")
   ; the following tests call-with-input-file, close-port,
   ; read-char, unread-char, and eof-object?
   (equal? (call-with-values
             (lambda ()
               (call-with-input-file "testfile.ss"
                 (lambda (p)
                    (apply values
                      (let f ()
                        (let ([c (read-char p)])
                          (if (eof-object? c)
                              (begin (close-port p) '())
                              (begin (unread-char c p)
                                     (let ([c (read-char p)])
                                        (cons c (f)))))))))))
             (lambda ls (list->string ls)))
           "a b c d e")
   ; the following tests call-with-input-file, close-input-port,
   ; read-char, peek-char, and eof-object?
   (equal? (call-with-input-file "testfile.ss"
              (lambda (p)
                 (list->string
                    (let f ()
                       (let ([c (peek-char p)])
                          (if (eof-object? c)
                              (begin (close-input-port p) '())
                              (let ([c (read-char p)])
                                 (cons c (f)))))))))
           "a b c d e")
   ; test various errors related to input ports
   (begin (set! ip (open-input-file "testfile.ss"))
          (and (port? ip) (input-port? ip)))
   (error? (unread-char #\a ip))
   (eqv? (read-char ip) #\a)
   (begin (unread-char #\a ip) (eqv? (read-char ip) #\a))
   (begin (clear-input-port ip) #t)
   (error? (unread-char #\a ip))
   (error? (write-char #\a ip))
   (error? (write 'a ip))
   (error? (display 'a ip))
   (error? (newline ip))
   (error? (fprintf ip "hi"))
   (error? (flush-output-port ip))
   (error? (clear-output-port ip))
   (begin (close-input-port ip) #t)
   (error? (read-char ip))
   (error? (read ip))
   (error? (char-ready? ip))
   ; test various errors related to output ports
   (begin (set! op (open-output-file "testfile.ss" 'replace))
          (and (port? op) (output-port? op)))
   (error? (char-ready? op))
   (error? (peek-char op))
   (error? (read-char op))
   (error? (unread-char #\a op))
   (error? (read op))
   (error? (clear-input-port op))
   (begin (close-output-port op) #t)
   (error? (write-char #\a op))
   (error? (write 'a op))
   (error? (display 'a op))
   (error? (newline op))
   (error? (fprintf op "hi"))
   (error? (flush-output-port op))
   (error? (clear-output-port op))
   (error? (current-output-port 'a))
   (error? (current-input-port 'a))
   (begin (current-output-port (console-output-port)) #t)
   (begin (current-input-port (console-input-port)) #t)

   ; the following tests open-input-string, open-output-string, read-char,
   ; eof-object?, unread-char, write-char, and get-output-string
   (let ([s "hi there, mom!"])
      (let ([ip (open-input-string s)] [op (open-output-string)])
         (do ([c (read-char ip) (read-char ip)])
             ((eof-object? c)
              (equal? (get-output-string op) s))
             (unread-char c ip)
             (write-char (read-char ip) op))))

   (error? (with-input-from-string))
   (error? (with-input-from-string "a"))
   (error? (with-input-from-string 'a (lambda () 3)))
   (error? (with-input-from-string "a" 'foo))
   (error? (with-input-from-string (lambda () 3) "a"))
   (error? (with-input-from-string '(this too?) values))
   (error? (with-input-from-string "a" (lambda () 3) 'compressed))
   (error? (with-output-to-string))
   (error? (with-output-to-string "a"))
   (error? (with-output-to-string 'a (lambda () 3)))
   (error? (with-output-to-string '(this too?)))
   (error? (eof-object #!eof))
   (eq? (with-input-from-string "" read) #!eof)
   (eq? (with-input-from-string "" read) (eof-object))
   (eq? (eof-object) #!eof)
   (error? (with-input-from-string "'" read))
   ; the following tests with-input-from-string, with-output-to-string,
   ; read-char, eof-object?, unread-char, and write-char
   (let ([s "hi there, mom!"])
     (equal?
       (with-input-from-string s
         (lambda ()
           (with-output-to-string
             (lambda ()
               (do ([c (read-char) (read-char)])
                   ((eof-object? c))
                 (unread-char c)
                 (write-char (read-char)))))))
       s))

   ; the following makes sure that call-with-{in,out}put-file close the
   ; port (from Dave Boyer)---at least on systems which restrict the
   ; number of open ports to less than 20
   (let loop ((i 20))
      (or (zero? i)
          (begin (call-with-output-file "testfile.ss"
                    (lambda (p) (write i p))
                    'replace)
                 (and (eq? (call-with-input-file "testfile.ss"
                              (lambda (p) (read p)))
                           i)
                      (loop (- i 1))))))

   ; test source information in error messages from read
   (error?
     (begin
       (with-output-to-file "testfile.ss"
         (lambda () (display "(cons 1 2 . 3 4)"))
         'replace)
       (let ([ip (open-input-file "testfile.ss")])
         (dynamic-wind
           void
           (lambda () (read ip))
           (lambda () (close-input-port ip))))))

   ; test source information in error messages from read
   (error?
     (begin
       (with-output-to-file "testfile.ss"
         (lambda () (display "(cons 1 2 ] 3 4)"))
         'replace)
       (let ([ip (open-input-file "testfile.ss")])
         (dynamic-wind
           void
           (lambda () (read ip))
           (lambda () (close-input-port ip))))))
 )

(mat port-operations1
  (error? (open-input-output-file))
  (error? (open-input-output-file 'furball))
  (error? (open-input-output-file "/probably/not/a/good/path"))
  (error? (open-input-output-file "testfile.ss" 'compressed))
  (error? (open-input-output-file "testfile.ss" 'uncompressed))
  (begin
    (define $ppp (open-input-output-file "testfile.ss"))
    (and (input-port? $ppp) (output-port? $ppp) (port? $ppp)))
  (error? (truncate-file $ppp -3))
  (error? (truncate-file $ppp 'all-the-way))
  (eof-object?
    (begin
      (truncate-file $ppp)
      (display "hello" $ppp)
      (flush-output-port $ppp)
      (read $ppp)))
  (eq? (begin (file-position $ppp 0) (read $ppp)) 'hello)
  (eqv? (begin
          (display "goodbye\n" $ppp)
          (truncate-file $ppp 9)
          (file-position $ppp))
        9)
  (eof-object? (read $ppp))
  (eqv? (begin (file-position $ppp 0) (file-position $ppp)) 0)
  (eq? (read $ppp) 'hellogood)
  (eqv? (begin
          (display "byebye\n" $ppp)
          (truncate-file $ppp 0)
          (file-position $ppp))
        0)
  (eof-object? (read $ppp))
  (eof-object?
    (begin
      (close-port $ppp)
      (let ([ip (open-input-file "testfile.ss")])
        (let ([c (read-char ip)])
          (close-input-port ip)
          c))))
  (error?
    (let ([ip (open-input-file "testfile.ss")])
      (dynamic-wind
        void
        (lambda () (truncate-file ip))
        (lambda () (close-input-port ip)))))
  (error? (truncate-file 'animal-crackers))
  (error? (truncate-file))
  (error? (truncate-file $ppp))
  (let ([op (open-output-string)])
    (and (= (file-position op) 0)
         (= (file-length op) 0)
         (begin (fresh-line op) #t)
         (= (file-length op) 0)
         (= (file-position op) 0)
         (do ([i 4000 (fx- i 1)])
             ((fx= i 0) #t)
           (display "hello" op))
         (= (file-length op) 20000)
         (= (file-position op) 20000)
         (begin (file-position op 5000) #t)
         (= (file-position op) 5000)
         (= (file-length op) 20000)
         (begin (truncate-file op) #t)
         (= (file-length op) 0)
         (= (file-position op) 0)
         (begin (truncate-file op 17) #t)
         (= (file-length op) 17)
         (= (file-position op) 17)
         (begin (display "okay" op) #t)
         (= (file-length op) 21)
         (= (file-position op) 21)
         (equal? (substring (get-output-string op) 17 21) "okay")
         (= (file-length op) 0)
         (= (file-position op) 0)
         (begin (fresh-line op) #t)
         (= (file-length op) 0)
         (= (file-position op) 0)
         (begin
           (write-char #\a op)
           (fresh-line op)
           #t)
         (= (file-position op) 2)
         (begin (fresh-line op) #t)
         (= (file-position op) 2)
         (equal? (get-output-string op) "a\n")))
  (let ([ip (open-input-string "beam me up, scotty!")]
        [s (make-string 10)])
    (and (= (file-position ip) 0)
         (= (file-length ip) 19)
         (not (eof-object? (peek-char ip)))
         (equal? (read ip) 'beam)
         (= (file-position ip) 4)
         (not (eof-object? (peek-char ip)))
         (equal? (block-read ip s 10) 10)
         (equal? s " me up, sc")
         (= (file-position ip) 14)
         (equal? (block-read ip s 10) 5)
         (equal? s "otty!p, sc")
         (= (file-position ip) 19)
         (eof-object? (peek-char ip))
         (eof-object? (read-char ip))
         (eof-object? (block-read ip s 10))
         (eof-object? (block-read ip s 0))
         (begin
           (file-position ip 10)
           (= (file-position ip) 10))
         (equal? (block-read ip s 10) 9)
         (equal? s ", scotty!c")))
  (error? ; unhandled message
    (get-output-string (open-input-string "oops")))
  (error? ; unhandled message
    (let ([op (open-output-file "testfile.ss" 'replace)])
      (dynamic-wind
        void
        (lambda () (get-output-string op))
        (lambda () (close-output-port op)))))
 )

(mat compression
  (let ()
    (define cp
      (lambda (mode src dst)
        (define buf-size 4096)
        (let ([buf (make-string buf-size)])
          (call-with-output-file dst
            (lambda (op)
              (call-with-input-file src
                (lambda (ip)
                  (let lp ()
                    (let ([n (block-read ip buf buf-size)])
                      (unless (eof-object? n) (block-write op buf n) (lp)))))))
            mode))))
    (define cmp
      (lambda (mode1 src1 mode2 src2)
        (define buf-size 4096)
        (let ([buf1 (make-string buf-size)]
              [buf2 (make-string buf-size)])
          (call-with-input-file src1
            (lambda (ip1)
              (call-with-input-file src2
                (lambda (ip2)
                  (let lp ()
                    (let ([n1 (block-read ip1 buf1 buf-size)]
                          [n2 (block-read ip2 buf2 buf-size)])
                      (if (eof-object? n1)
                          (eof-object? n2)
                          (and (eqv? n1 n2)
                               (string=? (substring buf1 0 n1)
                                         (substring buf2 0 n2))
                               (lp))))))
                mode2))
            mode1))))
    (and
      (cmp '() prettytest.ss '() prettytest.ss)
      (cmp '(compressed) prettytest.ss '() prettytest.ss)
      (cmp '() prettytest.ss '(compressed) prettytest.ss)
      (cmp '(compressed) prettytest.ss '(compressed) prettytest.ss)
      (begin
        (cp '(replace compressed) prettytest.ss "testfile.ss")
        #t)
      (cmp '(compressed) "testfile.ss" '() prettytest.ss)
      (not (= (call-with-input-file "testfile.ss" file-length) (call-with-input-file prettytest.ss file-length)))
     ; the following test could cause an error with anything but latin-1 codec
      #;(not (cmp '() "testfile.ss" '() prettytest.ss))
      (begin
        (cp '(compressed append) prettytest.ss "testfile.ss")
        #t)
      (not (cmp '(compressed) "testfile.ss" '() prettytest.ss))
    ))
  (error? (open-output-file "testfile.ss" '(replace append)))
  (error? (open-output-file "testfile.ss" '(append truncate)))
 ; test workaround for bogus gzclose error return for empty input files
  (and
    (eqv? (with-output-to-file "testfile.ss" void 'replace) (void))
    (eof-object? (with-input-from-file "testfile.ss" read 'compressed)))
 )

(mat read-comment
  (equal? '; this is the first comment
           (a ; second comment
           #;(third ; comment in comment
           comment #;(comment #1=e in
           . #;(comment in comment in comment)
           comment)) b ; fourth comment
           c #| fifth comment #| more
           nesting here |# |# d
           ; sixth and final comment
           #1#)
          '(a b c d e))
  (equal? (read (open-input-string "; this is the first comment
                                    (a ; second comment
                                    #;(third ; comment in comment
                                    comment #;(comment #1=e in
                                    . #;(comment in comment in comment)
                                    comment)) b ; fourth comment
                                    c #| fifth comment #| more
                                    nesting here |# |# d
                                    ; sixth and final comment
                                    #1#)"))
        '(a b c d e))
  (equal? (read (open-input-string "(#|##|# |#|#1
                                    #||#2
                                    #|||#3
                                    #|#||#|#4
                                    #|| hello ||#5
                                    #| ; rats |#)"))
        '(1 2 3 4 5))
 )

(mat read-graph
  (begin
    (define read-test-graph
      (case-lambda
        [(s) (read-test-graph s s)]
        [(s1 s2)
         (string=?
           (parameterize ((print-graph #t))
             (format "~s" (read (open-input-string s1))))
           s2)]))
    #t)
  (error?  ; verify that the error message is NOT "invalid memory reference"
    (let ((ip (open-input-string "(cons 0 #0#)")))
      ((#%$make-read ip #t #f) #t)))
  (let ()
    (define-record foo ((immutable x) (immutable y)))
    (record-reader 'foo (record-rtd (make-foo 3 4)))
    (and
      (read-test-graph "#0=#[foo (#0#) 0]")
      (read-test-graph "#0=(#[foo #0# 0])")
      (read-test-graph "#[foo #0=(a b c) #0#]")))
  (error? (read-test-graph "#0=#[foo #0# #0#]"))
  (read-test-graph "#(123 #[foo #0=(a b c) #0#])")
  (read-test-graph "#(#0=#[foo #1=(a b c) #1#] 0 #0#)")
  (read-test-graph "#(#1# 0 #1=#[foo #0=(a b c) #0#])"
                   "#(#0=#[foo #1=(a b c) #1#] 0 #0#)")
  (read-test-graph "#(123 #0=(#0#))")
  (read-test-graph "#(123 #0=(#0#))")
  (let ()
    (define-record r1 ((mutable a) (immutable b)))
    (define-record r2 ((immutable a)))
    (let* ((x2 (make-r2 (make-r1 '* '(a b c)))) (x1 (r2-a x2)))
      (set-r1-a! x1 x2)
      (record-reader 'r1 (record-rtd (make-r1 3 4)))
      (record-reader 'r2 (record-rtd (make-r2 3)))
      (read-test-graph
        (parameterize ((print-graph #t))
          (format "~s" (list (r1-b x1) x1))))))
  (read-test-graph "(#0=(a b c) #1=#[r1 #[r2 #1#] #0#])")
 )

(mat block-io
   ; test block-write and build testfile.ss for the following test
   (let ([p (open-output-file "testfile.ss" 'truncate)])
      (block-write p "hi there")
      (display " mom" p)
      (block-write p ", how are you?xxxx" (string-length ", how are you?"))
      (newline p)
      (let ([s (make-string 100 #\X)])
         (string-set! s 99 #\newline)
         (let ([s (apply string-append (make-list 10 s))])
            (let ([s (apply string-append (make-list 10 s))])
               (block-write p s)
               (block-write p s 5000))))
      (close-output-port p)
      #t)
   ; test block-read
   (let ([random-read-up
          (lambda (p n)
             (let f ([n n] [ls '()])
                (if (fx= n 0)
                    (apply string-append (reverse ls))
                    (if (fxodd? n)
                        (f (- n 1) (cons (string (read-char p)) ls))
                        (let ([s (make-string (random (fx+ n 1)))])
                           (let ([i (if (fx= (random 2) 0)
                                        (block-read p s)
                                        (block-read p s (string-length s)))])
                              (f (- n i) (cons (substring s 0 i) ls))))))))])
      (let ([s (make-string 100 #\X)])
         (string-set! s 99 #\newline)
         (let ([s (apply string-append (make-list 10 s))])
            (let ([s (apply string-append (make-list 10 s))])
               (let ([s (string-append "hi there mom, how are you?"
                                       (string #\newline)
                                       s
                                       (substring s 0 5000))])
                  (let ([p (open-input-file "testfile.ss")])
                     (let ([t (random-read-up p (string-length s))])
                        (and (eof-object? (read-char p))
                             (string=? t s)
                             (eqv? (close-input-port p) (void))))))))))
   ; test for bug: block-read complained when handler returned eof
   (eof-object?
     (let ((p (make-input-port (lambda args #!eof) "")))
      (block-read p (make-string 100))))
)

(mat file-length-and-file-position
   (procedure? file-length)
   (procedure? file-position)
   (let ([s "hi there"])
      (let ([n (string-length s)]
            [p (open-output-file "testfile.ss" 'replace)])
         (and (eqv? (file-length p) 0)
              (begin (display s p)
                     (= (file-position p) (file-length p) n))
              (begin (display #\space p)
                     (= (file-position p) (file-length p) (+ n 1)))
              (eqv? (file-position p 1) (void))
              (write-char #\o p)
              (eqv? (file-position p 2000) (void))
              (begin (display s p)
                     (= (file-length p) (file-position p) (+ 2000 n)))
              (eqv? (close-output-port p) (void)))))
;;; no error is reported, which isn't serious
;   (error? (file-position (open-input-file "testfile.ss") 10000))
   (error?
     (let ((p (open-input-file "testfile.ss")))
       (dynamic-wind
         void
         (lambda () (file-position p -1))
         (lambda () (close-input-port p)))))
   (guard (c [(i/o-invalid-position-error? c)])
     (let ([p (open-input-file "testfile.ss")])
       (dynamic-wind
         void
         (lambda ()
           (file-position p (if (fixnum? (expt 2 32)) (- (expt 2 63) 1) (- (expt 2 31) 1)))
           #t)
         (lambda () (close-input-port p)))))
   (error?
     (let ([p (open-input-file "testfile.ss")])
       (dynamic-wind
         void
         (lambda () (file-position p (expt 2 64)))
         (lambda () (close-input-port p)))))
   (error? (file-position 1))
   (error? (file-length 1))
   (let ([s "hi there"])
      (let ([n (string-length s)] [p (open-input-file "testfile.ss")])
         (and (eqv? (file-length p) (+ 2000 n))
              (eq? (read p) 'ho)
              (eq? (read p) 'there)
              (eqv? (file-position p) n)
              (eqv? (file-position p 2000) (void))
              (eq? (read p) 'hi)
              (eq? (read p) 'there)
              (= (file-position p) (file-length p) (+ 2000 n))
              (eqv? (close-input-port p) (void)))))
 )

(mat string-port-file-position
  (let ([ip (open-input-string "hit me")])
    (and (eq? (read ip) 'hit)
         (eq? (file-position ip) 3)
         (begin
           (file-position ip 1)
           (eq? (read ip) 'it))
         (begin
           (file-position ip 6)
           (eof-object? (read ip)))
         (begin
           (file-position ip 0)
           (eq? (read ip) 'hit))))
  (error? (file-position (open-input-string "hi") 3))
  (error? (file-position (open-input-string "hi") -1))
  (let ()
    (define f
      (lambda (n)
        (let ([op (open-output-string)])
          (and (begin
                 (write 'ab op)
                 (eq? (file-position op) 2))
               (begin
                 (file-position op 4)
                 (write 'ef op)
                 (eq? (file-position op) 6))
               (begin
                 (file-position op 2)
                 (write 'cd op)
                 (eq? (file-position op) 4))
               (begin
                 (set-port-length! op n)
                 (get-output-string op))))))
    (and (equal? (f 6) "abcdef")
         (equal? (f 4) "abcd")
         (equal? (f 2) "ab")
         (equal? (f 0) "")
         (equal? (f 5) "abcde")
         (let ((s (f 2000)))
           (and s (= (string-length s) 2000)))))
  (error? (file-position (open-output-string) -1))
 )

(mat fresh-line
  (procedure? fresh-line)
  (error? (fresh-line 3))
  (error? (fresh-line (open-input-string "hello")))
  (equal?
    (with-output-to-string
      (lambda ()
        (fresh-line)
        (fresh-line)
        (display "hello")
        (fresh-line)
        (fresh-line)))
    "hello\n")
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (fresh-line)
        (fresh-line)
        (display "hello")
        (fresh-line)
        (fresh-line))
      'replace)
    #t)
  (call-with-input-file "testfile.ss"
    (lambda (p)
      (let ([s (make-string 100)])
        (and
          (= (block-read p s (string-length s)) 6)
          (string=? (substring s 0 6) "hello\n")
          (eof-object? (read-char p))))))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (write-char #\a)
        (fresh-line)
        (flush-output-port)
        (set-port-bol! (current-output-port) #f)
        (fresh-line)
        (write-char #\b)
        (flush-output-port)
        (set-port-bol! (current-output-port) #t)
        (fresh-line)
        (fresh-line)
        (write-char #\c)
        (fresh-line)
        (fresh-line))
      'replace)
    #t)
  (call-with-input-file "testfile.ss"
    (lambda (p)
      (let ([s (make-string 100)])
        (and
          (= (block-read p s (string-length s)) 6)
          (string=? (substring s 0 6) "a\n\nbc\n")
          (eof-object? (read-char p))))))
 )

(mat char-ready?
   (procedure? char-ready?)
   (let ([x (open-input-string "a")])
      (and (char-ready? x)
           (eqv? (read-char x) #\a)
           (char-ready? x)
           (eof-object? (read-char x))))
   (parameterize ([current-input-port (open-input-string "a")])
      (and (char-ready?)
           (eqv? (read-char) #\a)
           (char-ready?)
           (eof-object? (read-char))))
 )

(mat clear-input-port ; test interactively
   (procedure? clear-input-port)
 )

;;; pretty-equal? is like equal? except that it considers gensyms
;;; with equal print names to be equal and any two nans to be equal.
(define pretty-equal?
   (rec equal?
      (lambda (x y) ; mostly snarfed from 5_1.ss
         (or (cond
                [(eq? x y) #t]
                [(pair? x)
                 (and (pair? y)
                      (equal? (car x) (car y))
                      (equal? (cdr x) (cdr y)))]
                [(symbol? x)
                 (and (gensym? x)
                      (gensym? y)
                      (string=? (symbol->string x) (symbol->string y)))]
                [(or (null? x) (null? y)) #f]
                [(or (char? x) (char? y)) #f]
                [(flonum? x)
                 (and (flonum? y)
                      (or (let ([nan? (lambda (x) (not (fl= x x)))])
                            (and (nan? x) (nan? y)))
                          (fl= x y)))]
                [(number? x)
                 (and (number? y)
                      (if (exact? x)
                          (and (exact? y) (= x y))
                          (and (equal? (real-part x) (real-part y))
                               (equal? (imag-part x) (imag-part y)))))]
                [(string? x) (and (string? y) (string=? x y))]
                [(box? x) (and (box? y) (equal? (unbox x) (unbox y)))]
                [(vector? x)
                 (and (vector? y)
                      (= (vector-length x) (vector-length y))
                      (let f ([i (- (vector-length x) 1)])
                         (or (< i 0)
                             (and (equal? (vector-ref x i) (vector-ref y i))
                                  (f (1- i))))))]
                [(fxvector? x)
                 (and (fxvector? y)
                      (= (fxvector-length x) (fxvector-length y))
                      (let f ([i (- (fxvector-length x) 1)])
                         (or (< i 0)
                             (and (fx= (fxvector-ref x i) (fxvector-ref y i))
                                  (f (1- i))))))]
                [(bytevector? x)
                 (and (bytevector? y)
                      (bytevector=? x y))]
                [else #f])
             (parameterize ([print-length 6] [print-level 3])
                (display "----------------------\n")
                (pretty-print x)
                (pretty-print '=/=)
                (pretty-print y)
                (display "----------------------\n")
                #f)))))

(mat pretty-print
   (let ([pretty-copy
          (lambda (ifn ofn)
             (let ([ip (open-input-file ifn)]
                   [op (open-output-file ofn 'replace)])
                (dynamic-wind
                   (lambda () #f)
                   (rec loop
                      (lambda ()
                         (let ([x (read ip)])
                            (or (eof-object? x)
                                (parameterize ([print-unicode #f])
                                  (pretty-print x op)
                                  (newline op)
                                  (loop))))))
                   (lambda ()
                      (close-input-port ip)
                      (close-output-port op)))))])
      (pretty-copy prettytest.ss "testfile.ss"))
   (let ([p1 (open-input-file prettytest.ss)]
         [p2 (open-input-file "testfile.ss")])
      (dynamic-wind
         (lambda () #f)
         (rec loop
            (lambda ()
               (let ([x1 (read p1)] [x2 (read p2)])
                  (unless (pretty-equal? x1 x2)
                     (errorf 'pretty-equal "~s is not equal to ~s" x1 x2))
                  (or (eof-object? x1) (loop)))))
         (lambda ()
            (close-input-port p1)
            (close-input-port p2))))
   (error? (pretty-format))
   (error? (pretty-format 'foo 'x 'x))
   (error? (pretty-format 3 'x))
   (error? (pretty-format 'foo '(bad 0 ... ... 0 format)))
   (list? (pretty-format 'let))
   (let ([x (pretty-format 'let)])
     (pretty-format 'let x)
     (equal? x (pretty-format 'let)))
   (string=?
     (parameterize ([pretty-standard-indent 2] [pretty-one-line-limit 1])
       (pretty-format 'frob '(frob (x 1 ...) 3 (x #f ...) 4 (x y 3 ...) ...))
       (with-output-to-string
         (lambda ()
           (pretty-print '(frob (alpha b c d)
                                (peter o n m)
                                (zero 1 2 3)
                                (nine 8 7 6))))))
     "(frob (alpha\n        b\n        c\n        d)\n    (peter\n       o\n       n\n       m)\n     (zero 1\n         2\n         3)\n     (nine 8\n         7\n         6))\n")
   (eqv? (begin (pretty-format 'frob #f) (pretty-format 'frob)) #f)
   (equal?
     (with-output-to-string
       (lambda ()
         (pretty-print ''#'#`#,#,@,,@`(a b c))))
     "'#'#`#,#,@,,@`(a b c)\n")
 )

(mat write
   (let ([unpretty-copy
          (lambda (ifn ofn)
             (let ([ip (open-input-file ifn)]
                   [op (open-output-file ofn 'replace)])
                (dynamic-wind
                   (lambda () #f)
                   (rec loop
                      (lambda ()
                         (let ([x (read ip)])
                            (or (eof-object? x)
                                (parameterize ([print-unicode #f])
                                  (write x op)
                                  (newline op)
                                  (loop))))))
                   (lambda ()
                      (close-input-port ip)
                      (close-output-port op)))))])
      (unpretty-copy prettytest.ss "testfile.ss"))
   (let ([p1 (open-input-file prettytest.ss)]
         [p2 (open-input-file "testfile.ss")])
      (dynamic-wind
         (lambda () #f)
         (rec loop
            (lambda ()
               (let ([x1 (read p1)] [x2 (read p2)])
                  (unless (pretty-equal? x1 x2)
                     (errorf 'pretty-equal "~s is not equal to ~s" x1 x2))
                  (or (eof-object? x1) (loop)))))
         (lambda ()
            (close-input-port p1)
            (close-input-port p2))))
 )

(mat fasl
  (error?
    (separate-eval '(let ([op (open-file-output-port "testfile.ss" (file-options compressed replace))])
                      (fasl-write 3 op))))
  (error?
    (separate-eval '(let ([ip (open-file-input-port "testfile.ss" (file-options compressed))])
                      (fasl-read ip))))
  (equal?
    (separate-eval '(with-exception-handler
                      (lambda (c) (unless (warning? c) (raise-continuable c)))
                      (lambda ()
                        (let ([op (open-file-output-port "testfile.ss" (file-options compressed replace))])
                          (fasl-write 3 op)))))
    "")
  (equal?
    (separate-eval `(with-exception-handler
                      (lambda (c) (unless (warning? c) (raise-continuable c)))
                      (lambda ()
                        (let ([ip (open-file-input-port "testfile.ss" (file-options compressed))])
                          (fasl-read ip)))))
    "3\n")
  (pretty-equal?
    (begin
      (call-with-port
        (open-file-output-port "testfile.ss" (file-options replace))
        (lambda (p) (fasl-write +nan.0 p)))
      (call-with-port (open-file-input-port "testfile.ss") fasl-read))
    (/ 0.0 0.0))
  (let ([ls (with-input-from-file prettytest.ss
              (rec f
                (lambda ()
                  (let ([x (read)])
                    (if (eof-object? x) '() (cons x (f)))))))])
    (define-record frob (x1 (uptr x2) (fixnum x3) (float x4) (double x5) (wchar_t x6) (integer-64 x7) (char x8) (unsigned-64 x9)))
    (let ([x (make-frob '#(#&3+4i 3.456+723i 3/4) 7500000 (most-negative-fixnum) +nan.0 3.1415 #\x3d0
               (- (expt 2 63) 5) #\$ (- (expt 2 64) 5))])
      (define put-stuff
        (lambda (p)
          (fasl-write (cons x x) p)
          (fasl-write (list +nan.0 +inf.0 -inf.0 -0.0) p)
          (for-each (lambda (x) (fasl-write x p)) ls)))
      (define (get-stuff fasl-read)
        (lambda (p)
          (let ([y (fasl-read p)])
            (and (equal? ($record->vector (car y)) ($record->vector x))
                 (eq? (cdr y) (car y))
                 (pretty-equal? (fasl-read p) (list +nan.0 +inf.0 -inf.0 -0.0))
                 (let loop ([ls ls])
                   (let ([x1 (fasl-read p)] [x2 (if (null? ls) #!eof (car ls))])
                     (unless (pretty-equal? x1 x2)
                       (errorf #f "~s is not equal to ~s" x1 x2))
                     (or (eof-object? x1) (loop (cdr ls)))))))))
      (call-with-port
        (open-file-output-port "testfile.ss" (file-options replace))
        put-stuff)
      (and
        (call-with-port
          (open-file-input-port "testfile.ss")
          (get-stuff fasl-read))
        (call-with-port
          (open-file-input-port "testfile.ss" (file-options #;compressed))
          (get-stuff fasl-read))
        (call-with-port
          (open-file-input-port "testfile.ss" (file-options #;compressed))
          (get-stuff (lambda (p)
                       (when (eof-object? (lookahead-u8 p)) (printf "done\n"))
                       (fasl-read p))))
        (begin
          (call-with-port
            (open-file-output-port "testfile.ss" (file-options compressed replace))
            put-stuff)
          (call-with-port
            (open-file-input-port "testfile.ss" (file-options compressed))
            (get-stuff fasl-read)))
        (call-with-port
          (open-bytevector-input-port
            (call-with-bytevector-output-port put-stuff))
          (get-stuff fasl-read)))))
  (eqv? (fasl-file prettytest.ss "testfile.ss") (void))
  (let ([ls (with-input-from-file prettytest.ss
              (rec f
                (lambda ()
                  (let ([x (read)])
                    (if (eof-object? x) '() (cons x (f)))))))])
    (call-with-port
      (open-file-input-port "testfile.ss")
      (lambda (p)
        (let loop ([ls ls])
          (let ([x1 (fasl-read p)] [x2 (if (null? ls) #!eof (car ls))])
            (unless (pretty-equal? x1 x2)
              (errorf #f "~s is not equal to ~s" x1 x2))
            (or (eof-object? x1) (loop (cdr ls))))))))
  (equal?
    (with-interrupts-disabled
      (let ([ls (cons (weak-cons 'a 'b) (weak-cons 'c (cons 'd (weak-cons 'e #f))))])
        (call-with-port
          (open-file-output-port "testfile.ss" (file-options replace))
          (lambda (p) (fasl-write ls p))))
      (let ([ls (call-with-port (open-file-input-port "testfile.ss") fasl-read)])
        (list
          (equal? ls '((a . b) c d e . #f))
          (weak-pair? ls)
          (weak-pair? (car ls))
          (weak-pair? (cdr ls))
          (weak-pair? (cddr ls))
          (weak-pair? (cdddr ls)))))
    '(#t #f #t #t #f #t))
)

(mat clear-output-port ; test interactively
   (procedure? clear-output-port)
 )

(mat flush-output-port ; test interactively
   (procedure? flush-output-port)
 )

;;; section 6-3:

(mat format
   (equal? (format "abcde") "abcde")
   (equal? (format "~s ~a ~c ~~ ~%" "hi" "there" #\X)
           (string-append "\"hi\" there X ~ " (string #\newline)))
   (equal? (format "~s" car) "#<procedure car>")
   (equal? (format "~s" (lambda () #f)) "#<procedure>")
 )

(mat printf
   (let ([p (open-output-string)])
      (parameterize ([current-output-port p])
         (printf "~s:~s" 3 4))
      (equal? (get-output-string p) "3:4"))
 )

(mat fprintf
   (let ([p (open-output-string)])
      (fprintf p "~s.~s:~s" 'abc 345 "xyz")
      (equal? (get-output-string p) "abc.345:\"xyz\""))
 )

(mat cp1in-verify-format-warnings
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda () (import scheme) (format "~a~~~s" 5)))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda () (import scheme) (format "~a~a~a~s" 1 2 3 4 5 6)))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda () (import scheme) (format "~a~~~s" 5))))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda () (import scheme) (format "~a~a~a~s" 1 2 3 4 5 6))))))

   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda () (import scheme) (printf "abc~s")))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda () (import scheme) (printf "~%~abc~adef~ag~s~~~%" 1 2 3 4 5)))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda () (import scheme) (printf "abc~s"))))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda () (import scheme) (printf "~%~abc~adef~ag~s~~~%" 1 2 3 4 5))))))

   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda (p) (import scheme) (fprintf p "abc~s")))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(lambda (p) (import scheme) (fprintf p "~%~abc~adef~ag~s~~~%" 1 2 3 4 5)))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda (p) (import scheme) (fprintf p "abc~s"))))))
   (warning? (parameterize ([#%$suppress-primitive-inlining #f])
               (eval '(mat/cf (lambda (p) (import scheme) (fprintf p "~%~abc~adef~ag~s~~~%" 1 2 3 4 5))))))
)

(mat print-parameters
   (equal? (parameterize ([print-level 3])
              (format "~s" (let ([x (list 'a)]) (set-car! x x) x)))
           "((((...))))")
   (equal? (parameterize ([print-length 3])
              (format "~s" (let ([x (list 'a)]) (set-cdr! x x) x)))
           "(a a a ...)")
   (equal? (parameterize ([print-graph #t])
              (format "~s" (let ([x (list 'a)]) (set-car! x x) x)))
           "#0=(#0#)")
   (equal? (parameterize ([print-graph #t])
              (format "~s" (let ([x (list 'a)]) (set-cdr! x x) x)))
           "#0=(a . #0#)")
   (equal? (parameterize ([print-graph #t])
              (format "~s" (let ([x (list 'a)] [y (list 'b)])
                              (list x y y x))))
           "(#0=(a) #1=(b) #1# #0#)")
   (equal? (parameterize ([print-graph #t])
              (format "~s" (let ([x (list 'a)] [y (list 'b)])
                              (vector x y y x))))
           "#(#0=(a) #1=(b) #1# #0#)")
   (equal? (parameterize ([print-graph #t])
              (format "~s" '(#2# #2=#{a b})))
           "(#0=#{a b} #0#)")
   (error? (guard (c [(and (warning? c) (format-condition? c))
                      (apply errorf (condition-who c) (condition-message c) (condition-irritants c))])
             (format "~s"
               (let ([x (list '*)])
                 (set-car! x x)
                 (set-cdr! x x)
                 x))))
   (equal? (parameterize ([print-vector-length #f])
              (format "~s ~s" '#5(1 2 3) '#8vfx(5 7 9 8 8 9 -1)))
           "#(1 2 3 3 3) #vfx(5 7 9 8 8 9 -1 -1)")
   (equal? (parameterize ([print-vector-length #t])
              (format "~s ~s" '#(1 2 3 3 3) '#vfx(5 7 9 8 8 9 -1 -1)))
           "#5(1 2 3) #8vfx(5 7 9 8 8 9 -1)")
   (equal? (parameterize ([print-vector-length #f])
              (format "~a ~a" '#5(1 2 3) '#8vfx(5 7 9 8 8 9 -1)))
           "#(1 2 3 3 3) #vfx(5 7 9 8 8 9 -1 -1)")
   (equal? (parameterize ([print-vector-length #t])
              (format "~a ~a" '#(1 2 3 3 3) '#vfx(5 7 9 8 8 9 -1 -1)))
           "#(1 2 3 3 3) #vfx(5 7 9 8 8 9 -1 -1)")
   (equal? (parameterize ([print-vector-length #f])
             (with-output-to-string
               (lambda ()
                 (pretty-print '#5(1 2 3))
                 (pretty-print '#8vfx(5 7 9 8 8 9 -1)))))
           "#(1 2 3 3 3)\n#vfx(5 7 9 8 8 9 -1 -1)\n")
   (equal? (parameterize ([print-vector-length #t])
             (with-output-to-string
               (lambda ()
                 (pretty-print '#(1 2 3 3 3))
                 (pretty-print '#vfx(5 7 9 8 8 9 -1 -1)))))
           "#5(1 2 3)\n#8vfx(5 7 9 8 8 9 -1)\n")
   (equal? (parameterize ([print-extended-identifiers #f])
             (with-output-to-string
               (lambda ()
                 (for-each pretty-print '(1+ +++ .. |12| xyz aBc |-155| |.5e7| |.5e|)))))
           "\\x31;+\n\\x2B;++\n\\x2E;.\n\\x31;2\nxyz\naBc\n\\x2D;155\n\\x2E;5e7\n\\x2E;5e\n")
   (equal? (parameterize ([print-extended-identifiers #t])
             (with-output-to-string
               (lambda ()
                 (for-each pretty-print '(1+ +++ .. |12| xyz aBc |-155| |.5e7| |.5e|)))))
           "1+\n+++\n..\n\\x31;2\nxyz\naBc\n\\x2D;155\n\\x2E;5e7\n.5e\n")
   (equal? (parameterize ([print-gensym #f])
              (format "~s" '(#3# #3=#{g0 fool})))
           "(g0 g0)")
   (equal? (parameterize ([print-graph #t] [print-gensym #f])
              (format "~s" '(#4# #4=#{g0 fool})))
           "(#0=g0 #0#)")
   (equal? (parameterize ([print-gensym 'pretty])
              (format "~s" '(#5# #5=#{g0 fool})))
           "(#:g0 #:g0)")
   (equal? (parameterize ([print-graph #t] [print-gensym 'pretty])
              (format "~s" '(#6# #6=#{g0 fool})))
           "(#0=#:g0 #0#)")
   (equal? (parameterize ([print-gensym 'pretty])
              (format "~s" '(#7# #7=#:g0)))
           "(#:g0 #:g0)")
   (let ([g (gensym "x")])
     (parameterize ([print-gensym 'pretty/suffix])
       (equal? (format "~s" g) (format "~s" g))))
   (do ([i 100 (fx- i 1)])
       ((fx= i 0) #t)
     (let ([g (gensym "x")])
       (unless (< (string-length (parameterize ([print-gensym 'pretty/suffix])
                                   (format "~s" g)))
                  (string-length (parameterize ([print-gensym #t])
                                   (format "~s" g))))
         (error #f "failed"))))
   (let ([g (gensym "x")])
     (let ([x (with-input-from-string
                (parameterize ([print-gensym 'pretty/suffix])
                  (format "~s" g))
                read)])
       (and (symbol? x) (not (gensym? x)))))
   (equal? (parameterize ([print-gensym 'pretty/suffix])
             (format "~s" '#{g0 cfdhkxfnlo6opm0x-c}))
           "g0.cfdhkxfnlo6opm0x-c")
   (equal? (parameterize ([print-graph #t] [print-gensym 'pretty])
              (format "~s" '(#8# #8=#:g0)))
           "(#0=#:g0 #0#)")
   (equal? (parameterize ([print-brackets #t])
              (let ([p (open-output-string)])
                 (pretty-print '(let ((x 3)) x) p)
                 (get-output-string p)))
           (format "~a~%" "(let ([x 3]) x)"))
   (equal? (parameterize ([print-brackets #f])
              (let ([p (open-output-string)])
                 (pretty-print '(let ((x 3)) x) p)
                 (get-output-string p)))
           (format "~a~%" "(let ((x 3)) x)"))
   (equal? (parameterize ([case-sensitive #t])
              (format "~s" (string->symbol "AbcDEfg")))
           "AbcDEfg")
   (equal? (format "~s" (read (open-input-string "abCdEfG")))
           "abCdEfG")
   (equal? (parameterize ([case-sensitive #f])
              (format "~s" (read (open-input-string "abCdEfG"))))
           "abcdefg")
   (equal? (parameterize ([print-radix 36])
              (format "~s" 35))
           "#36rZ")
   (equal? (parameterize ([print-radix 36])
              (format "~a" 35))
           "Z")
)

(mat general-port
   (<= (port-input-index (console-input-port))
       (port-input-size (console-input-port))
       (string-length (port-input-buffer (console-input-port))))
   (<= (port-input-count (console-input-port))
       (string-length (port-input-buffer (console-input-port))))
   (<= (port-output-index (console-output-port))
       (port-output-size (console-output-port))
       (string-length (port-output-buffer (console-output-port))))
   (<= (port-output-count (console-output-port))
       (string-length (port-output-buffer (console-output-port))))
   (equal?
     (let ([sip (open-string-input-port "hello")])
       (let ([n1 (port-input-count sip)])
         (read-char sip)
         (list n1 (port-input-count sip))))
     '(5 4))
   (equal?
     (let ([op (make-output-port (lambda args (error #f "oops")) (make-string 10))])
       (let ([n1 (port-output-count op)])
         (display "hey!" op)
         (list n1 (port-output-count op))))
     '(10 6))
   (let ()
      (define make-two-way-port
         ; no local buffering
         ; close-port passed through
         (lambda (ip op)
            (define handler
               (lambda (msg . args)
                  (record-case (cons msg args)
                     [block-read (p s n) (block-read ip s n)]
                     [block-write (p s n) (block-write op s n)]
                     [char-ready? (p) (char-ready? ip)]
                     [clear-input-port (p) (clear-input-port ip)]
                     [clear-output-port (p) (clear-output-port op)]
                     [close-port (p)
                      (close-port ip)
                      (close-port op)
                      (mark-port-closed! p)]
;                     [file-length (p) #f]
                     [file-position (p . pos)
                      (if (null? pos)
                          (most-negative-fixnum)
                          (errorf 'two-way-port "cannot reposition"))]
                     [flush-output-port (p) (flush-output-port op)]
                     [peek-char (p) (peek-char ip)]
                     [port-name (p) "two-way port"]
                     [read-char (p) (read-char ip)]
                     [unread-char (c p) (unread-char c ip)]
                     [write-char (c p) (write-char c op)]
                     [else (errorf 'two-way-port "operation ~s not handled"
                                  msg)])))
            (make-input/output-port handler "" "")))
      (let ([sip (open-input-string "far out")]
            [sop (open-output-string)])
         (let ([p1 (make-two-way-port sip sop)])
            (and (port? p1)
                 (begin (write (read p1) p1)
                        (string=? (get-output-string sop) "far"))
                 (char-ready? p1)
                 (char=? (read-char p1) #\space)
                 (char=? (read-char p1) #\o)
                 (begin (unread-char #\o p1)
                        (char=? (read-char p1) #\o))
                ; can't count on clear-output-port doing anything for
                ; string output ports, so next two checks are bogus
                 #;(begin (write-char #\a p1)
                        (clear-output-port p1)
                        (string=? (get-output-string sop) ""))
                 (begin
                   (file-position sip (file-length sip))
                   (char-ready? p1))
                 (eof-object? (peek-char p1))
                ; make sure these don't error out
                 (eq? (clear-input-port p1) (void))
                 (eq? (clear-output-port p1) (void))
                 (begin (close-port p1) (port-closed? p1))
                 (port-closed? sip)
                 (port-closed? sop)))))
   (let ()
      (define make-broadcast-port
         ; local buffering
         ; closed-port not passed through
         ; critical sections used where necessary to protect against interrupts
         ; uses block-write to dump buffers to subordinate ports
         ; check cltl2 to see what it says about local buffering,
         ;    and about passing through flush, clear, and close
         ; size set so that buffer always has room for character to be written,
         ;   allowing buffer to be flushed as soon as it becomes full
         (lambda ports
            (define handler
               (lambda (msg . args)
                  (record-case (cons msg args)
;                     [block-read (p s n) #f]
                     [block-write (p s n)
                      (unless (null? ports)
                         (with-interrupts-disabled
                            (flush-output-port p)
                            (for-each (lambda (p) (block-write p s n))
                                      ports)))]
;                     [char-ready? (p) (char-ready? ip)]
;                     [clear-input-port (p) (clear-input-port ip)]
                     [clear-output-port (p) (set-port-output-index! p 0)]
                     [close-port (p)
                      (set-port-output-size! p 0)
                      (mark-port-closed! p)]
;                     [file-length (p) #f]
                     [file-position (p . pos)
                      (if (null? pos)
                          (most-negative-fixnum)
                          (errorf 'broadcast-port "cannot reposition"))]
                     [flush-output-port (p)
                      (with-interrupts-disabled
                         (unless (null? ports)
                            (let ([b (port-output-buffer p)]
                                  [i (port-output-index p)])
                               (for-each (lambda (p) (block-write p b i))
                                         ports)))
                         (set-port-output-index! p 0))]
;                     [peek-char (p) (peek-char ip)]
                     [port-name (p) "broadcast port"]
;                     [read-char (p) (read-char ip)]
;                     [unread-char (c p) (unread-char c ip)]
                     [write-char (c p)
                      (with-interrupts-disabled
                         (unless (null? ports)
                            (let ([b (port-output-buffer p)]
                                  [i (port-output-index p)])
                           ; could check here to be sure that we really need
                           ; to flush
                               (string-set! b i c)
                               (for-each (lambda (p)
                                            (block-write p b (fx+ i 1)))
                                         ports)))
                         (set-port-output-index! p 0))]
                     [else (errorf 'broadcast-port "operation ~s not handled"
                                  msg)])))
            (let ([len 1024])
               (let ([p (make-output-port handler (make-string len))])
                  (set-port-output-size! p (fx- len 1))
                  p))))
      (let ([p (make-broadcast-port)])
         (and (port? p)
              (let ([x (make-string 1000 #\a)])
                 (let loop ([i 1000])
                    (if (fx= i 0)
                        (fx<= (port-output-index p)
                              (port-output-size p)
                              (string-length (port-output-buffer p)))
                        (begin (display x p)
                               (loop (fx- i 1))))))
              (begin (close-port p) (port-closed? p))))
      (let ([sop (open-output-string)])
         (let ([p (make-broadcast-port sop sop)])
            (and (port? p)
                 (let ([x "abcde"])
                    (display x p)
                    (and (string=? (get-output-string sop) "")
                         (begin (flush-output-port p)
                                (string=? (get-output-string sop)
                                          (string-append x x)))))
                 (begin (close-output-port p) (port-closed? p))))))

   (let ()
      (define make-transcript-port
         ; local buffering; run into problems with unread-char and
         ;     clear-output-port otherwise
         ; close-port passed through to tp only
         (lambda (ip op tp)
            (define handler
               (lambda (msg . args)
                  (record-case (cons msg args)
                     [block-read (p str cnt)
                      (with-interrupts-disabled
                         (let ([b (port-input-buffer p)]
                               [i (port-input-index p)]
                               [s (port-input-size p)])
                            (if (< i s)
                                (let ([cnt (fxmin cnt (fx- s i))])
                                   (do ([i i (fx+ i 1)]
                                        [j 0 (fx+ j 1)])
                                       ((fx= j cnt)
                                        (set-port-input-index! p i)
                                        cnt)
                                       (string-set! str j (string-ref b i))))
                                (let ([cnt (block-read ip str cnt)])
                                   (unless (eof-object? cnt)
                                      (block-write tp str cnt))
                                   cnt))))]
                     [char-ready? (p)
                      (or (< (port-input-index p) (port-input-size p))
                          (char-ready? ip))]
                     [clear-input-port (p)
                      ; set size to zero rather than index to size
                      ; in order to invalidate unread-char
                      (set-port-input-size! p 0)]
                     [clear-output-port (p) (set-port-output-index! p 0)]
                     [close-port (p)
                      (flush-output-port p)
                      (close-port tp)
                      (set-port-output-size! p 0)
                      (set-port-input-size! p 0)
                      (mark-port-closed! p)]
;                     [file-length (p) #f]
                     [file-position (p . pos)
                      (if (null? pos)
                          (most-negative-fixnum)
                          (errorf 'transcript-port "cannot reposition"))]
                     [flush-output-port (p)
                      (with-interrupts-disabled
                         (let ([b (port-output-buffer p)]
                               [i (port-output-index p)])
                            (block-write op b i)
                            (block-write tp b i)
                            (set-port-output-index! p 0)
                            (flush-output-port op)
                            (flush-output-port tp)))]
                     [peek-char (p)
                      (with-interrupts-disabled
                         (let ([b (port-input-buffer p)]
                               [i (port-input-index p)]
                               [s (port-input-size p)])
                            (if (fx< i s)
                                (string-ref b i)
                                (begin (flush-output-port p)
                                       (let ([s (block-read ip b)])
                                          (if (eof-object? s)
                                              s
                                              (begin (block-write tp b s)
                                                     (set-port-input-size! p s)
                                                     (string-ref b 0))))))))]
                     [port-name (p) "transcript"]
                     [read-char (p)
                      (with-interrupts-disabled
                         (let ([c (peek-char p)])
                            (unless (eof-object? c)
                               (set-port-input-index! p
                                  (fx+ (port-input-index p) 1)))
                            c))]
                     [unread-char (c p)
                      (with-interrupts-disabled
                         (let ([b (port-input-buffer p)]
                               [i (port-input-index p)]
                               [s (port-input-size p)])
                            (when (fx= i 0)
                               (errorf 'unread-char
                                      "tried to unread too far on ~s"
                                      p))
                            (set-port-input-index! p (fx- i 1))
                            ; following could be skipped; supposed to be
                            ; same character
                            (string-set! b (fx- i 1) c)))]
                     [write-char (c p)
                      (with-interrupts-disabled
                         (let ([b (port-output-buffer p)]
                               [i (port-output-index p)]
                               [s (port-output-size p)])
                            (string-set! b i c)
                           ; could check here to be sure that we really need
                           ; to flush
                            (block-write op b (fx+ i 1))
                            (block-write tp b (fx+ i 1))
                            (set-port-output-index! p 0)))]
                     [block-write (p str cnt)
                      (with-interrupts-disabled
                        (let ([b (port-output-buffer p)]
                              [i (port-output-index p)])
                         ; flush buffered data
                          (when (fx> i 0)
                            (block-write op b i)
                            (block-write tp b i))
                         ; write new data
                          (block-write op str cnt)
                          (block-write tp str cnt)
                          (set-port-output-index! p 0)))]
                     [else (errorf 'transcript-port "operation ~s not handled"
                                  msg)])))
            (let ([ib (make-string 100)] [ob (make-string 100)])
               (let ([p (make-input/output-port handler ib ob)])
                 (if (char-ready? ip)
                    ; kludge so that old input doesn't show up after later
                    ; output (e.g., input newline after output prompt)
                     (let ((n (block-read ip ib (string-length ib))))
                       (if (eof-object? n)
                           (set-port-input-size! p 0)
                           (set-port-input-size! p n)))
                     (set-port-input-size! p 0))
                  (set-port-output-size! p (fx- (string-length ob) 1))
                  p))))
;      (define-record tp-frame (cip cop tp))
;      (define tp-stack '())
;      (define transcript-on
;        (lambda (fn)
;          (with-interrupts-disabled
;            (let ((cip (console-input-port))
;                  (cop (console-output-port)))
;              (let ((tp (make-transcript-port cip cop
;                          (open-output-file fn 'replace))))
;                (set! tp-stack (cons (make-tp-frame cip cop tp) tp-stack))
;               (console-output-port tp)
;               (console-input-port tp)
;               (when (eq? (current-input-port) cip)
;                  (current-input-port tp))
;               (when (eq? (current-output-port) cop)
;                  (current-output-port tp)))))))
;      (define transcript-off
;        (lambda ()
;          (with-interrupts-disabled
;            (when (null? tp-stack) (errorf 'transcript-off "no transcript running"))
;            (let ((frame (car tp-stack)))
;              (let ((cip (tp-frame-cip frame))
;                    (cop (tp-frame-cop frame))
;                    (tp (tp-frame-tp frame)))
;                (console-input-port cip)
;                (console-output-port cop)
;                (when (eq? (current-input-port) tp) (current-input-port cip))
;                (when (eq? (current-output-port) tp) (current-output-port cop))
;                (set! tp-stack (cdr tp-stack))
;                (close-port tp))))))
      (let ([ip (open-input-string (format "2"))]
            [op (open-output-string)]
            [tp (open-output-string)])
         (let ([p (make-transcript-port ip op tp)])
            (and (begin (display "1" p) (eq? (read p) 2))
                 (begin (display "3" p)
                        (flush-output-port p)
                        (and (string=? (get-output-string op) "13")
                            ; 2 doesn't show up since we scan past available
                            ; input (see "kludge" above)
                             (string=? (get-output-string tp) "13")))
                 (begin (close-port p)
                        (and (port-closed? p) (port-closed? tp)))))))
 )

(mat port-handler
   (begin (set! ph (port-handler (current-output-port)))
          (procedure? ph))
   (string? (ph 'port-name (current-output-port)))
   (error? (ph))
   (error? (ph 'foo))
   (error? (ph 'foo (current-output-port)))
   (error? (ph 'read-char))
   (error? (ph 'write-char))
   (error? (ph 'write-char 3))
   (error? (ph 'write-char (current-input-port)))
   (error? (ph 'write-char 'a (current-output-port)))
   (error? (ph 'write-char #\a 'a))
   (error? (ph 'write-char #\a (open-input-string "hello")))
   (error? (ph 'write-char #\a (current-output-port) 'a))
   (boolean? (ph 'char-ready? (current-input-port)))
 )

(mat char-name
  (eqv? (char-name 'space) #\space)
  (eqv? (char-name #\space) 'space)
  (eqv? (char-name 'tab) #\tab)
  (eqv? (char-name #\tab) 'tab)
  (eqv? (char-name 'return) #\return)
  (eqv? (char-name #\return) 'return)
  (eqv? (char-name 'page) #\page)
  (eqv? (char-name #\page) 'page)
  (eqv? (char-name 'linefeed) #\linefeed)
  (eqv? (char-name #\linefeed) 'newline)
  (eqv? (char-name 'newline) #\newline)
  (eqv? (char-name #\newline) 'newline)
  (eqv? (char-name #\backspace) 'backspace)
  (eqv? (char-name 'backspace) #\backspace)
  (eqv? (char-name #\rubout) 'delete)
  (eqv? (char-name 'rubout) #\rubout)
  (eqv? (char-name #\nul) 'nul)
  (eqv? (char-name 'nul) #\nul)
  (eqv? (char-name 'foo) #f)
  (eqv? (char-name 'delete) #\delete)
  (eqv? (char-name #\delete) 'delete)
  (eqv? (char-name 'vtab) #\vtab)
  (eqv? (char-name #\vtab) 'vtab)
  (eqv? (char-name 'alarm) #\alarm)
  (eqv? (char-name #\alarm) 'alarm)
  (eqv? (char-name 'esc) #\esc)
  (eqv? (char-name #\esc) 'esc)
  (error? (read (open-input-string "#\\foo")))
  (and (eqv? (char-name 'foo #\003) (void))
       (eqv? (char-name 'foo) #\003)
       (eqv? (char-name #\003) 'foo)
       (eqv? (read (open-input-string "#\\foo")) #\003))
  (equal?
    (begin
      (char-name 'foo #f)
      (list (char-name 'foo) (char-name #\003)))
    '(#f #f))
  (error? (read (open-input-string "#\\new\\line")))
  (error? (read (open-input-string "#\\new\\x6c;ine")))
 )

(mat string-escapes
  (eqv? (string-ref "ab\b" 2) #\backspace)
  (eqv? (string-ref "\n" 0) #\newline)
  (eqv? (string-ref "a\fb" 1) #\page)
  (eqv? (string-ref "ab\r" 2) #\return)
  (eqv? (string-ref "\t" 0) #\tab)
  (eqv? (string-ref "\a\v" 0) #\bel)
  (eqv? (string-ref "\a\v" 1) #\vt)
  (eqv? (string-ref "\000" 0) #\nul)
  (eqv? (string-ref "\x00;" 0) #\nul)
  (eqv? (string-ref "a\x20;b" 1) #\space)
  (eqv? (string-ref "\\\"\'" 0) #\\)
  (eqv? (string-ref "\\\"\'" 1) #\")
  (eqv? (string-ref "\\\"\'" 2) #\')
  (= (char->integer (string-ref "a\012" 1)) #o12 10)
  (= (char->integer (string-ref "a\015" 1)) #o15 13)
  (= (char->integer (string-ref "a\177" 1)) #o177 127)
  (= (char->integer (string-ref "a\377" 1)) #o377 255)
  (error? (read (open-input-string "\"ab\\\"")))
  (error? (read (open-input-string "\"ab\\0\"")))
  (error? (read (open-input-string "\"ab\\01\"")))
  (error? (read (open-input-string "\"ab\\*\"")))
  (error? (read (open-input-string "\"ab\\x\"")))
  (error? (read (open-input-string "\"ab\\x*\"")))
  (error? (read (open-input-string "\"ab\\xg\"")))
  (equal? (format "~s" "\bab\nc\f\rd\t\v\a") "\"\\bab\\nc\\f\\rd\\t\\v\\a\"")
 )

(mat read-token
  (let ([ip (open-input-string "(cons 33 #;hello \"rot\")")])
    (and (let-values ([vals (read-token ip)])
           (equal? vals '(lparen #f 0 1)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(atomic cons 1 5)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(atomic 33 6 8)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(quote datum-comment 9 11)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(atomic hello 11 16)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(atomic "rot" 17 22)))
         (let-values ([vals (read-token ip)])
           (equal? vals '(rparen #f 22 23)))))
  (let ()
    (define with-input-from-string
      (lambda (s p)
        (parameterize ([current-input-port (open-input-string s)])
          (p))))
    (with-input-from-string "\n#17#\n"
      (lambda ()
        (let-values ([vals (read-token)])
          (equal? vals '(insert 17 1 5))))))
  (let ()
    (with-output-to-file "testfile.ss"
      (lambda () (display "\n#eat\n"))
      'replace)
    #t)
  (error?
    (let* ([ip (open-file-input-port "testfile.ss")]
           [sfd (make-source-file-descriptor "testfile.ss" ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (dynamic-wind
        void
        (lambda () (read-token ip sfd 0))
        (lambda () (close-input-port ip)))))
  (let ()
    (with-output-to-file "testfile.ss"
      (lambda () (display "\neat\n"))
      'replace)
    #t)
  (equal?
    (let-values ([vals (let* ([ip (open-file-input-port "testfile.ss")]
                              [sfd (make-source-file-descriptor "testfile.ss" ip #t)]
                              [ip (transcoded-port ip (native-transcoder))])
                         (dynamic-wind
                           void
                           (lambda () (read-token ip sfd 0))
                           (lambda () (close-input-port ip))))])
      vals)
    '(atomic eat 1 4))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#t" read-token)) list)
    '(atomic #t 0 2))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#true" read-token)) list)
    '(atomic #t 0 5))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#True" read-token)) list)
    '(atomic #t 0 5))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#TRUE" read-token)) list)
    '(atomic #t 0 5))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#f" read-token)) list)
    '(atomic #f 0 2))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#false" read-token)) list)
    '(atomic #f 0 6))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#False" read-token)) list)
    '(atomic #f 0 6))
  (equal?
    (call-with-values (lambda () (with-input-from-string "#FALSE" read-token)) list)
    '(atomic #f 0 6))
 )

(define read-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (load "testfile.ss" values)
    #t))
(define load-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (load "testfile.ss")
    #t))
(define compile-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (compile-file "testfile.ss")
    (load "testfile.so")
    #t))

(define-syntax xmat
  (syntax-rules ()
    [(_ string ...)
     (begin
       (mat read-test (error? (read-test string)) ...)
       (mat load-test (error? (load-test string)) ...)
       (mat compile-test (error? (compile-test string)) ...))]))

(begin (define-record f800 (a b)) (record-reader "zinjanthropus" (type-descriptor f800)))
(begin (define-record $acyclic ((immutable notme))) (record-reader '$acyclic (type-descriptor $acyclic)))

(xmat
  "; Test error \"fasl object created by different release\"\n; This one is the list (a b c d) created by version 5.9b\n\n#@\x2;\x4;\x0;\x0;\x0;5.9b\x0;\x4;\x0;\x0;\x0;\x2;\x1;\x0;\x0;\x0;a\x2;\x1;\x0;\x0;\x0;b\x2;\x1;\x0;\x0;\x0;c\x2;\x1;\x0;\x0;\x0;d\f&\x0;\x0;\x0;\n"
  )

(xmat
  "; Test error \"bracketed list terminated by close parenthesis\"\n\n(define nil '[))\n\n"
  "; Test error \"bracketed list terminated by close parenthesis\"\n\n(cond [(foobar) 'baz) [else 'ok])\n\n"
  "; Test error \"bracketed list terminated by close parenthesis\"\n\n(define pair '[a . b))\n\n"
  "; Test error \"duplicate mark #~s= seen\"\n\n(#327=(a b c #327=d) #327#)\n\n"
  "; Test error \"expected close brace terminating gensym syntax\"\n\n(define #{foo |bar|\n  (lambda (zap doodle)\n    zap))\n\n"
  "; Test error \"expected close brace terminating gensym syntax\"\n\n(define foo\n  (lambda (#{foo |bar| none)\n    'quack))\n\n"
  "; Test error \"expected one item after dot (.)\"\n\n(define foo\n  (lambda (a b . )\n    'zapp))\n\n"
  "; Test error \"expected one item after dot (.)\"\n\n(define foo\n  (lambda [a b . ]\n    'zapp))\n\n"
  "; Test error \"invalid character #\\\\~a~a~a\"\n\n(memv #\\401 (string->list \"abcd\"))\n\n"
  "; Test error \"invalid character #\\\\~a~a\"\n\n(make-list 25 (make-string 100 #\\37d))\n"
  "; Test error \"invalid character name\"\n\n(memv #\\bugsbunny (string->list \"looneytunes\"))\n"
  "; Test error \"invalid character name\"\n\n(memv #\\new (string->list \"deal\"))\n"
  "; Test error \"invalid character name\"\n\n(memv #\\Space (string->list \"no deal\"))\n"
  "; Test error \"invalid character name\"\n\n(memv #\\SPACE (string->list \"no deal\"))\n"
  "; Test error \"invalid number syntax\"\n\n(list #e23q3 'a 'b 'c)\n\n"
  "; Test error \"invalid number syntax\"\n\n(list #e3_4i 'a 'b 'c)\n\n"
  "; Test error \"invalid number syntax\"\n\n(list #e3+)"
  "; Test error \"invalid number syntax\"\n\n(sqrt #36r1+1i)\n\n"
  "; Test error \"invalid number syntax\"\n\n(sqrt #36r1+i)\n\n"
  "; Test error \"invalid number syntax\"\n\n(sqrt #36r1+1i)\n\n"
  "; Test error \"invalid number syntax\"\n\n(sqrt #36r1+i)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt 1/0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt 1/0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt 1#/0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt 1##/0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1/0#)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1/0###)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1/0###)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1/0###)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1/0###)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e+inf.0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e-inf.0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e+nan.0)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e0/0e20)\n\n"
  "; Test error \"cannot represent\"\n\n(sqrt #e1@1)\n\n"
  "; Test error \"invalid number syntax\"\n\n(sqrt #e+nan.5)\n\n"
  "; Test error \"invalid sharp-sign prefix #~c\"\n\n(if #T #N #T)\n"
  "; Test error \"invalid sharp-sign prefix ~a~a\"\n\n(if (optimize-til-it-hurts?) (#7%super-fast+ 1 2) (+ 1 2))\n"
  "; Test error \"invalid sharp-sign prefix ~a~a\"\n\n(when #2_3_4 'huh?)\n"
  "; Test error \"invalid string character \\\\~c~c~c\"\n\n  (define s \"james stock \\707!\")\n"
  "; Test error \"invalid string character \\\\~c~c\"\n\n\"=tofu\\07gnorefsefawd2slivne\"\n\n"
  "; Test error \"invalid string character \\\\~c\"\n\n\"I need \\3d glasses\"\n"
  "; Test error \"invalid string character\"\n\n(pretty-print \"this is \\xa fine mess\")\n"
  "; Test error \"invalid string character\"\n\n(pretty-print \"this is \\x\")\n"
  "; Test error \"invalid string character\"\n\n(pretty-print \"this is \\xGreat news!\")\n"
  "; Test error \"invalid string character \\\\~c\"\n\n\"status \\quo\"\n"
  "; Test error \"invalid syntax #!~s\"\n\n(when #!whuppo! 1 2 3)\n\n"
  "; Test error \"invalid syntax #!~s\"\n\n(when #!eo 1 2 3)\n\n"
  "; Test error \"invalid syntax #v~s\"\n\n(list #vxx())\n"
  "; Test error \"invalid syntax #~v,'0dv~s\"\n\n(list #073vxx())\n"
  "; Test error \"invalid syntax #v~s\"\n\n(list #vf())\n"
  "; Test error \"invalid syntax #~v,'0dv~s\"\n\n(list #073vf())\n"
  "; Test error \"invalid syntax #v~s\"\n\n(list #vfx[])\n"
  "; Test error \"invalid syntax #~v,'0dv~s\"\n\n(list #073vfx[])\n"
  "; Test error \"invalid vector length\"\n\n(vector-length #999999999999999999999999999999(a b c))\n\n"
  "; Test error \"invalid fxvector length\"\n\n(fxvector-length #999999999999999999999999999999vfx(1 2 3))\n\n"
  "; Test error \"invalid bytevector length\"\n\n(bytevector-length #999999999999999999999999999999vu8(1 2 3))\n\n"
  "; Test error \"mark #~s= missing\"\n\n'(what about this?) ; separate top-level S-expression, so ok.\n\n(begin\n  (reverse '(a b . #77#))\n  (cons 1 2))"
  "; Test error \"more than one item found after dot (.)\"\n\n(define foo\n  (lambda (able baker . charlie delta epsilon)\n    'wow))\n\n"
  "; Test error \"more than one item found after dot (.)\"\n\n(define foo\n  (lambda [able baker . charlie delta epsilon]\n    'wow))\n\n"
  "; Test error \"non-symbol found after #[\"\n\n(pretty-print '#[(a \"b c\" #\\d) 1 2 3])\n"
  "; Test error \"outdated object file format\"\n\n\"What is\"   #3q\n'(1 2 3)\n\n"
  "; Test error \"parenthesized list terminated by close bracket\"\n\n(define nil '(])\n\n"
  "; Test error \"parenthesized list terminated by close bracket\"\n\n(cond [(foobar) 'baz] (else 'ok])\n\n"
  "; Test error \"parenthesized list terminated by close bracket\"\n\n(define pair '(a . b])\n\n"
  "; Test error \"too many vector elements supplied\"\n\n(pretty-print '#3(one two three four five six seven))\n"
  "; Test error \"non-fixnum found in fxvector\"\n\n(pretty-print '#vfx(1 2.0 3 4))\n"
  "; Test error \"non-fixnum found in fxvector\"\n\n(pretty-print '#3vfx(1 2.0 3 4))\n"
  "; Test error \"too many fxvector elements supplied\"\n\n(pretty-print '#3vfx(1 2 3 4))\n"
  "; Test error \"invalid value 2.0 found in bytevector\"\n\n(pretty-print '#vu8(1 2.0 3 4))\n"
  "; Test error \"invalid value -1 found in bytevector\"\n\n(pretty-print '#3vu8(1 -1 3 4))\n"
  "; Test error \"invalid value #f found in bytevector\"\n\n#vu8(1 2 #f\n"
  "; Test error \"invalid value #t found in bytevector\"\n\n#vu8(1 2 #t\n"
  "; Test error \"invalid value \"foo\" found in bytevector\"\n\n#vu8(1 2 \"foo\")\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 (\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #(\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #7(\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #4=\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #5#\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 [\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #[\n"
  "; Test error \"invalid value { found in bytevector\"\n\n#vu8(1 2 {\n"
  "; Test error \"invalid value } found in bytevector\"\n\n#vu8(1 2 }\n"
  "; Test error \"invalid value 3.4 found in bytevector\"\n\n#vu8(1 2 3.4\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 '\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 `\n"
  "; Test error \"invalid value - found in bytevector\"\n\n#vu8(1 2 -\n"
  "; Test error \"invalid value + found in bytevector\"\n\n#vu8(1 2 +\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 .\n"
  "; Test error \"invalid value .. found in bytevector\"\n\n#vu8(1 2 ..)\n"
  "; Test error \"invalid value ... found in bytevector\"\n\n#vu8(1 2 ...)\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 ,)\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #,@)\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #@)\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #vfx(3\n"
  "; Test error \"non-octet found in bytevector\"\n\n#vu8(1 2 #vu8(3\n"
  "; Test error \"too many bytevector elements supplied\"\n\n(pretty-print '#3vu8(1 2 3 4))\n"
  "; Test error \"too few fields supplied for record ~s\"\n\n(define x '#[zinjanthropus 3])"
  "; Test error \"too many fields supplied for record ~s\"\n\n(define x '#[zinjanthropus 3 4 5])"
  "; Test error \"unexpected close bracket\"\n\n1 2 3 ]\n"
  "; Test error \"unexpected close parenthesis\"\n\n(define x 3))\n"
  "; Test error \"unexpected dot\"\n\n(lambda (x . . y) x)\n\n"
  "; Test error \"unexpected dot\"\n\n(lambda ( . y) y)\n\n"
  "; Test error \"unexpected dot\"\n\n(define x '(a . b . c))\n"
  "; Test error \"unexpected dot\"\n\n(define x '[a . b . c])\n"
  "; Test error \"unexpected end-of-file reading block comment\"\n(pretty-print\n #| foo\n   #| bar |#\n    baz \"pickle  ; not eof on string since we're in block comment"
  "; Test error \"unexpected end-of-file reading block comment\"\n(pretty-print\n #| foo\n   #"
  "; Test error \"unexpected end-of-file reading block comment\"\n(pretty-print\n #| foo\n   |"
  "; Test error \"unexpected end-of-file reading box\"\n\n   #& ; box is empty!\n"
  "; Test error \"unexpected end-of-file reading bracketed list\" (before first element)\n\n(lambda (x y z)\n  (cond\n    [\n\n   "
  "; Test error \"unexpected end-of-file reading bracketed list\"\n\n(lambda (x y z)\n  (cond\n    [(< x 1) y\n    [else z]\n\n\n"
  "; Test error \"unexpected end-of-file reading bracketed list\" (after dot)\n\n(car '[a b . c\n\n"
  "; Test error \"unexpected end-of-file reading bracketed list\" (after element after dot)\n\n(car '[a b . c\n\n"
  "; Test error \"unexpected end-of-file reading character\"\n#\\"
  "; Test error \"unexpected end-of-file reading character\"\n#\\new"
  "; Test error \"unexpected end-of-file reading character\"\n#\\02"
  "; Test error \"unexpected end-of-file reading boolean\"\n\n#tr"
  "; Test error \"unexpected end-of-file reading boolean\"\n\n#tru"
  "; Test error \"unexpected end-of-file reading boolean\"\n\n#fa"
  "; Test error \"unexpected end-of-file reading boolean\"\n\n#fal"
  "; Test error \"unexpected end-of-file reading boolean\"\n\n#fals"
  "; Test error \"unexpected end-of-file reading expression comment\"\n\n(define oops '#; ; that's all I've got!\n"
  "; Test error \"unexpected end-of-file reading gensym\"\n(pretty-print '#{"
  "; Test error \"unexpected end-of-file reading gensym\"\n(pretty-print '#{foo"
  "; Test error \"unexpected end-of-file reading gensym\"\n(pretty-print '#{foo |bar|"
  "; Test error \"unexpected end-of-file reading graph mark\"\n(define x '#1=\n"
  "; Test error \"unexpected end-of-file reading hash-bang syntax\"\n\n(list #!eo"
  "; Test error \"unexpected end-of-file reading #v syntax\"\n\"would this be cool?\" #v"
  "; Test error \"unexpected end-of-file reading #~v,'0dv syntax\"\n\"would this be cool?\" #01v"
  "; Test error \"unexpected end-of-file reading #v syntax\"\n\"would this be cool?\" #vf"
  "; Test error \"unexpected end-of-file reading #~v,'0dv syntax\"\n\"would this be cool?\" #01vf"
  "; Test error \"unexpected end-of-file reading #v syntax\"\n\"would this be cool?\" #vfx"
  "; Test error \"unexpected end-of-file reading #~v,'0dv syntax\"\n\"would this be cool?\" #01vfx"
  "; Test error \"unexpected end-of-file reading list\" (before first element) \n\n  (\n\n   "
  "; Test error \"unexpected end-of-file reading list\"\n\n(lambda (x y z\n  (cond\n    [(< x 1) y]\n    [else z]))\n\n"
  "; Test error \"unexpected end-of-file reading list\" (after dot)\n\n(car '(a b . \n\n"
  "; Test error \"unexpected end-of-file reading list\" (after element after dot)\n\n(car '(a b . c\n\n"
  "; Test error \"unexpected end-of-file reading # prefix\"\n\"would this be cool?\" #"
  "; Test error \"unexpected end-of-file reading # prefix\"\n\"would this be cool?\" #35"
  "; Test error \"unexpected end-of-file reading number\"\n\n(list #e3+"
  "; Test error \"unexpected end-of-file reading quote\"\n(define fido '   \n\n\n"
  "; Test error \"unexpected end-of-file reading quasiquote\"\n(define e `   \n"
  "; Test error \"unexpected end-of-file reading unquote\"\n(define e `(+ ,(* 2 3) ,   \n\n"
  "; Test error \"unexpected end-of-file reading unquote-splicing\"\n(define r (list 1 2 3))\n(set! r `(0 ,@   \n\n"
  "; Test error \"unexpected end-of-file reading quasisyntax\"\n(define e #`   \n"
  "; Test error \"unexpected end-of-file reading unsyntax\"\n(define e #`(+ #,(* 2 3) #,   \n\n"
  "; Test error \"unexpected end-of-file reading unsyntax-splicing\"\n(define r (list 1 2 3))\n(set! r #`(0 #,@   \n\n"
  "; Test error \"unexpected end-of-file reading record\"\n\n(define oops '#[   \n\n"
  "; Test error \"unexpected end-of-file reading record\"\n\n(define oops '#[$acyclic   \n\n"
  "; Test error \"unexpected end-of-file reading string\"\n\n(printf \"This is \\\"not\\\" what I meant)\n\n"
  "; Test error \"unexpected end-of-file reading string\"\n\n(define s \"hello \\"
  "; Test error \"unexpected end-of-file reading string\"\n\n(define s \"hello \\0"
  "; Test error \"unexpected end-of-file reading string\"\n\n(define s \"hello \\03"
  "; Test error \"unexpected end-of-file reading string\"\n\n(define s \"hello \\x"
  "; Test error \"unexpected end-of-file reading string\"\n\n(define s \"hello \\x2"
  "; Test error \"unexpected end-of-file reading string\"\n\n(list \"abc\\x3c3"
  "; Test error \"invalid code point value 2097152 in string hex escape\"\n\n(list \"abc\\x200000;\")"
  "; Test error \"invalid character q in string hex escape\"\n\n(list \"abc\\xq;\")"
  "; Test error \"invalid character \" in string hex escape\"\n\n(list \"abc\\x\")"
  "; Test error \"unexpected end-of-file reading symbol\"\n\n(list 'abc\\"
  "; Test error \"unexpected end-of-file reading symbol\"\n\n(cons '|froma\\|gerie\\ %dq|jl&"
  "; Test error \"unexpected end-of-file reading symbol\"\n(pretty-print\n #| foo\n   #| bar |#\n |#\n|# #| oops |#"
  "; Test error \"unexpected end-of-file reading symbol\"\n\n(list 'abc\\x"
  "; Test error \"unexpected end-of-file reading symbol\"\n\n(list 'abc\\x3c3"
  "; Test error \"invalid code point value 2097152 in symbol hex escape\"\n\n(list 'abc\\x200000;)"
  "; Test error \"invalid character q in symbol hex escape\"\n\n(list 'abc\\xq;)"
  "; Test error \"unexpected end-of-file reading vector\"\n\n  (define v '#(a b \n"
  "; Test error \"unexpected end-of-file reading vector\"\n\n  (define v '#35(a b \n"
  "; Test error \"unexpected end-of-file reading fxvector\"\n\n  (define v '#vfx(0 1 \n"
  "; Test error \"unexpected end-of-file reading fxvector\"\n\n  (define v '#35vfx(0 1 \n"
  "; Test error \"unexpected end-of-file reading bytevector\"\n\n  (define v '#vu8(0 1 \n"
  "; Test error \"unexpected end-of-file reading bytevector\"\n\n  (define v '#35vu8(0 1 \n"
  "; Test error \"unrecognized record name ~s\"\n#[zsunekunvliwndwalv 1 2 3 4]"
  "; Test error \"unresolvable cycle\"\n\n(define oops '#1=#[$acyclic #1#])\n"
  "; Test error \"open brace syntax not allowed in #!r6rs mode\"\n\n#!r6rs '{\n"
  "; Test error \"close brace syntax not allowed in #!r6rs mode\"\n\n#!r6rs '}\n"
  "; Test error \"#[...] record syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#[abc]\n"
  "; Test error \"#{...} gensym syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#{abc def}\n"
  "; Test error \"#& box syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#&box\n"
  "; Test error \"#% primitive syntax not allowed in #!r6rs mode\"\n\n#!r6rs #%car\n"
  "; Test error \"#: gensym syntax not allowed in #!r6rs mode\"\n\n#!r6rs #:g0\n"
  "; Test error \"#<n>(...) vector syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3(a b c)\n"
  "; Test error \"#<n>r number syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3r1201\n"
  "; Test error \"#<n># insert syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3#\n"
  "; Test error \"#<n>= mark syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3=\n"
  "; Test error \"#<n>% primitive syntax not allowed in #!r6rs mode\"\n\n#!r6rs #3%car\n"
  "; Test error \"octal character syntax not allowed in #!r6rs mode\"\n\n#!r6rs #\\010\n"
  "; Test error \"invalid delimiter 1 for character\"\n\n#\\0001\n"
  "; Test error \"delimiter { is not allowed in #!r6rs mode\"\n\n#!r6rs #\\0{\n"
  "; Test error \"invalid delimiter 2 for boolean\"\n\n#t2\n"
  "; Test error \"invalid delimiter 2 for boolean\"\n\n#true2\n"
  "; Test error \"invalid delimiter 3 for boolean\"\n\n#f3\n"
  "; Test error \"invalid delimiter 3 for boolean\"\n\n#false3\n"
  "; Test error \"invalid boolean\"\n\n#travis"
  "; Test error \"invalid boolean\"\n\n#FALSIFY"
  ;; NOTE: there's no "delimiter not allowed in #!r6rs mode" test for r7rs-style booleans because they are not r6rs!
  "; Test error \"#!eof syntax not allowed in #!r6rs mode\"\n\n#!r6rs #!eof\n"
  "; Test error \"#!bwp syntax not allowed in #!r6rs mode\"\n\n#!r6rs #!bwp\n"
  "; Test error \"#vfx(...) fxvector syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#vfx(1 2 3)\n"
  "; Test error \"#<n>vfx(...) fxvector syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3vfx(1 2 3)\n"
  "; Test error \"#<n>vu8(...) fxvector syntax not allowed in #!r6rs mode\"\n\n#!r6rs '#3vu8(1 2 3)\n"
  "; Test error \"octal string-character syntax not allowed in #!r6rs mode\"\n\n#!r6rs \"a\\010b\"\n"
  "; Test error \"back-slash symbol escape syntax not allowed in #!r6rs mode\"\n\n#!r6rs ab\\ cd\n"
  "; Test error \"|...| symbol escape syntax not allowed in #!r6rs mode\"\n\n#!r6rs |ab  cd|\n"
  "; Test error \"@abc symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs @abc\n"
  "; Test error \"123a symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs 123a\n"
  "; Test error \"123# number syntax not allowed in #!r6rs mode\"\n\n#!r6rs 123#\n"
  "; Test error \"#x1/2e2 number syntax not allowed in #!r6rs mode\"\n\n#!r6rs 1/2e2\n"
  "; Test error \"#x.3 number syntax not allowed in #!r6rs mode\"\n\n#!r6rs #x.3\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #true\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #True\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #TRUE\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #false\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #False\n"
  "; Test error \"alternative boolean syntax not allowed in #!r6rs mode\"\n\n#!r6rs #FALSE\n"
 ; following tests adapted from the read0 benchmark distributed by Will
 ; Clinger, which as of 08/08/2009 appears to be in the public domain,
 ; with no license, copyright notice, author name, or date.
  "; Test error \"\x488; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x488;\n"
  "; Test error \"@ symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs @\n"
  "; Test error \"@b symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs @b\n"
  "; Test error \"@ symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs @\n"
  "; Test error \"\x488; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x488;\n"
  "; Test error \"\x489; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x489;\n"
  "; Test error \"\x660; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x660;\n"
  "; Test error \"\x661; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x661;\n"
  "; Test error \"\x662; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x662;\n"
  "; Test error \"\x663; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x663;\n"
  "; Test error \"\x664; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x664;\n"
  "; Test error \"\x665; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x665;\n"
  "; Test error \"\x666; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x666;\n"
  "; Test error \"\x667; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x667;\n"
  "; Test error \"\x668; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x668;\n"
  "; Test error \"\x669; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x669;\n"
  "; Test error \"\x6F0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F0;\n"
  "; Test error \"\x6F1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F1;\n"
  "; Test error \"\x6F2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F2;\n"
  "; Test error \"\x6F3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F3;\n"
  "; Test error \"\x6F4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F4;\n"
  "; Test error \"\x6F5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F5;\n"
  "; Test error \"\x6F6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F6;\n"
  "; Test error \"\x6F7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F7;\n"
  "; Test error \"\x6F8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F8;\n"
  "; Test error \"\x6F9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x6F9;\n"
  "; Test error \"\x7C0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C0;\n"
  "; Test error \"\x7C1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C1;\n"
  "; Test error \"\x7C2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C2;\n"
  "; Test error \"\x7C3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C3;\n"
  "; Test error \"\x7C4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C4;\n"
  "; Test error \"\x7C5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C5;\n"
  "; Test error \"\x7C6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C6;\n"
  "; Test error \"\x7C7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C7;\n"
  "; Test error \"\x7C8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C8;\n"
  "; Test error \"\x7C9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x7C9;\n"
  "; Test error \"\x903; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x903;\n"
  "; Test error \"\x93E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x93E;\n"
  "; Test error \"\x93F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x93F;\n"
  "; Test error \"\x940; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x940;\n"
  "; Test error \"\x949; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x949;\n"
  "; Test error \"\x94A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x94A;\n"
  "; Test error \"\x94B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x94B;\n"
  "; Test error \"\x94C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x94C;\n"
  "; Test error \"\x966; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x966;\n"
  "; Test error \"\x967; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x967;\n"
  "; Test error \"\x968; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x968;\n"
  "; Test error \"\x969; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x969;\n"
  "; Test error \"\x96A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96A;\n"
  "; Test error \"\x96B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96B;\n"
  "; Test error \"\x96C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96C;\n"
  "; Test error \"\x96D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96D;\n"
  "; Test error \"\x96E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96E;\n"
  "; Test error \"\x96F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x96F;\n"
  "; Test error \"\x982; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x982;\n"
  "; Test error \"\x983; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x983;\n"
  "; Test error \"\x9BE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9BE;\n"
  "; Test error \"\x9BF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9BF;\n"
  "; Test error \"\x9C0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9C0;\n"
  "; Test error \"\x9C7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9C7;\n"
  "; Test error \"\x9C8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9C8;\n"
  "; Test error \"\x9CB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9CB;\n"
  "; Test error \"\x9CC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9CC;\n"
  "; Test error \"\x9D7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9D7;\n"
  "; Test error \"\x9E6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9E6;\n"
  "; Test error \"\x9E7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9E7;\n"
  "; Test error \"\x9E8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9E8;\n"
  "; Test error \"\x9E9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9E9;\n"
  "; Test error \"\x9EA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9EA;\n"
  "; Test error \"\x9EB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9EB;\n"
  "; Test error \"\x9EC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9EC;\n"
  "; Test error \"\x9ED; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9ED;\n"
  "; Test error \"\x9EE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9EE;\n"
  "; Test error \"\x9EF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x9EF;\n"
  "; Test error \"\xA03; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA03;\n"
  "; Test error \"\xA3E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA3E;\n"
  "; Test error \"\xA3F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA3F;\n"
  "; Test error \"\xA40; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA40;\n"
  "; Test error \"\xA66; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA66;\n"
  "; Test error \"\xA67; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA67;\n"
  "; Test error \"\xA68; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA68;\n"
  "; Test error \"\xA69; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA69;\n"
  "; Test error \"\xA6A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6A;\n"
  "; Test error \"\xA6B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6B;\n"
  "; Test error \"\xA6C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6C;\n"
  "; Test error \"\xA6D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6D;\n"
  "; Test error \"\xA6E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6E;\n"
  "; Test error \"\xA6F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA6F;\n"
  "; Test error \"\xA83; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA83;\n"
  "; Test error \"\xABE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xABE;\n"
  "; Test error \"\xABF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xABF;\n"
  "; Test error \"\xAC0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAC0;\n"
  "; Test error \"\xAC9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAC9;\n"
  "; Test error \"\xACB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xACB;\n"
  "; Test error \"\xACC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xACC;\n"
  "; Test error \"\xAE6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAE6;\n"
  "; Test error \"\xAE7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAE7;\n"
  "; Test error \"\xAE8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAE8;\n"
  "; Test error \"\xAE9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAE9;\n"
  "; Test error \"\xAEA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAEA;\n"
  "; Test error \"\xAEB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAEB;\n"
  "; Test error \"\xAEC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAEC;\n"
  "; Test error \"\xAED; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAED;\n"
  "; Test error \"\xAEE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAEE;\n"
  "; Test error \"\xAEF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAEF;\n"
  "; Test error \"\xB02; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB02;\n"
  "; Test error \"\xB03; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB03;\n"
  "; Test error \"\xB3E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB3E;\n"
  "; Test error \"\xB40; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB40;\n"
  "; Test error \"\xB47; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB47;\n"
  "; Test error \"\xB48; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB48;\n"
  "; Test error \"\xB4B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB4B;\n"
  "; Test error \"\xB4C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB4C;\n"
  "; Test error \"\xB57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB57;\n"
  "; Test error \"\xB66; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB66;\n"
  "; Test error \"\xB67; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB67;\n"
  "; Test error \"\xB68; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB68;\n"
  "; Test error \"\xB69; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB69;\n"
  "; Test error \"\xB6A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6A;\n"
  "; Test error \"\xB6B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6B;\n"
  "; Test error \"\xB6C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6C;\n"
  "; Test error \"\xB6D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6D;\n"
  "; Test error \"\xB6E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6E;\n"
  "; Test error \"\xB6F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xB6F;\n"
  "; Test error \"\xBBE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBBE;\n"
  "; Test error \"\xBBF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBBF;\n"
  "; Test error \"\xBC1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBC1;\n"
  "; Test error \"\xBC2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBC2;\n"
  "; Test error \"\xBC6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBC6;\n"
  "; Test error \"\xBC7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBC7;\n"
  "; Test error \"\xBC8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBC8;\n"
  "; Test error \"\xBCA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBCA;\n"
  "; Test error \"\xBCB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBCB;\n"
  "; Test error \"\xBCC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBCC;\n"
  "; Test error \"\xBD7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBD7;\n"
  "; Test error \"\xBE6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBE6;\n"
  "; Test error \"\xBE7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBE7;\n"
  "; Test error \"\xBE8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBE8;\n"
  "; Test error \"\xBE9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBE9;\n"
  "; Test error \"\xBEA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBEA;\n"
  "; Test error \"\xBEB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBEB;\n"
  "; Test error \"\xBEC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBEC;\n"
  "; Test error \"\xBED; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBED;\n"
  "; Test error \"\xBEE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBEE;\n"
  "; Test error \"\xBEF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xBEF;\n"
  "; Test error \"\xC01; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC01;\n"
  "; Test error \"\xC02; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC02;\n"
  "; Test error \"\xC03; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC03;\n"
  "; Test error \"\xC41; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC41;\n"
  "; Test error \"\xC42; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC42;\n"
  "; Test error \"\xC43; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC43;\n"
  "; Test error \"\xC44; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC44;\n"
  "; Test error \"\xC66; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC66;\n"
  "; Test error \"\xC67; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC67;\n"
  "; Test error \"\xC68; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC68;\n"
  "; Test error \"\xC69; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC69;\n"
  "; Test error \"\xC6A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6A;\n"
  "; Test error \"\xC6B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6B;\n"
  "; Test error \"\xC6C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6C;\n"
  "; Test error \"\xC6D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6D;\n"
  "; Test error \"\xC6E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6E;\n"
  "; Test error \"\xC6F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC6F;\n"
  "; Test error \"\xC82; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC82;\n"
  "; Test error \"\xC83; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xC83;\n"
  "; Test error \"\xCBE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCBE;\n"
  "; Test error \"\xCC0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC0;\n"
  "; Test error \"\xCC1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC1;\n"
  "; Test error \"\xCC2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC2;\n"
  "; Test error \"\xCC3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC3;\n"
  "; Test error \"\xCC4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC4;\n"
  "; Test error \"\xCC7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC7;\n"
  "; Test error \"\xCC8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCC8;\n"
  "; Test error \"\xCCA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCCA;\n"
  "; Test error \"\xCCB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCCB;\n"
  "; Test error \"\xCD5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCD5;\n"
  "; Test error \"\xCD6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCD6;\n"
  "; Test error \"\xCE6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCE6;\n"
  "; Test error \"\xCE7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCE7;\n"
  "; Test error \"\xCE8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCE8;\n"
  "; Test error \"\xCE9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCE9;\n"
  "; Test error \"\xCEA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCEA;\n"
  "; Test error \"\xCEB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCEB;\n"
  "; Test error \"\xCEC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCEC;\n"
  "; Test error \"\xCED; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCED;\n"
  "; Test error \"\xCEE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCEE;\n"
  "; Test error \"\xCEF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xCEF;\n"
  "; Test error \"\xD02; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD02;\n"
  "; Test error \"\xD03; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD03;\n"
  "; Test error \"\xD3E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD3E;\n"
  "; Test error \"\xD3F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD3F;\n"
  "; Test error \"\xD40; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD40;\n"
  "; Test error \"\xD46; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD46;\n"
  "; Test error \"\xD47; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD47;\n"
  "; Test error \"\xD48; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD48;\n"
  "; Test error \"\xD4A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD4A;\n"
  "; Test error \"\xD4B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD4B;\n"
  "; Test error \"\xD4C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD4C;\n"
  "; Test error \"\xD57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD57;\n"
  "; Test error \"\xD66; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD66;\n"
  "; Test error \"\xD67; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD67;\n"
  "; Test error \"\xD68; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD68;\n"
  "; Test error \"\xD69; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD69;\n"
  "; Test error \"\xD6A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6A;\n"
  "; Test error \"\xD6B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6B;\n"
  "; Test error \"\xD6C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6C;\n"
  "; Test error \"\xD6D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6D;\n"
  "; Test error \"\xD6E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6E;\n"
  "; Test error \"\xD6F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD6F;\n"
  "; Test error \"\xD82; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD82;\n"
  "; Test error \"\xD83; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xD83;\n"
  "; Test error \"\xDCF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDCF;\n"
  "; Test error \"\xDD0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDD0;\n"
  "; Test error \"\xDD1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDD1;\n"
  "; Test error \"\xDD8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDD8;\n"
  "; Test error \"\xDD9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDD9;\n"
  "; Test error \"\xDDA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDA;\n"
  "; Test error \"\xDDB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDB;\n"
  "; Test error \"\xDDC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDC;\n"
  "; Test error \"\xDDD; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDD;\n"
  "; Test error \"\xDDE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDE;\n"
  "; Test error \"\xDDF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDDF;\n"
  "; Test error \"\xDF2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDF2;\n"
  "; Test error \"\xDF3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xDF3;\n"
  "; Test error \"\xE50; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE50;\n"
  "; Test error \"\xE51; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE51;\n"
  "; Test error \"\xE52; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE52;\n"
  "; Test error \"\xE53; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE53;\n"
  "; Test error \"\xE54; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE54;\n"
  "; Test error \"\xE55; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE55;\n"
  "; Test error \"\xE56; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE56;\n"
  "; Test error \"\xE57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE57;\n"
  "; Test error \"\xE58; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE58;\n"
  "; Test error \"\xE59; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xE59;\n"
  "; Test error \"\xED0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED0;\n"
  "; Test error \"\xED1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED1;\n"
  "; Test error \"\xED2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED2;\n"
  "; Test error \"\xED3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED3;\n"
  "; Test error \"\xED4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED4;\n"
  "; Test error \"\xED5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED5;\n"
  "; Test error \"\xED6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED6;\n"
  "; Test error \"\xED7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED7;\n"
  "; Test error \"\xED8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED8;\n"
  "; Test error \"\xED9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xED9;\n"
  "; Test error \"\xF20; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF20;\n"
  "; Test error \"\xF21; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF21;\n"
  "; Test error \"\xF22; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF22;\n"
  "; Test error \"\xF23; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF23;\n"
  "; Test error \"\xF24; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF24;\n"
  "; Test error \"\xF25; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF25;\n"
  "; Test error \"\xF26; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF26;\n"
  "; Test error \"\xF27; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF27;\n"
  "; Test error \"\xF28; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF28;\n"
  "; Test error \"\xF29; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF29;\n"
  "; Test error \"\xF3E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF3E;\n"
  "; Test error \"\xF3F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF3F;\n"
  "; Test error \"\xF7F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xF7F;\n"
  "; Test error \"\x102B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x102B;\n"
  "; Test error \"\x102C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x102C;\n"
  "; Test error \"\x1031; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1031;\n"
  "; Test error \"\x1038; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1038;\n"
  "; Test error \"\x103B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x103B;\n"
  "; Test error \"\x103C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x103C;\n"
  "; Test error \"\x1040; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1040;\n"
  "; Test error \"\x1041; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1041;\n"
  "; Test error \"\x1042; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1042;\n"
  "; Test error \"\x1043; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1043;\n"
  "; Test error \"\x1044; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1044;\n"
  "; Test error \"\x1045; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1045;\n"
  "; Test error \"\x1046; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1046;\n"
  "; Test error \"\x1047; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1047;\n"
  "; Test error \"\x1048; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1048;\n"
  "; Test error \"\x1049; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1049;\n"
  "; Test error \"\x1056; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1056;\n"
  "; Test error \"\x1057; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1057;\n"
  "; Test error \"\x1062; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1062;\n"
  "; Test error \"\x1063; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1063;\n"
  "; Test error \"\x1064; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1064;\n"
  "; Test error \"\x1067; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1067;\n"
  "; Test error \"\x1068; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1068;\n"
  "; Test error \"\x1069; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1069;\n"
  "; Test error \"\x106A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x106A;\n"
  "; Test error \"\x106B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x106B;\n"
  "; Test error \"\x106C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x106C;\n"
  "; Test error \"\x106D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x106D;\n"
  "; Test error \"\x1083; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1083;\n"
  "; Test error \"\x1084; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1084;\n"
  "; Test error \"\x1087; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1087;\n"
  "; Test error \"\x1088; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1088;\n"
  "; Test error \"\x1089; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1089;\n"
  "; Test error \"\x108A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x108A;\n"
  "; Test error \"\x108B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x108B;\n"
  "; Test error \"\x108C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x108C;\n"
  "; Test error \"\x108F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x108F;\n"
  "; Test error \"\x1090; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1090;\n"
  "; Test error \"\x1091; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1091;\n"
  "; Test error \"\x1092; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1092;\n"
  "; Test error \"\x1093; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1093;\n"
  "; Test error \"\x1094; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1094;\n"
  "; Test error \"\x1095; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1095;\n"
  "; Test error \"\x1096; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1096;\n"
  "; Test error \"\x1097; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1097;\n"
  "; Test error \"\x1098; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1098;\n"
  "; Test error \"\x1099; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1099;\n"
  "; Test error \"\x17B6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17B6;\n"
  "; Test error \"\x17BE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17BE;\n"
  "; Test error \"\x17BF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17BF;\n"
  "; Test error \"\x17C0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C0;\n"
  "; Test error \"\x17C1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C1;\n"
  "; Test error \"\x17C2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C2;\n"
  "; Test error \"\x17C3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C3;\n"
  "; Test error \"\x17C4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C4;\n"
  "; Test error \"\x17C5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C5;\n"
  "; Test error \"\x17C7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C7;\n"
  "; Test error \"\x17C8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17C8;\n"
  "; Test error \"\x17E0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E0;\n"
  "; Test error \"\x17E1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E1;\n"
  "; Test error \"\x17E2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E2;\n"
  "; Test error \"\x17E3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E3;\n"
  "; Test error \"\x17E4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E4;\n"
  "; Test error \"\x17E5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E5;\n"
  "; Test error \"\x17E6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E6;\n"
  "; Test error \"\x17E7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E7;\n"
  "; Test error \"\x17E8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E8;\n"
  "; Test error \"\x17E9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x17E9;\n"
  "; Test error \"\x1810; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1810;\n"
  "; Test error \"\x1811; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1811;\n"
  "; Test error \"\x1812; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1812;\n"
  "; Test error \"\x1813; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1813;\n"
  "; Test error \"\x1814; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1814;\n"
  "; Test error \"\x1815; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1815;\n"
  "; Test error \"\x1816; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1816;\n"
  "; Test error \"\x1817; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1817;\n"
  "; Test error \"\x1818; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1818;\n"
  "; Test error \"\x1819; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1819;\n"
  "; Test error \"\x1923; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1923;\n"
  "; Test error \"\x1924; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1924;\n"
  "; Test error \"\x1925; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1925;\n"
  "; Test error \"\x1926; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1926;\n"
  "; Test error \"\x1929; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1929;\n"
  "; Test error \"\x192A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x192A;\n"
  "; Test error \"\x192B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x192B;\n"
  "; Test error \"\x1930; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1930;\n"
  "; Test error \"\x1931; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1931;\n"
  "; Test error \"\x1933; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1933;\n"
  "; Test error \"\x1934; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1934;\n"
  "; Test error \"\x1935; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1935;\n"
  "; Test error \"\x1936; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1936;\n"
  "; Test error \"\x1937; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1937;\n"
  "; Test error \"\x1938; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1938;\n"
  "; Test error \"\x1946; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1946;\n"
  "; Test error \"\x1947; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1947;\n"
  "; Test error \"\x1948; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1948;\n"
  "; Test error \"\x1949; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1949;\n"
  "; Test error \"\x194A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194A;\n"
  "; Test error \"\x194B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194B;\n"
  "; Test error \"\x194C; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194C;\n"
  "; Test error \"\x194D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194D;\n"
  "; Test error \"\x194E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194E;\n"
  "; Test error \"\x194F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x194F;\n"
  "; Test error \"\x19D0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D0;\n"
  "; Test error \"\x19D1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D1;\n"
  "; Test error \"\x19D2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D2;\n"
  "; Test error \"\x19D3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D3;\n"
  "; Test error \"\x19D4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D4;\n"
  "; Test error \"\x19D5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D5;\n"
  "; Test error \"\x19D6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D6;\n"
  "; Test error \"\x19D7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D7;\n"
  "; Test error \"\x19D8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D8;\n"
  "; Test error \"\x19D9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x19D9;\n"
  "; Test error \"\x1A19; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1A19;\n"
  "; Test error \"\x1A1A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1A1A;\n"
  "; Test error \"\x1B04; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B04;\n"
  "; Test error \"\x1B35; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B35;\n"
  "; Test error \"\x1B3B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B3B;\n"
  "; Test error \"\x1B3D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B3D;\n"
  "; Test error \"\x1B3E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B3E;\n"
  "; Test error \"\x1B3F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B3F;\n"
  "; Test error \"\x1B40; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B40;\n"
  "; Test error \"\x1B41; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B41;\n"
  "; Test error \"\x1B43; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B43;\n"
  "; Test error \"\x1B44; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B44;\n"
  "; Test error \"\x1B50; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B50;\n"
  "; Test error \"\x1B51; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B51;\n"
  "; Test error \"\x1B52; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B52;\n"
  "; Test error \"\x1B53; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B53;\n"
  "; Test error \"\x1B54; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B54;\n"
  "; Test error \"\x1B55; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B55;\n"
  "; Test error \"\x1B56; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B56;\n"
  "; Test error \"\x1B57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B57;\n"
  "; Test error \"\x1B58; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B58;\n"
  "; Test error \"\x1B59; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B59;\n"
  "; Test error \"\x1B82; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1B82;\n"
  "; Test error \"\x1BA1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BA1;\n"
  "; Test error \"\x1BA6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BA6;\n"
  "; Test error \"\x1BA7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BA7;\n"
  "; Test error \"\x1BAA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BAA;\n"
  "; Test error \"\x1BB0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB0;\n"
  "; Test error \"\x1BB1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB1;\n"
  "; Test error \"\x1BB2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB2;\n"
  "; Test error \"\x1BB3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB3;\n"
  "; Test error \"\x1BB4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB4;\n"
  "; Test error \"\x1BB5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB5;\n"
  "; Test error \"\x1BB6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB6;\n"
  "; Test error \"\x1BB7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB7;\n"
  "; Test error \"\x1BB8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB8;\n"
  "; Test error \"\x1BB9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1BB9;\n"
  "; Test error \"\x1C24; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C24;\n"
  "; Test error \"\x1C25; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C25;\n"
  "; Test error \"\x1C26; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C26;\n"
  "; Test error \"\x1C27; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C27;\n"
  "; Test error \"\x1C28; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C28;\n"
  "; Test error \"\x1C29; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C29;\n"
  "; Test error \"\x1C2A; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C2A;\n"
  "; Test error \"\x1C2B; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C2B;\n"
  "; Test error \"\x1C34; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C34;\n"
  "; Test error \"\x1C35; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C35;\n"
  "; Test error \"\x1C40; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C40;\n"
  "; Test error \"\x1C41; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C41;\n"
  "; Test error \"\x1C42; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C42;\n"
  "; Test error \"\x1C43; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C43;\n"
  "; Test error \"\x1C44; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C44;\n"
  "; Test error \"\x1C45; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C45;\n"
  "; Test error \"\x1C46; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C46;\n"
  "; Test error \"\x1C47; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C47;\n"
  "; Test error \"\x1C48; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C48;\n"
  "; Test error \"\x1C49; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C49;\n"
  "; Test error \"\x1C50; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C50;\n"
  "; Test error \"\x1C51; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C51;\n"
  "; Test error \"\x1C52; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C52;\n"
  "; Test error \"\x1C53; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C53;\n"
  "; Test error \"\x1C54; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C54;\n"
  "; Test error \"\x1C55; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C55;\n"
  "; Test error \"\x1C56; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C56;\n"
  "; Test error \"\x1C57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C57;\n"
  "; Test error \"\x1C58; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C58;\n"
  "; Test error \"\x1C59; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1C59;\n"
  "; Test error \"\x20DD; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20DD;\n"
  "; Test error \"\x20DE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20DE;\n"
  "; Test error \"\x20DF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20DF;\n"
  "; Test error \"\x20E0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20E0;\n"
  "; Test error \"\x20E2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20E2;\n"
  "; Test error \"\x20E3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20E3;\n"
  "; Test error \"\x20E4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x20E4;\n"
  "; Test error \"\xA620; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA620;\n"
  "; Test error \"\xA621; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA621;\n"
  "; Test error \"\xA622; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA622;\n"
  "; Test error \"\xA623; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA623;\n"
  "; Test error \"\xA624; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA624;\n"
  "; Test error \"\xA625; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA625;\n"
  "; Test error \"\xA626; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA626;\n"
  "; Test error \"\xA627; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA627;\n"
  "; Test error \"\xA628; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA628;\n"
  "; Test error \"\xA629; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA629;\n"
  "; Test error \"\xA670; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA670;\n"
  "; Test error \"\xA671; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA671;\n"
  "; Test error \"\xA672; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA672;\n"
  "; Test error \"\xA823; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA823;\n"
  "; Test error \"\xA824; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA824;\n"
  "; Test error \"\xA827; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA827;\n"
  "; Test error \"\xA880; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA880;\n"
  "; Test error \"\xA881; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA881;\n"
  "; Test error \"\xA8B4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B4;\n"
  "; Test error \"\xA8B5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B5;\n"
  "; Test error \"\xA8B6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B6;\n"
  "; Test error \"\xA8B7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B7;\n"
  "; Test error \"\xA8B8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B8;\n"
  "; Test error \"\xA8B9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8B9;\n"
  "; Test error \"\xA8BA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BA;\n"
  "; Test error \"\xA8BB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BB;\n"
  "; Test error \"\xA8BC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BC;\n"
  "; Test error \"\xA8BD; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BD;\n"
  "; Test error \"\xA8BE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BE;\n"
  "; Test error \"\xA8BF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8BF;\n"
  "; Test error \"\xA8C0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8C0;\n"
  "; Test error \"\xA8C1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8C1;\n"
  "; Test error \"\xA8C2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8C2;\n"
  "; Test error \"\xA8C3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8C3;\n"
  "; Test error \"\xA8D0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D0;\n"
  "; Test error \"\xA8D1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D1;\n"
  "; Test error \"\xA8D2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D2;\n"
  "; Test error \"\xA8D3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D3;\n"
  "; Test error \"\xA8D4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D4;\n"
  "; Test error \"\xA8D5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D5;\n"
  "; Test error \"\xA8D6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D6;\n"
  "; Test error \"\xA8D7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D7;\n"
  "; Test error \"\xA8D8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D8;\n"
  "; Test error \"\xA8D9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA8D9;\n"
  "; Test error \"\xA900; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA900;\n"
  "; Test error \"\xA901; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA901;\n"
  "; Test error \"\xA902; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA902;\n"
  "; Test error \"\xA903; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA903;\n"
  "; Test error \"\xA904; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA904;\n"
  "; Test error \"\xA905; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA905;\n"
  "; Test error \"\xA906; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA906;\n"
  "; Test error \"\xA907; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA907;\n"
  "; Test error \"\xA908; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA908;\n"
  "; Test error \"\xA909; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA909;\n"
  "; Test error \"\xA952; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA952;\n"
  "; Test error \"\xA953; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xA953;\n"
  "; Test error \"\xAA2F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA2F;\n"
  "; Test error \"\xAA30; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA30;\n"
  "; Test error \"\xAA33; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA33;\n"
  "; Test error \"\xAA34; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA34;\n"
  "; Test error \"\xAA4D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA4D;\n"
  "; Test error \"\xAA50; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA50;\n"
  "; Test error \"\xAA51; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA51;\n"
  "; Test error \"\xAA52; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA52;\n"
  "; Test error \"\xAA53; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA53;\n"
  "; Test error \"\xAA54; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA54;\n"
  "; Test error \"\xAA55; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA55;\n"
  "; Test error \"\xAA56; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA56;\n"
  "; Test error \"\xAA57; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA57;\n"
  "; Test error \"\xAA58; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA58;\n"
  "; Test error \"\xAA59; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xAA59;\n"
  "; Test error \"\xFF10; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF10;\n"
  "; Test error \"\xFF11; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF11;\n"
  "; Test error \"\xFF12; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF12;\n"
  "; Test error \"\xFF13; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF13;\n"
  "; Test error \"\xFF14; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF14;\n"
  "; Test error \"\xFF15; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF15;\n"
  "; Test error \"\xFF16; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF16;\n"
  "; Test error \"\xFF17; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF17;\n"
  "; Test error \"\xFF18; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF18;\n"
  "; Test error \"\xFF19; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \xFF19;\n"
  "; Test error \"\x104A0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A0;\n"
  "; Test error \"\x104A1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A1;\n"
  "; Test error \"\x104A2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A2;\n"
  "; Test error \"\x104A3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A3;\n"
  "; Test error \"\x104A4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A4;\n"
  "; Test error \"\x104A5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A5;\n"
  "; Test error \"\x104A6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A6;\n"
  "; Test error \"\x104A7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A7;\n"
  "; Test error \"\x104A8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A8;\n"
  "; Test error \"\x104A9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x104A9;\n"
  "; Test error \"\x1D165; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D165;\n"
  "; Test error \"\x1D166; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D166;\n"
  "; Test error \"\x1D16D; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D16D;\n"
  "; Test error \"\x1D16E; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D16E;\n"
  "; Test error \"\x1D16F; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D16F;\n"
  "; Test error \"\x1D170; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D170;\n"
  "; Test error \"\x1D171; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D171;\n"
  "; Test error \"\x1D172; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D172;\n"
  "; Test error \"\x1D7CE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7CE;\n"
  "; Test error \"\x1D7CF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7CF;\n"
  "; Test error \"\x1D7D0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D0;\n"
  "; Test error \"\x1D7D1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D1;\n"
  "; Test error \"\x1D7D2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D2;\n"
  "; Test error \"\x1D7D3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D3;\n"
  "; Test error \"\x1D7D4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D4;\n"
  "; Test error \"\x1D7D5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D5;\n"
  "; Test error \"\x1D7D6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D6;\n"
  "; Test error \"\x1D7D7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D7;\n"
  "; Test error \"\x1D7D8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D8;\n"
  "; Test error \"\x1D7D9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7D9;\n"
  "; Test error \"\x1D7DA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DA;\n"
  "; Test error \"\x1D7DB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DB;\n"
  "; Test error \"\x1D7DC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DC;\n"
  "; Test error \"\x1D7DD; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DD;\n"
  "; Test error \"\x1D7DE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DE;\n"
  "; Test error \"\x1D7DF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7DF;\n"
  "; Test error \"\x1D7E0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E0;\n"
  "; Test error \"\x1D7E1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E1;\n"
  "; Test error \"\x1D7E2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E2;\n"
  "; Test error \"\x1D7E3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E3;\n"
  "; Test error \"\x1D7E4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E4;\n"
  "; Test error \"\x1D7E5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E5;\n"
  "; Test error \"\x1D7E6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E6;\n"
  "; Test error \"\x1D7E7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E7;\n"
  "; Test error \"\x1D7E8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E8;\n"
  "; Test error \"\x1D7E9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7E9;\n"
  "; Test error \"\x1D7EA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7EA;\n"
  "; Test error \"\x1D7EB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7EB;\n"
  "; Test error \"\x1D7EC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7EC;\n"
  "; Test error \"\x1D7ED; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7ED;\n"
  "; Test error \"\x1D7EE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7EE;\n"
  "; Test error \"\x1D7EF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7EF;\n"
  "; Test error \"\x1D7F0; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F0;\n"
  "; Test error \"\x1D7F1; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F1;\n"
  "; Test error \"\x1D7F2; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F2;\n"
  "; Test error \"\x1D7F3; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F3;\n"
  "; Test error \"\x1D7F4; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F4;\n"
  "; Test error \"\x1D7F5; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F5;\n"
  "; Test error \"\x1D7F6; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F6;\n"
  "; Test error \"\x1D7F7; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F7;\n"
  "; Test error \"\x1D7F8; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F8;\n"
  "; Test error \"\x1D7F9; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7F9;\n"
  "; Test error \"\x1D7FA; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FA;\n"
  "; Test error \"\x1D7FB; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FB;\n"
  "; Test error \"\x1D7FC; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FC;\n"
  "; Test error \"\x1D7FD; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FD;\n"
  "; Test error \"\x1D7FE; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FE;\n"
  "; Test error \"\x1D7FF; symbol syntax not allowed in #!r6rs mode\"\n\n#!r6rs \x1D7FF;\n"
 )

(mat record-annotation
 ; regression check: make sure annotations do not slip into records
 ; by way of graph references
  (let ([p (open-output-file "testfile.ss" 'truncate)])
    (display "(define-record #{%foo %bar} (x y))
(define $$rats (list '#0=(a b) #;'#1=(d e) '#[#{%foo %bar} #0# #1#]))
" p)
    (close-output-port p)
    #t)
  (begin
    (load "testfile.ss")
    #t)
  (record? (cadr $$rats) (type-descriptor #{%foo %bar}))
  (let ([r (cadr $$rats)])
    (eq? (%foo-x r) (car $$rats))
    (equal? (%foo-y r) '(d e)))
 )

(mat annotation-tests
  (let ([x (read (open-input-string "#1=#2=(#1# . #2#)"))])
    (and (eq? (car x) x) (eq? (cdr x) x)))
  (let ([x (read (open-input-string "(#1=#1# . #1#)"))]
        [y (read (open-input-string "#2=#2#"))])
    (and (eq? (car x) (cdr x)) (eq? (car x) y)))
  (vector? '#(annotation 3 #f 3))
  (vector? (eval (read (open-input-string "'#(annotation #1=(a . #1#) #f #f)"))))
  (load-test "(define-record #{$elmer fudd} (c))\n(define x '#[#{$elmer fudd} 3])\n")
  (and ($elmer? x) (eq? ($elmer-c x) 3))
  (compile-test "(define-record #{$bugs bunny} (c))\n(define x '#[#{$bugs bunny} 3])\n")
  (and ($bugs? x) (eq? ($bugs-c x) 3))
  (load-test "(define-syntax $kwote (lambda (x) `(,#'quote ,x)))\n(define $argh #1=($kwote . #1#))\n")
  (eq? $argh (cdr $argh))
  (compile-test "(define-syntax $kwoat (lambda (x) `(,#'quote ,x)))\n(define $argh #1=($kwoat #1#))\n")
  (eq? $argh (cadr $argh))
  (load-test "(define-syntax $quoat (lambda (x) `(,#'quote ,x)))\n(begin (define x #1=($quoat a)) (define y #1#))\n")
  (eq? x y)
  (load-test "(define x '#1=(17 . #1#))\n(define y '#1=#2=(#1# . #2#))\n(define z '(#1=#1# . #1#))\n(define w '#2=(#1# . #1=#2#))\n")
  (and (eq? (car x) 17) (eq? (cdr x) x))
  (and (eq? (car y) y) (eq? (cdr y) y))
  (and (eq? (car z) (cdr z)) (eq? (car z) (read (open-input-string "#1=#1#"))))
  (and (eq? (car w) w) (eq? (cdr w) w))
  (compile-test "(define x1 '#1=(17 . #1#))\n(define y1 '#1=#2=(#1# . #2#))\n(define z1 '(#1=#1# . #1#))\n(define w1 '#2=(#1# . #1=#2#))\n")
  (and (eq? (car x1) 17) (eq? (cdr x1) x1))
  (and (eq? (car y1) y1) (eq? (cdr y1) y1))
  (and (eq? (car z1) (cdr z1)) (eq? (car z1) (read (open-input-string "#1=#1#"))))
  (and (eq? (car w1) w1) (eq? (cdr w1) w1))
  (load-test "(define-record #{$eager beaver} ((immutable busy)))\n(define x '(#[#{$eager beaver} #1=(a b)] #1#))\n")
  (and ($eager? (car x))
       (equal? ($eager-busy (car x)) '(a b))
       (eq? ($eager-busy (car x)) (cadr x)))
  (compile-test "(define-record #{$beaver eager} ((immutable busy)))\n(define x '(#[#{$beaver eager} #1=(a b)] #1#))\n")
  (and ($beaver? (car x))
       (equal? ($beaver-busy (car x)) '(a b))
       (eq? ($beaver-busy (car x)) (cadr x)))
 ; w/quote on record
  (load-test "(define-record #{$tony tiger} ((immutable great!)))\n(define x (list '#[#{$tony tiger} #1=(a b)] '#1#))\n")
  (and ($tony? (car x))
       (equal? ($tony-great! (car x)) '(a b))
       (eq? ($tony-great! (car x)) (cadr x)))
 ; missing quote on record; see if annotation still comes back
  (load-test "(define-record #{$tiger tony} ((immutable great!)))\n(define x (list '#[#{$tiger tony} #1=(a b)] '#1#))\n")
  (and ($tiger? (car x))
       (equal? ($tiger-great! (car x)) '(a b))
       (eq? ($tiger-great! (car x)) (cadr x)))
  (load-test "(define-record #{$slow joe} ((double-float pokey)))\n(define x '#[#{$slow joe} 3.4])\n")
  (and ($slow? x) (eqv? ($slow-pokey x) 3.4))
  (load-test "(define-syntax $silly (syntax-rules () ((_ #(a b c) #2(d e)) (list 'a 'b 'c 'd 'e '#(a b c) '#2(d e) '#3(a b c) '#(d e)))))\n(define x ($silly #(#(1 2) #3(3 4 5) #()) #(#0() #3(#&8))))\n")
  (equal?
    x
    '(#2(1 2)
       #3(3 4 5)
       #0()
       #0()
       #3(#&8)
       #3(#2(1 2) #3(3 4 5) #0())
       #2(#0() #3(#&8))
       #3(#2(1 2) #3(3 4 5) #0())
       #2(#0() #3(#&8))))
  (load-test "(define-record #{james kirk} ((double-float girls)))\n(define x '(#2=253.5 . #[#{james kirk} #2#]))\n")
  (and (= (car x) 253.5) (= (james-girls (cdr x)) 253.5))
  (load-test "(define-syntax $peabrain (identifier-syntax (a 4) ((set! a b) (list a b))))\n(define x (+ $peabrain 1))\n(define y (set! $peabrain (* x $peabrain)))\n")
  (and (equal? x 5) (equal? y '(4 20)))
 )

(mat symbol-printing
  (equal? (format "~s" '\#foo\|bar) "\\x23;foo\\x7C;bar")
  (eq? '\x23;foo\x7C;bar '\#foo\|bar)
 )

(mat with-source-path (parameters [current-directory *mats-dir*] [source-directories '(".")] [library-directories '(".")])
  (equal? (separate-eval '(source-directories)) "(\".\")\n")
  (equal?
    (with-source-path 'test "I should not be here" list)
    '("I should not be here"))
  (equal?
    (with-source-path 'test "/I/should/not/be/here" list)
    '("/I/should/not/be/here"))
  (equal?
    (with-source-path 'test "fatfib.ss" list)
    '("fatfib.ss"))
  (equal?
    (parameterize ([source-directories '("")])
      (with-source-path 'test "fatfib.ss" list))
    '("fatfib.ss"))
  (error? ; Error in test: file "fatfib.ss" not found in source directories
    (parameterize ([source-directories '("." ".")])
      (with-source-path 'test "fatfib.ss" list)))
  (error? ; Error in test: file "I should not be here" not found in source directories
    (parameterize ([source-directories '("." "../examples")])
      (with-source-path 'test "I should not be here" list)))
  (equal?
    (parameterize ([source-directories '("." "../examples")])
      (with-source-path 'test "mat.ss" list))
    '("mat.ss"))
  (equal?
    (with-source-path 'test "mat.ss" list)
    '("mat.ss"))
  (equal?
    (parameterize ([source-directories '("" "../examples")])
      (with-source-path 'test "mat.ss" list))
    '("mat.ss"))
  (error? ; Error in test: file "mat.ss" not found in source directories
    (parameterize ([source-directories '()])
      (with-source-path 'test "mat.ss" list)))
  (error? ; Error in test: file "mat.ss" not found in source directories
    (parameterize ([source-directories '("../examples")])
      (with-source-path 'test "mat.ss" list)))
  (equal?
    (parameterize ([source-directories '("." "../examples")])
      (with-source-path 'test "fatfib.ss" list))
    '("../examples/fatfib.ss"))
  (equal?
    (parameterize ([source-directories '("." "../examples")])
      (with-source-path 'test "./fatfib.ss" list))
    '("./fatfib.ss"))
  (begin
    (parameterize ([source-directories '("." "../examples")])
      (load "fatfib.ss" compile))
    (procedure? fatfib))
  (equal? ((inspect/object fatfib) 'type) 'procedure)
  (equal? 
    (call-with-values
      (lambda () (((inspect/object fatfib) 'code) 'source-path))
      list)
    '("../examples/fatfib.ss" 16 4))
  (equal? 
    (parameterize ([source-directories '("." "../examples")])
      (call-with-values
        (lambda () (((inspect/object fatfib) 'code) 'source-path))
        list))
    '("../examples/fatfib.ss" 16 4))
  (begin
    (load "../examples/fatfib.ss" compile)
    (procedure? fatfib))
  (equal? ((inspect/object fatfib) 'type) 'procedure)
  (equal?
    (call-with-values
      (lambda () (((inspect/object fatfib) 'code) 'source-path))
      list)
    '("../examples/fatfib.ss" 16 4))
  (or (windows?)
      (equal?
       (parameterize ([cd "/"] [source-directories (list (cd))])
         (call-with-values
           (lambda () (((inspect/object fatfib) 'code) 'source-path))
           list))
       (list (format "~a/../examples/fatfib.ss" (cd)) 16 4)))
  (begin
    (parameterize ([source-directories (list (parameterize ([cd ".."]) (cd)))])
      (load "examples/fatfib.ss" compile))
    (procedure? fatfib))
  (equal? ((inspect/object fatfib) 'type) 'procedure)
  (equal?
    (call-with-values
      (lambda () (((inspect/object fatfib) 'code) 'source-path))
      (lambda (x y z) (list (path-last x) y z)))
    '("fatfib.ss" 16 4))
  (equal?
    (parameterize ([source-directories (list (parameterize ([cd ".."]) (cd)))])
      (call-with-values
        (lambda () (((inspect/object fatfib) 'code) 'source-path))
        list))
    (list (format "~a/examples/fatfib.ss" (parameterize ([cd ".."]) (cd))) 16 4))
  (equal?
    (parameterize ([cd ".."] [source-directories '("examples")])
      (call-with-values
        (lambda () (((inspect/object fatfib) 'code) 'source-path))
        (lambda (x y z) (list (path-last x) y z))))
    '("fatfib.ss" 16 4))
  (or (windows?) (embedded?)
      (begin
        (system "ln -s ../examples .")
        (load "examples/fatfib.ss" compile)
        (system "rm -f examples")
        #t))
  (or (windows?) (embedded?)
      (equal?
        (call-with-values
          (lambda () (((inspect/object fatfib) 'code) 'source-path))
          list)
        '("examples/fatfib.ss" 359)))
  (or (windows?) (embedded?)
      (equal?
        (parameterize ([source-directories '("..")])
          (call-with-values
            (lambda () (((inspect/object fatfib) 'code) 'source-path))
            list))
        '("../examples/fatfib.ss" 16 4)))
  (or (windows?) (embedded?)
      (equal?
        (parameterize ([source-directories '("../examples")])
          (call-with-values
            (lambda () (((inspect/object fatfib) 'code) 'source-path))
            list))
        '("../examples/fatfib.ss" 16 4)))
  (or (windows?) (embedded?)
      (equal?
        (parameterize ([source-directories (list (format "~a/examples" (parameterize ([cd ".."]) (cd))))])
          (call-with-values
            (lambda () (((inspect/object fatfib) 'code) 'source-path))
            list))
        (list (format "~a/examples/fatfib.ss" (parameterize ([cd ".."]) (cd))) 16 4)))
)

(mat filesystem-operations
  (eqv?
    (directory-separator)
    (if (windows?) #\\ #\/))
  (directory-separator? #\/)
  (or (not (windows?)) (directory-separator? #\\))
  (error? (directory-separator? '/))
  (error? (directory-separator? '"/"))
  (begin
    (delete-file "testfile.ss" #f)
    (delete-file "testfile.ss" #f)
    (delete-file "testfile.ss")
    #t)
  (begin
    (with-output-to-file "testfile.ss" values)
    (r6rs:delete-file "testfile.ss")
    (not (file-exists? "testfile.ss")))
  (error? (delete-file "testfile.ss" #t))
  (error? (r6rs:delete-file "testfile.ss"))
  (and
    (not (file-exists? "testfile.ss"))
    (not (file-exists? "testfile.ss" #t))
    (not (file-exists? "testfile.ss" #f)))
  (and
    (not (file-regular? "testfile.ss"))
    (not (file-regular? "testfile.ss" #t))
    (not (file-regular? "testfile.ss" #f)))
  (and
    (not (file-directory? "testfile.ss"))
    (not (file-directory? "testfile.ss" #t))
    (not (file-directory? "testfile.ss" #f)))
  (not (file-symbolic-link? "testfile.ss"))
  (begin
    (rm-rf "testdir")
    #t)
  (and
    (not (file-exists? "testdir"))
    (not (file-exists? "testdir" #t))
    (not (file-exists? "testdir" #f)))
  (and
    (not (file-regular? "testdir"))
    (not (file-regular? "testdir" #t))
    (not (file-regular? "testdir" #f)))
  (and
    (not (file-directory? "testdir"))
    (not (file-directory? "testdir" #t))
    (not (file-directory? "testdir" #f)))
  (not (file-symbolic-link? "testdir"))
  (begin
    (mkdir "testdir")
    (and
      (file-exists? "testdir")
      (file-exists? "testdir" #t)
      (file-exists? "testdir" #f)))
  (and
    (not (file-regular? "testdir"))
    (not (file-regular? "testdir" #t))
    (not (file-regular? "testdir" #f)))
  (and
    (file-directory? "testdir")
    (file-directory? "testdir" #t)
    (file-directory? "testdir" #f))
  (not (file-symbolic-link? "testdir"))
  (eqv? (directory-list "testdir") '())
  (begin
    (with-output-to-file "testdir/testfile.ss" values)
    (and
      (file-exists? "testdir/testfile.ss")
      (file-exists? "testdir/testfile.ss" #t)
      (file-exists? "testdir/testfile.ss" #f)))
  (and
    (file-regular? "testdir/testfile.ss")
    (file-regular? "testdir/testfile.ss" #t)
    (file-regular? "testdir/testfile.ss" #f))
  (and
    (not (file-directory? "testdir/testfile.ss"))
    (not (file-directory? "testdir/testfile.ss" #t))
    (not (file-directory? "testdir/testfile.ss" #f)))
  (not (file-symbolic-link? "testdir/testfile.ss"))
  (equal? (directory-list "testdir") '("testfile.ss"))
  (begin
    (with-output-to-file "testdir/foo" values)
    (and
      (file-exists? "testdir/foo")
      (file-exists? "testdir/foo" #t)
      (file-exists? "testdir/foo" #f)))
  (begin
    (with-output-to-file "testdir/bar" values)
    (and
      (file-exists? "testdir/bar")
      (file-exists? "testdir/bar" #t)
      (file-exists? "testdir/bar" #f)))
  (file-regular? "testdir/foo")
  (not (file-directory? "testdir/foo"))
  (not (file-symbolic-link? "testdir/foo"))
  (file-regular? "testdir/bar")
  (not (file-directory? "testdir/bar"))
  (not (file-symbolic-link? "testdir/bar"))
  (equal?
    (sort string<? (directory-list "testdir"))
    '("bar" "foo" "testfile.ss"))
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir"))])
    (delete-directory "testdir" #t))
  (eqv? (delete-directory "testdir" #f) #f)
  (eqv? (delete-directory "testdir") #f)
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir/testfile.ss"))])
    (delete-directory "testdir/testfile.ss" #t))
  (not (delete-directory "testdir/testfile.ss" #f))
  (not (delete-directory "testdir/testfile.ss"))
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir"))])
    (delete-file "testdir" #t))
  (not (delete-file "testdir"))
  (not (delete-file "testdir" #f))
  (eqv? (delete-file "testdir/testfile.ss" #t) (void))
  (eqv? (delete-file "testdir/foo" #f) #t)
  (eqv? (delete-file "testdir/bar") #t)
  (not (delete-file "testdir" #f))
  (not (delete-file "testdir"))
  (eqv? (delete-directory "testdir" #f) #t)
  (begin
    (mkdir "testdir")
    (file-exists? "testdir"))
  (eqv? (delete-directory "testdir" #t) (void))
  (begin
    (mkdir "testdir")
    (file-exists? "testdir"))
  (eqv? (delete-directory "testdir") #t)
  (error? (file-exists? 'foo))
  (error? (file-regular? 'foo))
  (error? (file-directory? 'foo))
  (error? (file-symbolic-link? 'foo))
  (error? (file-exists? 'foo #t))
  (error? (file-regular? 'foo #t))
  (error? (file-directory? 'foo #t))
  (error? (file-exists? 'foo #f))
  (error? (file-regular? 'foo #f))
  (error? (file-directory? 'foo #f))
  (error? (delete-file 'foo #t))
  (error? (delete-file 'foo #f))
  (error? (delete-file 'foo))
  (error? (delete-directory 'foo #t))
  (error? (delete-directory 'foo #f))
  (error? (delete-directory 'foo))
  (error? (directory-list 'foo))
  (begin
    (mkdir "testdir")
    (with-output-to-file "testdir/rats" values)
    (file-exists? "testdir"))
  (eqv? (rename-file "testdir" "testdirx") (void))
  (eqv? (rename-file "testdirx/rats" "testdirx/star") (void))
  (not (delete-file "testdirx/rats" #f))
  (eqv? (delete-file "testdirx/star" #t) (void))
  (not (delete-directory "testdir" #f))
  (eqv? (delete-directory "testdirx" #t) (void))
  (or (embedded?) (> (length (directory-list "~")) 0))
  (or (embedded?) (> (length (directory-list "~/")) 0))
  (or (not (windows?))
      (> (length (directory-list "c:")) 0))
  (or (not (windows?))
      (> (length (directory-list "c:/")) 0))
  (or (not (windows?))
      (> (length (directory-list "\\\\?\\c:\\")) 0))
  (or (not (windows?))
      (> (length (directory-list "\\\\?\\C:\\")) 0))
  (file-directory? "/")
  (file-directory? "/.")
  (file-exists? ".")
  (file-exists? "./")
  (if (windows?)
      (and (file-directory? "c:")
           (file-directory? "c:/")
           (file-directory? "c:/."))
      (not (file-directory? "c:")))
  (if (windows?)
      (and (not (file-directory? "\\\\?\\c:"))
           (file-directory? "\\\\?\\c:\\"))
      (not (file-directory? "\\\\?\\c:")))
  (if (windows?)
      (and (file-exists? "c:")
           (file-exists? "c:/")
           (file-exists? "c:/."))
      (not (file-exists? "c:")))
  (if (windows?)
      (and (not (file-exists? "\\\\?\\c:"))
           (file-exists? "\\\\?\\c:\\"))
      (not (file-exists? "\\\\?\\c:")))
  (if (windows?)
      (and (not (file-regular? "\\\\?\\c:"))
           (not (file-regular? "\\\\?\\c:\\"))
           (or (not (file-exists? "\\\\?\\c:\\autoexec.bat"))
               (file-regular? "\\\\?\\c:\\autoexec.bat")))
      (not (file-regular? "\\\\?\\c:\\autoexec.bat")))
  (error? (get-mode 'foo))
  (error? (get-mode 'foo #t))
  (error? (get-mode 'foo #f))
  (error? (get-mode "probably/not/there"))
  (error? (get-mode "probably/not/there" #f))
  (error? (get-mode "probably/not/there" #t))
  (error? (file-access-time "probably/not/there"))
  (error? (file-access-time "probably/not/there" #f))
  (error? (file-access-time "probably/not/there" #t))
  (error? (file-change-time "probably/not/there"))
  (error? (file-change-time "probably/not/there" #f))
  (error? (file-change-time "probably/not/there" #t))
  (error? (file-modification-time "probably/not/there"))
  (error? (file-modification-time "probably/not/there" #f))
  (error? (file-modification-time "probably/not/there" #t))
  )

(mat filesystem-operations2 (parameters [current-directory *mats-dir*])
  (if (or (windows?) (embedded?))
      (fixnum? (get-mode "mat.ss"))
      (let ([m (get-mode "mat.ss")])
        (and (logtest m #o400)
             (not (logtest m #o111)))))
  (or (not (windows?))
      (and (fixnum? (get-mode "c:/"))
           (eqv? (get-mode "c:/") (get-mode "C:\\"))
           (eqv? (get-mode "c:/") (get-mode "c:\\."))))
  (if (or (windows?) (embedded?))
      (fixnum? (get-mode "../mats"))
      (eqv? (logand (get-mode "../mats") #o700) #o700))
  (and (eqv? (get-mode "../mats") (get-mode "../mats/"))
       (eqv? (get-mode "../mats") (get-mode "../mats/.")))
  ; access times are unreliable on contemporary file systems
  (time? (file-access-time "../../mats/mat.ss"))
  (time<=? (file-change-time "mat.ss") (file-change-time "mat.so"))
  (time<=? (file-modification-time "mat.ss") (file-modification-time "mat.so"))
  (equal?
    (list (time? (file-access-time "../mats"))
          (time? (file-change-time "../mats"))
          (time? (file-modification-time "../mats")))
    '(#t #t #t))
  (equal?
    (list (time? (file-access-time "../mats/"))
          (time? (file-change-time "../mats/"))
          (time? (file-modification-time "../mats/")))
    '(#t #t #t))
  (or (not (windows?))
      (and (time? (file-access-time "c:"))
           (time? (file-change-time "c:"))
           (time? (file-modification-time "c:"))))
  (or (not (windows?))
      (and (time? (file-access-time "c:/"))
           (time? (file-change-time "c:/"))
           (time? (file-modification-time "c:/"))))
  (or (not (windows?))
      (and (time? (file-access-time "\\\\?\\C:\\"))
           (time? (file-change-time "\\\\?\\C:\\"))
           (time? (file-modification-time "\\\\?\\C:\\"))))
  (or (not (windows?))
      (and (time? (file-access-time "\\\\?\\c:\\"))
           (time? (file-change-time "\\\\?\\c:\\"))
           (time? (file-modification-time "\\\\?\\c:\\"))))
  (or (windows?) (embedded?)
      (time=? (file-access-time "Makefile") (file-access-time (format "Mf-~a" (machine-type)))))
  (or (windows?) (embedded?)
      (time=? (file-change-time "Makefile") (file-change-time (format "Mf-~a" (machine-type)))))
  (or (windows?) (embedded?)
      (time=? (file-modification-time "Makefile") (file-modification-time (format "Mf-~a" (machine-type)))))
)

(mat unicode-filesystem-operations
  (begin
    (delete-file "testfile\x3bb;.ss" #f)
    (delete-file "testfile\x3bb;.ss" #f)
    (delete-file "testfile\x3bb;.ss")
    #t)
  (begin
    (with-output-to-file "testfile\x3bb;.ss" values)
    (r6rs:delete-file "testfile\x3bb;.ss")
    (not (file-exists? "testfile\x3bb;.ss")))
  (error? (delete-file "testfile\x3bb;.ss" #t))
  (error? (r6rs:delete-file "testfile\x3bb;.ss"))
  (and
    (not (file-exists? "testfile\x3bb;.ss"))
    (not (file-exists? "testfile\x3bb;.ss" #t))
    (not (file-exists? "testfile\x3bb;.ss" #f)))
  (and
    (not (file-regular? "testfile\x3bb;.ss"))
    (not (file-regular? "testfile\x3bb;.ss" #t))
    (not (file-regular? "testfile\x3bb;.ss" #f)))
  (and
    (not (file-directory? "testfile\x3bb;.ss"))
    (not (file-directory? "testfile\x3bb;.ss" #t))
    (not (file-directory? "testfile\x3bb;.ss" #f)))
  (not (file-symbolic-link? "testfile\x3bb;.ss"))
  (and
    (not (file-exists? "testdir\x3bb;"))
    (not (file-exists? "testdir\x3bb;" #t))
    (not (file-exists? "testdir\x3bb;" #f)))
  (and
    (not (file-regular? "testdir\x3bb;"))
    (not (file-regular? "testdir\x3bb;" #t))
    (not (file-regular? "testdir\x3bb;" #f)))
  (and
    (not (file-directory? "testdir\x3bb;"))
    (not (file-directory? "testdir\x3bb;" #t))
    (not (file-directory? "testdir\x3bb;" #f)))
  (not (file-symbolic-link? "testdir\x3bb;"))
  (begin
    (mkdir "testdir\x3bb;")
    (and
      (file-exists? "testdir\x3bb;")
      (file-exists? "testdir\x3bb;" #t)
      (file-exists? "testdir\x3bb;" #f)))
  (and
    (not (file-regular? "testdir\x3bb;"))
    (not (file-regular? "testdir\x3bb;" #t))
    (not (file-regular? "testdir\x3bb;" #f)))
  (and
    (file-directory? "testdir\x3bb;")
    (file-directory? "testdir\x3bb;" #t)
    (file-directory? "testdir\x3bb;" #f))
  (not (file-symbolic-link? "testdir\x3bb;"))
  (eqv? (directory-list "testdir\x3bb;") '())
  (begin
    (with-output-to-file "testdir\x3bb;/testfile\x3bb;.ss" values)
    (and
      (file-exists? "testdir\x3bb;/testfile\x3bb;.ss")
      (file-exists? "testdir\x3bb;/testfile\x3bb;.ss" #t)
      (file-exists? "testdir\x3bb;/testfile\x3bb;.ss" #f)))
  (and
    (file-regular? "testdir\x3bb;/testfile\x3bb;.ss")
    (file-regular? "testdir\x3bb;/testfile\x3bb;.ss" #t)
    (file-regular? "testdir\x3bb;/testfile\x3bb;.ss" #f))
  (and
    (not (file-directory? "testdir\x3bb;/testfile\x3bb;.ss"))
    (not (file-directory? "testdir\x3bb;/testfile\x3bb;.ss" #t))
    (not (file-directory? "testdir\x3bb;/testfile\x3bb;.ss" #f)))
  (not (file-symbolic-link? "testdir\x3bb;/testfile\x3bb;.ss"))
  (equal? (directory-list "testdir\x3bb;") '("testfile\x3bb;.ss"))
  (begin
    (with-output-to-file "testdir\x3bb;/foo" values)
    (and
      (file-exists? "testdir\x3bb;/foo")
      (file-exists? "testdir\x3bb;/foo" #t)
      (file-exists? "testdir\x3bb;/foo" #f)))
  (begin
    (with-output-to-file "testdir\x3bb;/bar" values)
    (and
      (file-exists? "testdir\x3bb;/bar")
      (file-exists? "testdir\x3bb;/bar" #t)
      (file-exists? "testdir\x3bb;/bar" #f)))
  (file-regular? "testdir\x3bb;/foo")
  (not (file-directory? "testdir\x3bb;/foo"))
  (not (file-symbolic-link? "testdir\x3bb;/foo"))
  (file-regular? "testdir\x3bb;/bar")
  (not (file-directory? "testdir\x3bb;/bar"))
  (not (file-symbolic-link? "testdir\x3bb;/bar"))
  (equal?
    (sort string<? (directory-list "testdir\x3bb;"))
    '("bar" "foo" "testfile\x3bb;.ss"))
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir\x3bb;"))])
    (delete-directory "testdir\x3bb;" #t))
  (eqv? (delete-directory "testdir\x3bb;" #f) #f)
  (eqv? (delete-directory "testdir\x3bb;") #f)
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir\x3bb;/testfile\x3bb;.ss"))])
    (delete-directory "testdir\x3bb;/testfile\x3bb;.ss" #t))
  (not (delete-directory "testdir\x3bb;/testfile\x3bb;.ss" #f))
  (not (delete-directory "testdir\x3bb;/testfile\x3bb;.ss"))
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testdir\x3bb;"))])
    (delete-file "testdir\x3bb;" #t))
  (not (delete-file "testdir\x3bb;"))
  (not (delete-file "testdir\x3bb;" #f))
  (eqv? (delete-file "testdir\x3bb;/testfile\x3bb;.ss" #t) (void))
  (eqv? (delete-file "testdir\x3bb;/foo" #f) #t)
  (eqv? (delete-file "testdir\x3bb;/bar") #t)
  (not (delete-file "testdir\x3bb;" #f))
  (not (delete-file "testdir\x3bb;"))
  (eqv? (delete-directory "testdir\x3bb;" #f) #t)
  (begin
    (mkdir "testdir\x3bb;")
    (file-exists? "testdir\x3bb;"))
  (eqv? (delete-directory "testdir\x3bb;" #t) (void))
  (begin
    (mkdir "testdir\x3bb;")
    (file-exists? "testdir\x3bb;"))
  (eqv? (delete-directory "testdir\x3bb;") #t)
  (begin
    (mkdir "testdir\x3bb;")
    (with-output-to-file "testdir\x3bb;/ra\x3bb;ts" values)
    (file-exists? "testdir\x3bb;"))
  (fixnum? (get-mode "testdir\x3bb;/ra\x3bb;ts"))
  (time? (file-access-time "testdir\x3bb;/ra\x3bb;ts"))
  (time? (file-change-time "testdir\x3bb;/ra\x3bb;ts"))
  (time? (file-modification-time "testdir\x3bb;/ra\x3bb;ts"))
  (eqv? (rename-file "testdir\x3bb;" "testdir\x3bb;x") (void))
  (eqv? (rename-file "testdir\x3bb;x/ra\x3bb;ts" "testdir\x3bb;x/sta\x3bb;r") (void))
  (not (delete-file "testdir\x3bb;x/ra\x3bb;ts" #f))
  (eqv? (delete-file "testdir\x3bb;x/sta\x3bb;r" #t) (void))
  (not (delete-directory "testdir\x3bb;" #f))
  (eqv? (delete-directory "testdir\x3bb;x" #t) (void))
  (error? (get-mode "probably/not/there\x3bb;"))
  (error? (get-mode "probably/not/there\x3bb;" #f))
  (error? (get-mode "probably/not/there\x3bb;" #t))
  (error? (file-access-time "probably/not/\x3bb;there"))
  (error? (file-access-time "probably/not/\x3bb;there" #f))
  (error? (file-access-time "probably/not/\x3bb;there" #t))
  (error? (file-change-time "probably/not/\x3bb;there"))
  (error? (file-change-time "probably/not/\x3bb;there" #f))
  (error? (file-change-time "probably/not/\x3bb;there" #t))
  (error? (file-modification-time "probably/not/\x3bb;there"))
  (error? (file-modification-time "probably/not/\x3bb;there" #f))
  (error? (file-modification-time "probably/not/\x3bb;there" #t))
)

(mat pathprocs
  (error? (path-absolute? 'a/b/c))
  (error? (path-parent 'a/b/c))
  (error? (path-last 'a/b/c))
  (error? (path-root 'a/b/c))
  (error? (path-extension 'a/b/c))

  (eq? (path-absolute? "") #f)
  (eq? (path-absolute? "a") #f)
  (eq? (path-absolute? "/") #t)
  (eq? (path-absolute? "//bar/rot") #t)
  (eq? (path-absolute? "~foo/bar") #t)
  (eq? (path-absolute? "~/foo") #t)
  (eq? (path-absolute? "../") #f)
  (eq? (path-absolute? "./") #f)
  (eq? (path-absolute? "/abc") #t)
  (eq? (path-absolute? "foo") #f)
  (eq? (path-absolute? "foo/bar/a.b") #f)
  (eq? (path-absolute? "c:abc") #f)

  (equal? (path-parent "") "")
  (equal? (path-parent "a") "")
  (equal? (path-parent "/") "/")
  (equal? (path-parent "../") "..")
  (equal? (path-parent "./") ".")
  (equal? (path-parent "/abc") "/")
  (equal? (path-parent "foo/bar") "foo")
  (equal? (path-parent "foo/bar/") "foo/bar")
  (equal? (path-parent "foo/bar/a") "foo/bar")
  (equal? (path-parent "foo/bar/a.b") "foo/bar")
  (equal? (path-parent "foo/bar.b.q/a.b") "foo/bar.b.q")
  (equal?
    (path-parent "c:abc")
    (if (windows?) "c:" ""))
  (equal?
    (path-parent "Z:abc")
    (if (windows?) "Z:" ""))

  (equal? (path-last "") "")
  (equal? (path-last "a") "a")
  (equal? (path-last "/") "")
  (equal? (path-last "../") "")
  (equal? (path-last "./") "")
  (equal? (path-last "//") "")
  (equal? (path-last "/abc") "abc")
  (equal? (path-last "foo/bar") "bar")
  (equal? (path-last "foo/bar/") "")
  (equal? (path-last "foo/bar/a") "a")
  (equal? (path-last "foo/bar/a.b") "a.b")
  (equal? (path-last "foo/bar.b.q/a.b") "a.b")
  (equal?
    (path-last "c:abc")
    (if (windows?) "abc" "c:abc"))
  (equal?
    (path-last "Z:abc")
    (if (windows?) "abc" "Z:abc"))

  (equal? (path-root "") "")
  (equal? (path-root "a") "a")
  (equal? (path-root "..") "..")
  (equal? (path-root ".") ".")
  (equal? (path-root "..abc") ".")
  (equal? (path-root "abc.") "abc")
  (equal? (path-root "a.b.c") "a.b")
  (equal? (path-root "a.b.c.ss") "a.b.c")
  (equal? (path-last "foo") "foo")
  (equal? (path-root "/foo/bar.b.q/a.b.c") "/foo/bar.b.q/a.b")
  (equal? (path-root "c:/foo/bar.b.q/a.b.c") "c:/foo/bar.b.q/a.b")
  (equal? (path-root "c:") "c:")

  (equal? (path-extension "") "")
  (equal? (path-extension "a") "")
  (equal? (path-extension "..") "")
  (equal? (path-extension ".") "")
  (equal? (path-extension "..abc") "abc")
  (equal? (path-extension "abc.") "")
  (equal? (path-extension "a.b.c") "c")
  (equal? (path-extension "a.b.c.ss") "ss")
  (equal? (path-extension "/foo/bar.b.q/a.b.c") "c")
  (equal? (path-extension "c:/foo/bar.b.q/a.b.c") "c")
  (equal? (path-extension "c:..") "")
  (equal? (path-extension "c:") "")

 ; if this test fails, search for the asterisks in the printed table
  (let ([okay? #t])
    (define print-table
      (lambda (x* expected**)
        (define print-row
          (lambda (abs? path first rest parent last root extension)
            (printf "~a~11t~a~17t~a~28t~a~39t~a~50t~a~61t~a~73t~a\n"
              abs? path first rest parent last root extension)))
        (print-row "path" " abs" " first" " rest" " parent" " last" " root" " ext")
        (let ([actual** (map (lambda (x)
                               (list
                                 (if (path-absolute? x) "t" "f")
                                 (path-first x)
                                 (path-rest x)
                                 (path-parent x)
                                 (path-last x)
                                 (path-root x)
                                 (path-extension x)))
                             x*)])
          (for-each
            (lambda (x expected* actual*)
              (define uscore (lambda (s) (if (eqv? s "") "_" s)))
              (apply print-row x
                (map (lambda (expected actual)
                       (format "~a~a"
                         (if (string=? expected actual) " " (begin (set! okay? #f) "*"))
                         (uscore actual)))
                     expected* actual*)))
            x* expected** actual**))))

    (define-syntax table
      (syntax-rules ()
        [(_ (path abs? first rest parent last root extension) ...)
         (print-table '(path ...)
           '((abs? first rest parent last root extension) ...))]))

   ; common
    (table
      ("c" "f" "" "c" "" "c" "c" "")
      ("c." "f" "" "c." "" "c." "c" "")
      ("c.q" "f" "" "c.q" "" "c.q" "c" "q")
      ("c.qq" "f" "" "c.qq" "" "c.qq" "c" "qq")
      ("c.qqqqq" "f" "" "c.qqqqq" "" "c.qqqqq" "c" "qqqqq")
      ("c.qqq." "f" "" "c.qqq." "" "c.qqq." "c.qqq" "")
      ("c.qqq.zz" "f" "" "c.qqq.zz" "" "c.qqq.zz" "c.qqq" "zz")
      ("c./" "f" "c." "" "c." "" "c./" "")
      ("c.q/" "f" "c.q" "" "c.q" "" "c.q/" "")
      ("c.qq.z/" "f" "c.qq.z" "" "c.qq.z" "" "c.qq.z/" "")
      (".qq" "f" "" ".qq" "" ".qq" "" "qq")
      (".qq.z" "f" "" ".qq.z" "" ".qq.z" ".qq" "z")
      ("/" "t" "/" "" "/" "" "/" "")
      ("/abc" "t" "/" "abc" "/" "abc" "/abc" "")
      ("/abc/" "t" "/" "abc/" "/abc" "" "/abc/" "")
      ("abc" "f" "" "abc" "" "abc" "abc" "")
      ("/abc/def" "t" "/" "abc/def" "/abc" "def" "/abc/def" "")
      ("abc//def" "f" "abc" "def" "abc" "def" "abc//def" "")
      (".." "f" ".." "" ".." "" ".." "")
      ("../.." "f" ".." ".." ".." ".." "../.." "")
      ("../" "f" ".." "" ".." "" "../" "")
      ("../a" "f" ".." "a" ".." "a" "../a" "")
      ("../a/b" "f" ".." "a/b" "../a" "b" "../a/b" "")
      ("." "f" "." "" "." "" "." "")
      ("./." "f" "." "." "." "." "./." "")
      ("./" "f" "." "" "." "" "./" "")
      ("./a" "f" "." "a" "." "a" "./a" "")
      ("./a/b" "f" "." "a/b" "./a" "b" "./a/b" "")
      ("..." "f" "" "..." "" "..." ".." "")
      (".../" "f" "..." "" "..." "" ".../" "")
      (".../a" "f" "..." "a" "..." "a" ".../a" "")
      (".foo" "f" "" ".foo" "" ".foo" "" "foo")
      (".foo/" "f" ".foo" "" ".foo" "" ".foo/" "")
      (".foo/a" "f" ".foo" "a" ".foo" "a" ".foo/a" "")
      (".foo/a.q" "f" ".foo" "a.q" ".foo" "a.q" ".foo/a" "q")
      ("~" "t" "~" "" "~" "" "~" "")
      ("~/a" "t" "~" "a" "~" "a" "~/a" "")
      ("~/a/" "t" "~" "a/" "~/a" "" "~/a/" "")
      ("~/a/b" "t" "~" "a/b" "~/a" "b" "~/a/b" "")
      ("~a" "t" "~a" "" "~a" "" "~a" "")
      ("~a.b" "t" "~a.b" "" "~a.b" "" "~a.b" "")
      ("~a/" "t" "~a" "" "~a" "" "~a/" "")
      ("~a/b" "t" "~a" "b" "~a" "b" "~a/b" "")
      ("~a/b/" "t" "~a" "b/" "~a/b" "" "~a/b/" "")
      ("~a/b/c" "t" "~a" "b/c" "~a/b" "c" "~a/b/c" "")
    )

   ; windows
    (if (windows?)
        (table
          ("c:" "f" "c:" "" "c:" "" "c:" "")
          ("c:/" "t" "c:/" "" "c:/" "" "c:/" "")
          ("c:.." "f" "c:" ".." "c:" ".." "c:.." "")
          ("c:../" "f" "c:" "../" "c:.." "" "c:../" "")
          ("c:../a" "f" "c:" "../a" "c:.." "a" "c:../a" "")
          ("c:." "f" "c:" "." "c:" "." "c:." "")
          ("c:./" "f" "c:" "./" "c:." "" "c:./" "")
          ("c:./a" "f" "c:" "./a" "c:." "a" "c:./a" "")
          ("c:/abc" "t" "c:/" "abc" "c:/" "abc" "c:/abc" "")
          ("c:abc" "f" "c:" "abc" "c:" "abc" "c:abc" "")
          ("c:abc/def" "f" "c:" "abc/def" "c:abc" "def" "c:abc/def" "")
          ("c:/abc/def" "t" "c:/" "abc/def" "c:/abc" "def" "c:/abc/def" "")
          ("//abc" "t" "//abc" "" "//abc" "" "//abc" "")
          ("//abc/" "t" "//abc" "" "//abc" "" "//abc/" "")
          ("//abc/def" "t" "//abc" "def" "//abc" "def" "//abc/def" "")
          ("//x.com" "t" "//x.com" "" "//x.com" "" "//x.com" "")
          ("\\\\?\\" "t" "\\\\?\\" "" "\\\\?\\" "" "\\\\?\\" "" )
          ("\\\\?\\c:" "t" "\\\\?\\c:" "" "\\\\?\\c:" "" "\\\\?\\c:" "" )
          ("\\\\?\\c:\\" "t" "\\\\?\\c:\\" "" "\\\\?\\c:\\" "" "\\\\?\\c:\\" "" )
          ("\\\\?\\UNC\\" "t" "\\\\?\\UNC\\" "" "\\\\?\\UNC\\" "" "\\\\?\\UNC\\" "" )
          ("\\\\?\\Unc\\" "t" "\\\\?\\Unc\\" "" "\\\\?\\Unc\\" "" "\\\\?\\Unc\\" "" )
          ("\\\\?\\uNc\\\\" "t" "\\\\?\\uNc\\\\" "" "\\\\?\\uNc\\\\" "" "\\\\?\\uNc\\\\" "" )
          ("\\\\?\\unc\\x.com" "t" "\\\\?\\unc\\x.com" "" "\\\\?\\unc\\x.com" "" "\\\\?\\unc\\x.com" "" )
          ("\\\\?\\unc\\x.com\\rot.foo" "t" "\\\\?\\unc\\x.com" "rot.foo" "\\\\?\\unc\\x.com" "rot.foo" "\\\\?\\unc\\x.com\\rot" "foo" )
          ("\\\\?\\unc\\\\x.com\\rot.foo" "t" "\\\\?\\unc\\\\x.com" "rot.foo" "\\\\?\\unc\\\\x.com" "rot.foo" "\\\\?\\unc\\\\x.com\\rot" "foo" )
          ("\\\\?\\unc\\x.com/rot.foo" "t" "\\\\?\\unc\\x.com/rot.foo" "" "\\\\?\\unc\\x.com/rot.foo" "" "\\\\?\\unc\\x.com/rot.foo" "" )
        )
        (table
          ("c:" "f" "" "c:" "" "c:" "c:" "")
          ("c:/" "f" "c:" "" "c:" "" "c:/" "")
          ("c:.." "f" "" "c:.." "" "c:.." "c:." "")
          ("c:../" "f" "c:.." "" "c:.." "" "c:../" "")
          ("c:../a" "f" "c:.." "a" "c:.." "a" "c:../a" "")
          ("c:." "f" "" "c:." "" "c:." "c:" "")
          ("c:./" "f" "c:." "" "c:." "" "c:./" "")
          ("c:./a" "f" "c:." "a" "c:." "a" "c:./a" "")
          ("c:/abc" "f" "c:" "abc" "c:" "abc" "c:/abc" "")
          ("c:abc" "f" "" "c:abc" "" "c:abc" "c:abc" "")
          ("c:abc/def" "f" "c:abc" "def" "c:abc" "def" "c:abc/def" "")
          ("c:/abc/def" "f" "c:" "abc/def" "c:/abc" "def" "c:/abc/def" "")
          ("//abc" "t" "/" "abc" "/" "abc" "//abc" "")
          ("//abc/" "t" "/" "abc/" "//abc" "" "//abc/" "")
          ("//abc/def" "t" "/" "abc/def" "//abc" "def" "//abc/def" "")
          ("//x.com" "t" "/" "x.com" "/" "x.com" "//x" "com")
        ))
    okay?)
)

(mat binary-vs-textual-port
  (textual-port? (current-input-port))
  (not (binary-port? (current-input-port)))
  (textual-port? (current-output-port))
  (not (binary-port? (current-output-port)))

  (begin
    (define $handler-standin (#%$port-handler (open-string-input-port "hi")))
    #t)

  (binary-port? (#%$make-binary-input-port "" $handler-standin '#vu8()))
  (not (textual-port? (#%$make-binary-input-port "" $handler-standin '#vu8())))
  (not (binary-port? (#%$make-textual-input-port "" $handler-standin "")))
  (textual-port? (#%$make-textual-input-port "" $handler-standin ""))
  (not (binary-port? (make-input-port values "")))
  (textual-port? (make-input-port values ""))

  (binary-port? (#%$make-binary-output-port "" $handler-standin '#vu8()))
  (not (textual-port? (#%$make-binary-output-port "" $handler-standin '#vu8())))
  (not (binary-port? (#%$make-textual-output-port "" $handler-standin "")))
  (textual-port? (#%$make-textual-output-port "" $handler-standin ""))
  (not (binary-port? (make-output-port values "")))
  (textual-port? (make-output-port values ""))

  (let ((x (make-input-port values "")))
    (and (port? x)
         (and (input-port? x) (textual-port? x))
         (not (and (output-port? x) (binary-port? x)))
         (not (output-port? x))
         (not (binary-port? x))))
  (let ((x (#%$make-binary-input-port "" $handler-standin '#vu8())))
    (and (port? x)
         (and (input-port? x) (binary-port? x))
         (not (and (output-port? x) (textual-port? x)))
         (not (output-port? x))
         (not (textual-port? x))))
  (let ((x (#%$make-textual-input-port "" $handler-standin "")))
    (and (port? x)
         (and (input-port? x) (textual-port? x))
         (not (and (output-port? x) (binary-port? x)))
         (not (output-port? x))
         (not (binary-port? x))))
  (let ((x (make-output-port values "")))
    (and (port? x)
         (and (output-port? x) (textual-port? x))
         (not (and (input-port? x) (binary-port? x)))
         (not (input-port? x))
         (not (binary-port? x))))
  (let ((x (#%$make-binary-output-port "" $handler-standin '#vu8())))
    (and (port? x)
         (and (output-port? x) (binary-port? x))
         (not (and (input-port? x) (textual-port? x)))
         (not (input-port? x))
         (not (textual-port? x))))
  (let ((x (#%$make-textual-output-port "" $handler-standin "")))
    (and (port? x)
         (and (output-port? x) (textual-port? x))
         (not (and (input-port? x) (binary-port? x)))
         (not (input-port? x))
         (not (binary-port? x))))
)

(mat port-name
  (equal? "foo" (port-name (#%$make-binary-output-port "foo" $handler-standin #vu8())))
  (equal? "foo" (port-name (#%$make-textual-output-port "foo" $handler-standin "")))
  (equal? "foo" (let ([x (#%$make-binary-output-port "rot" $handler-standin #vu8())])
                  (set-port-name! x "foo")
                  (port-name x)))
  (equal? "foo" (let ([x (#%$make-textual-output-port "#f" $handler-standin "")])
                  (set-port-name! x "foo")
                  (port-name x)))
  (equal? "foo" (port-name (make-output-port (lambda args "foo") "")))
  (equal? "generic" (port-name (make-output-port (lambda args (errorf 'foo "foo")) "")))
)

(mat procedure-name
  (equal? (format "~s" car) "#<procedure car>")
  (equal? (format "~s" (lambda (x) x)) "#<procedure>")
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print '(define ($pn-q x) (lambda (y) (+ x y)))))
      'replace)
    (load "testfile.ss" compile)
    #t)
  (equal? (format "~s" $pn-q) "#<procedure $pn-q at testfile.ss:0>")
  (equal? (format "~s" ($pn-q 3)) "#<procedure at testfile.ss:18>")
)

(mat bignum-printing
  (let ()
    (define wrint
      (let ([digit->char
             (lambda (d)
               (string-ref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" d))])
        (lambda (n b)
          (if (< n b)
              (write-char (digit->char n))
              (begin
                (wrint (quotient n b) b)
                (write-char (digit->char (remainder n b))))))))
    (do ([i 4000 (fx- i 1)])
        ((fx= i 0))
      (let ([n (random (expt 2 (random (* (fixnum-width) 30))))]
            [b (+ 2 (random 35))])
        (unless (let ([s (with-output-to-string (lambda () (wrint n b)))])
                  (and (string=?
                         (parameterize ([print-radix b]) (format "~a" n))
                         s)
                       (or (= n 0)
                         (string=?
                           (parameterize ([print-radix b]) (format "~a" (- n)))
                           (format "-~a" s)))))
          (errorf #f "failed in base ~s for ~s" b n))
        (unless (string=?
                  (format "~a" n)
                  (with-output-to-string (lambda () (wrint n 10))))
          (errorf #f "failed in base 10 for ~s" n))))
    #t)
)

(mat process
   (begin (set! p (process (patch-exec-path $cat_flush)))
          (= (length p) 3))
   (and (port? (car p)) (input-port? (car p))
        (port? (cadr p)) (output-port? (cadr p))
        (integer? (caddr p)))
   (and (file-port? (car p)) (file-port? (cadr p)))
   (and (fixnum? (port-file-descriptor (car p)))
        (fixnum? (port-file-descriptor (cadr p))))
   (let ([ip (car p)])
     (and (not (port-has-port-position? ip))
          (not (port-has-set-port-position!? ip))
          (not (port-has-port-length? ip))
          (not (port-has-set-port-length!? ip))))
   (let ([op (car p)])
     (and (not (port-has-port-position? op))
          (not (port-has-set-port-position!? op))
          (not (port-has-port-length? op))
          (not (port-has-set-port-length!? op))))
   (not (char-ready? (car p)))
   (begin (display "hello " (cadr p))
          (flush-output-port (cadr p))
          #t)
   (begin (sleep (make-time 'time-duration 0 3)) #t) ; wait for subprocess to catch up
   (char-ready? (car p))
   (eq? (read (car p)) 'hello)
   (char-ready? (car p))
   (char=? (read-char (car p)) #\space)
   (not (char-ready? (car p)))
   (begin (close-output-port (cadr p)) #t)
   (begin (sleep (make-time 'time-duration 0 3)) #t) ; wait for subprocess to catch up
   (sanitized-error? (write-char #\a (cadr p)))
   (sanitized-error? (write-char #\newline (cadr p)))
   (sanitized-error? (flush-output-port (cadr p)))
   (char-ready? (car p))
   (eof-object? (read-char (car p)))
   (begin (close-input-port (car p)) #t)
   (sanitized-error? (char-ready? (car p)))
   (sanitized-error? (read-char (car p)))
   (sanitized-error? (clear-input-port (cadr p)))
 )
