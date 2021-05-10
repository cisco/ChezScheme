;;; io.ms
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

(define (native-string->bytevector s)
  (string->bytevector s (native-transcoder)))

; convert uses of custom-port-warning? to warning? if custom-port warnings
; are enabled in io.ss
(define (custom-port-warning? x) #t)

(define prettytest.ss (format "~a/prettytest.ss" *mats-dir*))

(mat port-operations
  (error? (close-port cons))
  ; the following several clauses test various open-file-output-port options
  (let ([p (open-file-output-port "testfile.ss" (file-options no-fail))])
    (and (port? p) (output-port? p) (begin (close-port p) #t)))
  (error? ; file already exists
    (open-file-output-port "testfile.ss"))
  (error? ; file already exists
    (open-file-output-port "testfile.ss" (file-options compressed)))
  (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
    (and (port? p) (output-port? p) (begin (close-port p) #t)))
  (let ([p (open-file-output-port "testfile.ss" (file-options no-fail))])
    (and (port? p) (output-port? p) (begin (close-port p) #t)))
  (let ([p (open-file-output-port "testfile.ss" (file-options no-fail))])
    (put-bytevector p (native-string->bytevector "\"hello"))
    (close-port p)
    (let ([p (open-file-output-port "testfile.ss" (file-options no-fail no-truncate append))])
      (put-bytevector p (native-string->bytevector " there\""))
      (close-port p)
      (let ([p (open-file-input-port "testfile.ss")])
        (and (equal? (get-bytevector-all p) (native-string->bytevector "\"hello there\""))
             (eof-object? (get-u8 p))
             (begin (close-port p)
                    #t)))))
  (let ([p (let loop () (if (file-exists? "testfile.ss")
                            (begin (delete-file "testfile.ss" #f) (loop))
                            (open-file-output-port "testfile.ss")))])
    (for-each (lambda (x)
                (put-bytevector p (native-string->bytevector x))
                (put-bytevector p (native-string->bytevector " ")))
              '("a" "b" "c" "d" "e"))
    (put-bytevector p (native-string->bytevector "\n"))
    (close-port p)
    #t)
  (equal? (let ([p (open-file-input-port "testfile.ss")])
            (let f ([x (get-u8 p)])
              (if (eof-object? x)
                  (begin (close-port p) '())
                  (cons (integer->char x) (f (get-u8 p))))))
          (if (eq? (native-eol-style) 'crlf)
              '(#\a #\space #\b #\space #\c #\space
                #\d #\space #\e #\space #\return #\newline)
              '(#\a #\space #\b #\space #\c #\space
                #\d #\space #\e #\space #\newline)))
  (error? (call-with-port 3 values))
  (error? (call-with-port (current-input-port) 'a))
  (equal? (call-with-values
              (lambda ()
                (call-with-port
                 (open-file-output-port "testfile.ss" (file-options replace))
                 (lambda (p)
                   (for-each (lambda (c) (put-u8 p (char->integer c)))
                             (string->list "a b c d e"))
                   (values 1 2 3))))
            list)
          '(1 2 3))
  (equal? (call-with-port
           (open-file-input-port "testfile.ss")
           (lambda (p)
             (list->string
              (let f ()
                (let ([c (get-u8 p)])
                  (if (eof-object? c)
                      '()
                      (begin (unget-u8 p c)
                             (let ([c (get-u8 p)])
                               (cons (integer->char c) (f))))))))))
          "a b c d e")
  (equal? (call-with-port
           (open-file-input-port "testfile.ss")
           (lambda (p)
             (list->string
              (let f ()
                (let ([c (get-u8 p)])
                  (unget-u8 p c)
                  (if (eof-object? c)
                      (begin
                        (unless (and (eof-object? (lookahead-u8 p))
                                     (port-eof? p)
                                     (eof-object? (get-u8 p)))
                          (errorf #f "unget of eof apparently failed"))
                        '())
                      (let ([c (get-u8 p)])
                        (cons (integer->char c) (f)))))))))
          "a b c d e")
  (andmap (lambda (p)
            (equal? (call-with-port
                     p
                     (lambda (p)
                       (list->string
                        (let f ()
                          (let ([c (lookahead-u8 p)])
                            (if (eof-object? c)
                                '()
                                (let ([c (get-u8 p)])
                                  (cons (integer->char c) (f)))))))))
                    "a b c d e"))
          (list (open-file-input-port "testfile.ss")
                (open-bytevector-input-port '#vu8(97 32 98 32 99 32 100 32 101))
                (open-bytevector-input-port (bytevector->immutable-bytevector '#vu8(97 32 98 32 99 32 100 32 101)))))
  ; test various errors related to input ports
  (begin (set! ip (open-file-input-port "testfile.ss"))
         (and (port? ip) (input-port? ip)))
  (error? ; unget can only follow get
    (unget-u8 ip 40))
  (eqv? (get-u8 ip) (char->integer #\a))
  (begin (unget-u8 ip (char->integer #\a)) (eqv? (get-u8 ip) (char->integer #\a)))
  (error? (put-u8 ip (char->integer #\a)))
  (error? (put-bytevector ip #vu8()))
  (error? (flush-output-port ip))
  (begin (close-port ip) #t)
  (begin (close-port ip) #t)
  (error? (port-eof? ip))
  (error? (input-port-ready? ip))
  (error? (get-u8? ip))
  (error? (lookahead-u8? ip))
  (error? (unget-u8? ip))
  (error? (get-bytevector-n ip 1))
  (error? (get-bytevector-n! ip (make-bytevector 10) 0 10))
  (error? (get-bytevector-some ip))
  (error? (get-bytevector-all ip))
  ; test various errors related to output ports
  (begin (set! op (open-file-output-port "testfile.ss" (file-options replace)))
         (and (port? op) (output-port? op)))
  (error? (input-port-ready? op))
  (error? (lookahead-u8 op))
  (error? (get-u8 op))
  (error? (unget-u8 op 40))
  (error? (get-bytevector-n op 1))
  (error? (get-bytevector-n! op (make-bytevector 10) 0 10))
  (error? (get-bytevector-some op))
  (error? (get-bytevector-all op))
  (begin (close-port op) #t)
  (begin (close-port op) #t)
  (error? (put-u8 op (char->integer #\a)))
  (error? (put-bytevector op #vu8(1)))
  (error? (flush-output-port op))

  (let ([s (native-string->bytevector "hi there, mom!")])
    (let ([ip (open-bytevector-input-port s)])
      (let-values ([(op op-ex) (open-bytevector-output-port)])
        (do ([c (get-u8 ip) (get-u8 ip)])
            ((eof-object? c)
             (equal? (op-ex) s))
             (unget-u8 ip c)
             (put-u8 op (get-u8 ip))))))

  (error? (eof-object #!eof))
  (eq? (call-with-port (open-bytevector-input-port #vu8()) get-u8) #!eof)
  (eq? (call-with-port (open-bytevector-input-port #vu8()) get-u8) (eof-object))
  (eq? (eof-object) #!eof)
  (let ([s (native-string->bytevector "hi there, mom!")])
    (equal?
      (call-with-port (open-bytevector-input-port s)
        (lambda (i)
          (call-with-bytevector-output-port
            (lambda (o)
              (do ([c (get-u8 i) (get-u8 i)])
                  ((eof-object? c))
                (unget-u8 i c)
                (put-u8 o (get-u8 i)))))))
      s))

  ; the following makes sure that call-with-port closes the at least on
  ; systems which restrict the number of open ports to less than 2048
  (let ([filename "testfile.ss"])
    (let loop ((i 2048))
      (or (zero? i)
          (begin
            (call-with-port
             (open-file-output-port filename (file-options replace))
             (lambda (p) (put-u8 p (quotient i 256)) (put-u8 p (modulo i 256))))
            (and (eq? (call-with-port
                       (open-file-input-port filename)
                       (lambda (p)
                         (let* ([hi (get-u8 p)]
                                [lo (get-u8 p)])
                           (+ (* 256 hi) lo))))
                      i)
                 (loop (- i 1)))))))
  (begin
    (close-input-port #%$console-input-port)
    (not (port-closed? #%$console-input-port)))
  (begin
    (close-output-port #%$console-output-port)
    (not (port-closed? #%$console-output-port)))
 )

(mat port-operations1
  (error? ; incorrect number of arguments
    (open-file-input-port))
  (error? ; furball is not a string
    (open-file-input-port 'furball))
  (error? ; not a file-options object
    (open-file-input-port "testfile.ss" '()))
  (error? ; not a valid buffer mode
    (open-file-input-port "testfile.ss" (file-options) 17))
  (error? ; not a transcoder
    (open-file-input-port "testfile.ss" (file-options) (buffer-mode block) 'wow))
  (error? ; incorrect number of arguments
    (open-file-input-port "testfile.ss" (file-options) (buffer-mode block) #f 'now?))
  (error? ; cannot open
    (open-file-input-port "/probably/not/a/good/path"))
  (error? ; cannot open
    (open-file-input-port "/probably/not/a/good/path" (file-options compressed)))
  (error? ; invalid options
    (open-file-input-port "testfile.ss" (file-options uncompressed)))
  (error? ; invalid options
    (open-file-input-port "testfile.ss" (file-options truncate)))
  (error? ; incorrect number of arguments
    (open-file-output-port))
  (error? ; furball is not a string
    (open-file-output-port 'furball))
  (error? ; not a file-options object
    (open-file-output-port "testfile.ss" '(no-create)))
  (error? ; not a valid buffer mode
    (open-file-output-port "testfile.ss" (file-options) 17))
  (error? ; not a transcoder
    (open-file-output-port "testfile.ss" (file-options) (buffer-mode block) 'wow))
  (error? ; incorrect number of arguments
    (open-file-output-port "testfile.ss" (file-options) (buffer-mode block) #f 'now?))
  (error? ; cannot open
    (open-file-output-port "/probably/not/a/good/path"))
  (error? ; invalid options
    (open-file-output-port "testfile.ss" (file-options uncompressed)))
  (error? ; invalid options
    (open-file-output-port "testfile.ss" (file-options truncate)))
  (error? ; incorrect number of arguments
    (open-file-input/output-port))
  (error? ; furball is not a string
    (open-file-input/output-port 'furball))
  (error? ; not a file-options object
    (open-file-input/output-port "testfile.ss" '(no-create)))
  (error? ; not a valid buffer mode
    (open-file-input/output-port "testfile.ss" (file-options) 17))
  (error? ; not a transcoder
    (open-file-input/output-port "testfile.ss" (file-options) (buffer-mode block) 'wow))
  (error? ; incorrect number of arguments
    (open-file-input/output-port "testfile.ss" (file-options) (buffer-mode block) #f 'now?))
  (error? ; cannot open
    (open-file-input/output-port "/probably/not/a/good/path"))
  (error? ; invalid options
    (open-file-input/output-port "testfile.ss" (file-options uncompressed)))
  (error? ; invalid options
    (open-file-input/output-port "testfile.ss" (file-options truncate)))
  (begin (delete-file "testfile.ss") #t)
  (error? ; no such file
    (open-file-input-port "testfile.ss"))
  (error? ; no such file
    (open-file-output-port "testfile.ss" (file-options no-create)))
  (error? ; no such file
    (open-file-input/output-port "testfile.ss" (file-options no-create)))
  (begin (mkdir "testfile.ss") #t)
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testfile.ss"))])
    (open-file-output-port "testfile.ss" (file-options no-create)))
  (guard (c [(and (i/o-filename-error? c)
                  (equal? (i/o-error-filename c) "testfile.ss"))])
    (open-file-input/output-port "testfile.ss" (file-options no-create)))
  (begin (delete-directory "testfile.ss") #t)
  (begin
    (define $ppp (open-file-input/output-port "testfile.ss" (file-options replace)))
    (and (input-port? $ppp) (output-port? $ppp) (port? $ppp)))
  (error? (set-port-length! $ppp -3))
  (error? (set-port-length! $ppp 'all-the-way))
  (eof-object?
   (begin
     (set-port-length! $ppp 0)
     (set-port-position! $ppp 0)
     (put-bytevector $ppp (native-string->bytevector "hello"))
     (flush-output-port $ppp)
     (get-u8 $ppp)))
  (equal? (begin (set-port-position! $ppp 0) (get-bytevector-all $ppp))
          (native-string->bytevector "hello"))
  (eqv? (begin
          (put-bytevector $ppp (native-string->bytevector "goodbye\n"))
          (truncate-port $ppp 9)
          (port-position $ppp))
        9)
  (eof-object? (get-u8 $ppp))
  (eqv? (begin (set-port-position! $ppp 0) (port-position $ppp)) 0)
  (equal? (get-bytevector-all $ppp) (native-string->bytevector "hellogood"))
  (eqv? (begin
          (put-bytevector $ppp (native-string->bytevector "byebye\n"))
          (truncate-port $ppp 0)
          (port-position $ppp))
        0)
  (eof-object? (get-u8 $ppp))
  (eof-object?
   (begin
     (close-port $ppp)
     (let ([ip (open-file-input-port "testfile.ss")])
       (let ([c (get-u8 ip)])
         (close-port $ppp)
         (close-port ip)
         c))))
  (error?
   (let ([ip (open-file-input-port "testfile.ss")])
     (dynamic-wind
         void
         (lambda () (truncate-port ip))
         (lambda () (close-port ip)))))
  (error? (truncate-port 'animal-crackers))
  (error? (truncate-port))
  (error? (truncate-port $ppp))
  (let-values ([(op get) (open-bytevector-output-port)])
    (and (= (port-position op) 0)
         (= (port-length op) 0)
         (do ([i 4000 (fx- i 1)])
             ((fx= i 0) #t)
           (put-bytevector op (string->utf8 "hello")))
         (= (port-length op) 20000)
         (= (port-position op) 20000)
         (begin (set-port-position! op 5000) #t)
         (= (port-position op) 5000)
         (= (port-length op) 20000)
         (begin (truncate-port op) #t)
         (= (port-position op) 0)
         (= (port-length op) 0)
         (begin (truncate-port op 17) #t)
         (= (port-position op) 17)
         (= (port-length op) 17)
         (begin (put-bytevector op (string->utf8 "okay")) #t)
         (= (port-position op) 21)
         (= (port-length op) 21)
         (let ([bv (get)])
           (and (= (char->integer #\o) (bytevector-u8-ref bv 17))
                (= (char->integer #\k) (bytevector-u8-ref bv 18))
                (= (char->integer #\a) (bytevector-u8-ref bv 19))
                (= (char->integer #\y) (bytevector-u8-ref bv 20))))
         (= (port-position op) 0)
         (= (port-length op) 0)
         (begin (put-u8 op (char->integer #\a))
                (put-u8 op (char->integer #\newline))
                #t)
         (= (port-position op) 2)
         (equal? (get) (string->utf8 "a\n"))))
  (let ([ip (open-bytevector-input-port (native-string->bytevector "beam me up, scotty!"))]
        [bv (make-bytevector 10)])
    (and (= (port-position ip) 0)
         (= (port-length ip) 19)
         (not (eof-object? (lookahead-u8 ip)))
         (equal? (get-bytevector-n ip 4) (native-string->bytevector "beam"))
         (= (port-position ip) 4)
         (not (eof-object? (lookahead-u8 ip)))
         (equal? (get-bytevector-n! ip bv 0 10) 10)
         (equal? bv (native-string->bytevector " me up, sc"))
         (= (port-position ip) 14)
         (equal? (get-bytevector-n! ip bv 0 10) 5)
         (equal? bv (native-string->bytevector "otty!p, sc"))
         (= (port-position ip) 19)
         (eof-object? (lookahead-u8 ip))
         (eof-object? (get-u8 ip))
         (eof-object? (get-bytevector-n! ip bv 0 10))
         (= (get-bytevector-n! ip bv 0 0) 0) ;; TODO: check w/ Kent about this
         (begin
           (set-port-position! ip 10)
           (= (port-position ip) 10))
         (equal? (get-bytevector-n! ip bv 0 10) 9)
         (equal? bv (native-string->bytevector ", scotty!c"))))
)

(mat port-operations2
  (equal?
    (let* ([op (open-file-output-port "testfile.ss" (file-options replace) (buffer-mode none))]
           [ip (open-file-input-port "testfile.ss")])
      (put-u8 op 97)
      (let* ([b1 (get-u8 ip)] [b2 (get-u8 ip)])
        (put-u8 op 98)
        (let* ([b3 (get-u8 ip)] [b4 (get-u8 ip)])
          (put-u8 op 99)
          (let ([b5 (get-u8 ip)])
            (close-port op)
              (let ([b6 (get-u8 ip)])
                (close-port ip)
                (list b1 b2 b3 b4 b5 b6))))))
    '(97 #!eof 98 #!eof 99 #!eof))
  (equal?
    (let* ([op (open-file-output-port "testfile.ss" (file-options replace) (buffer-mode none))]
           [ip (open-file-input-port "testfile.ss")])
      (let ([eof1? (port-eof? ip)])
        (put-u8 op 97)
       ; the port-eof? call above buffers the eof, so b1 should be #!eof
        (let* ([b1 (get-u8 ip)] [b2 (get-u8 ip)])
          (put-u8 op 98)
          (let* ([eof2? (port-eof? ip)] [b3 (get-u8 ip)])
            (let ([b4 (get-u8 ip)])
              (put-u8 op 99)
              (let* ([b5 (get-u8 ip)])
                (close-port op)
                (let* ([b6 (get-u8 ip)] [eof3? (port-eof? ip)])
                  (close-port ip)
                  (list eof1? b1 b2 eof2? b3 b4 b5 b6 eof3?))))))))
    '(#t #!eof 97 #f 98 #!eof 99 #!eof #t))
  (equal?
   ; following assumes block buffering really doesn't cause any writes until
   ; at least after a few bytes have been written
    (let* ([op (open-file-output-port "testfile.ss" (file-options replace) (buffer-mode block))]
           [ip (open-file-input-port "testfile.ss")])
      (put-u8 op 97)
      (let ([b1 (get-u8 ip)])
        (put-u8 op 98)
        (let ([b2 (get-u8 ip)])
          (close-port op)
          (let* ([b3 (get-u8 ip)] [b4 (get-u8 ip)] [b5 (get-u8 ip)])
            (close-port ip)
            (list b1 b2 b3 b4 b5)))))
    '(#!eof #!eof 97 98 #!eof))
 ; test switching between input and output modes
 ; should be adapted for textual ports
  (equal?
    (begin
      (call-with-port
        (open-file-output-port "testfile.ss" (file-options replace))
        (lambda (p) (put-bytevector p #vu8(1 2 3 4 5))))
      (let ([iop (open-file-input/output-port "testfile.ss"
                   (file-options no-fail no-truncate))])
        (let ([b1 (get-u8 iop)])
          (put-u8 iop 17)
          (let ([b2 (get-u8 iop)])
            (close-port iop)
            (list b1 b2
              (call-with-port
                (open-file-input-port "testfile.ss")
                get-bytevector-all))))))
    '(1 3 #vu8(1 17 3 4 5)))
 ; test switching between input and output modes
 ; old implementation is broken---uncomment for new implementation
 ; and move to set of mats testing convenience i/o
  #;(equal?
    (begin
      (with-output-to-file "testfile.ss"
        (lambda () (display "hi there"))
        'replace)
      (let ([iop (open-input-output-file "testfile.ss")])
        (let ([c1 (read-char iop)])
          (write-char #\! iop)
          (let ([c2 (read-char iop)])
            (close-port iop)
            (list c1 c2
              (with-input-from-file "testfile.ss"
                (lambda ()
                  (list->string
                    (let f ()
                      (let ([c (read-char)])
                        (if (eof-object? c)
                            '()
                            (cons c (f)))))))))))))
    '(#\h #\space "h! there"))
  (equal?
    (let-values ([(p g) (open-string-output-port)])
      (fresh-line p)
      (fresh-line p)
      (display "hello" p)
      (fresh-line p)
      (fresh-line p)
      (newline p)
      (fresh-line p)
      (display "goodbye" p)
      (newline p)
      (fresh-line p)
      (g))
    "hello\n\ngoodbye\n")
 ; check for bug fix in transcoded-port-put-some
  (let f ([n 1000])
    (or (fx= n 0)
        (begin
          (let ([op (open-file-output-port "testfile.ss" (file-options replace)
                      (buffer-mode line) (native-transcoder))])
            (do ([i 1000 (- i 1)])
                ((fx= i 0))
              (display #!eof op))
            (close-port op))
          (and (equal? (call-with-port
                         (open-file-input-port "testfile.ss" (file-options)
                           (buffer-mode block) (native-transcoder))
                         get-string-all)
                       (apply string-append (make-list 1000 "#!eof")))
               (f (- n 1))))))
)

(mat port-operations3
  (error? (file-port? "not a port"))
  (error? (port-file-descriptor 'oops))
  (error? (port-file-descriptor (open-input-string "hello")))
  (or (threaded?) (file-port? (console-input-port)))
  (or (threaded?) (file-port? (console-output-port)))
  (not (file-port? (open-input-string "hello")))
  (or (threaded?) (= (port-file-descriptor (console-input-port)) 0))
  (or (threaded?) (= (port-file-descriptor (console-output-port)) 1))
  (> (let ([ip (open-input-file prettytest.ss)])
       (let ([n (and (file-port? ip) (port-file-descriptor ip))])
         (close-port ip)
         n))
     1)
  (> (let ([ip (open-input-file prettytest.ss 'compressed)])
       (let ([n (and (file-port? ip) (port-file-descriptor ip))])
         (close-port ip)
         n))
     1)
  (> (let ([op (open-output-file "testfile.ss" '(replace))])
       (let ([n (and (file-port? op) (port-file-descriptor op))])
         (close-port op)
         n))
     1)
  (> (let ([op (open-output-file "testfile.ss" '(replace compressed))])
       (let ([n (and (file-port? op) (port-file-descriptor op))])
         (close-port op)
         n))
     1)
 )

(if (embedded?)
    (mat iconv-codec
      (error? (errorf 'iconv-codec "-73 is not a string"))
      (error? (errorf 'transcoded-port "unsupported encoding almost certainly bogus"))
      (error? (errorf 'close-port "iconv CP1252 codec cannot encode #\\x3BB"))
      (error? (errorf 'close-port "iconv CP1252 codec cannot encode #\\newline with eol-style ls"))
      (error? (errorf 'close-port "latin-1 codec cannot encode #\\newline with eol-style ls")))
    (mat iconv-codec
      (error? ; invalid codec
        (iconv-codec -73))
      (error? ; unsupported encoding
        (let ()
          (define codec (iconv-codec "almost certainly bogus"))
          (define transcoder
            (make-transcoder codec
              (eol-style none)
              (error-handling-mode ignore)))
          (define-values (bp get) (open-bytevector-output-port))
          (define op (transcoded-port bp transcoder))
          (newline op)
          (close-port op)))
      (let ()
        (define codec (iconv-codec "UTF-8"))
        (define transcoder
          (make-transcoder codec
            (eol-style none)
            (error-handling-mode ignore)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (define p1)
        (define p2)
        (define p3)
        (define p4)
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              (make-transcoder (utf-8-codec) (eol-style none)
                                (error-handling-mode raise)))
              (lambda (ip)
                (set! p1 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p2 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              (lambda (ip)
                (set! p3 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p4 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (eq? p1 0)
          (eq? p2 20)
          (eq? p3 0)
          (eq? p4 20)))
      (let () ; same but eol-style lf
        (define codec (iconv-codec "UTF-8"))
        (define transcoder
          (make-transcoder codec
            (eol-style lf)
            (error-handling-mode ignore)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (define p1)
        (define p2)
        (define p3)
        (define p4)
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              (make-transcoder (utf-8-codec) (eol-style lf)
                                (error-handling-mode raise)))
              (lambda (ip)
                (set! p1 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p2 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              (lambda (ip)
                (set! p3 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p4 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (eq? p1 0)
          (eq? p2 20)
          (eq? p3 0)
          (eq? p4 20)))
      (let () ; same but eol-style crlf
        (define codec (iconv-codec "UTF-8"))
        (define transcoder
          (make-transcoder codec
            (eol-style crlf)
            (error-handling-mode ignore)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (define p1)
        (define p2)
        (define p3)
        (define p4)
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              (make-transcoder (utf-8-codec) (eol-style crlf)
                                (error-handling-mode raise)))
              (lambda (ip)
                (set! p1 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p2 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              (lambda (ip)
                (set! p3 (port-position ip))
                (let ([s (get-string-all ip)])
                  (set! p4 (port-position ip))
                  s)))
            "\nhello l\x0;ambda:\n\x3bb;!\n")
          (eq? p1 0)
          (eq? p2 23)
          (eq? p3 0)
          (eq? p4 23)))
      (let ()
        (define codec (iconv-codec "GB18030"))
        (define transcoder
          (make-transcoder codec
            (eol-style none)
            (error-handling-mode raise)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            #vu8(#x0a #x68 #x65 #x6c #x6c #x6f #x20 #x6c #x00 #x61 #x6d #x62 #x64 #x61 #x3a #x0a #xa6 #xcb #x21 #x0a))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              get-string-all)
            "\nhello l\x0;ambda:\n\x3bb;!\n")))
      (let ()
        (define codec (iconv-codec "CP1252"))
        (define transcoder
          (make-transcoder codec
            (eol-style none)
            (error-handling-mode replace)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            #vu8(#x0a #x68 #x65 #x6c #x6c #x6f #x20 #x6c #x00 #x61 #x6d #x62 #x64 #x61 #x3a #x0a #x3f #x21 #x0a))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              get-string-all)
            "\nhello l\x0;ambda:\n?!\n")))
      (let () ; same but eol-style lf
        (define codec (iconv-codec "CP1252"))
        (define transcoder
          (make-transcoder codec
            (eol-style lf)
            (error-handling-mode replace)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            #vu8(#x0a #x68 #x65 #x6c #x6c #x6f #x20 #x6c #x00 #x61 #x6d #x62 #x64 #x61 #x3a #x0a #x3f #x21 #x0a))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              get-string-all)
            "\nhello l\x0;ambda:\n?!\n")))
      (let () ; same but eol-style crlf
        (define codec (iconv-codec "CP1252"))
        (define transcoder
          (make-transcoder codec
            (eol-style crlf)
            (error-handling-mode replace)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            #vu8(#x0d #x0a #x68 #x65 #x6c #x6c #x6f #x20 #x6c #x00 #x61 #x6d #x62 #x64 #x61 #x3a #x0d #x0a #x3f #x21 #x0d #x0a))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              get-string-all)
            "\nhello l\x0;ambda:\n?!\n")))
      (let ()
        (define codec (iconv-codec "CP1252"))
        (define transcoder
          (make-transcoder codec
            (eol-style none)
            (error-handling-mode ignore)))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace)
            (buffer-mode line)
            transcoder))
        (newline op)
        (display "hello l\x0;ambda:\n\x3bb;!\n" op)
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            #vu8(#x0a #x68 #x65 #x6c #x6c #x6f #x20 #x6c #x00 #x61 #x6d #x62 #x64 #x61 #x3a #x0a #x21 #x0a))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss" (file-options)
                              (buffer-mode block)
                              transcoder)
              get-string-all)
            "\nhello l\x0;ambda:\n!\n")))
      (error? ; encoding error
        (let-values ([(bp get) (open-bytevector-output-port)])
          (define codec (iconv-codec "CP1252"))
          (define transcoder
            (make-transcoder codec
              (eol-style none)
              (error-handling-mode raise)))
          (define op (transcoded-port bp transcoder))
          (newline op)
          (display "hello l\x0;ambda: \x3bb;!\n" op)
          (close-port op)))
      (error? ; encoding error
        (let-values ([(bp get) (open-bytevector-output-port)])
          (define codec (iconv-codec "CP1252"))
          (define transcoder
            (make-transcoder codec
              (eol-style ls)
              (error-handling-mode raise)))
          (define op (transcoded-port bp transcoder))
          (newline op)
          (close-port op)))
      ; some (older?) versions of iconv don't handle unassigned code-page 1252
      ; characters properly.  c'est la vie.
      #;(let ()
          (define bp (open-bytevector-input-port #vu8(#x80 #x81 #x82 #x8c #x8d #x8e #x8f #x90 #x91 #x9c #x9d #x9e)))
          (define codec (iconv-codec "CP1252"))
          (define transcoder
            (make-transcoder codec
              (eol-style none)
              (error-handling-mode replace)))
          (define ip (transcoded-port bp transcoder))
          (equal?
            (get-string-all ip)
            "\x20ac;\xfffd;\x201a;\x0152;\xfffd;\x017d;\xfffd;\xfffd;\x2018;\x0153;\xfffd;\x017e;"))
      #;(let ()
          (define bp (open-bytevector-input-port #vu8(#x80 #x81 #x82 #x8c #x8d #x8e #x8f #x90 #x91 #x9c #x9d #x9e)))
          (define codec (iconv-codec "CP1252"))
          (define transcoder
            (make-transcoder codec
              (eol-style none)
              (error-handling-mode ignore)))
          (define ip (transcoded-port bp transcoder))
          (equal?
            (get-string-all ip)
            "\x20ac;\x201a;\x0152;\x017d;\x2018;\x0153;\x017e;"))
      #;(error? ; decoding error
          (let ()
            (define bp (open-bytevector-input-port #vu8(#x80 #x81 #x82 #x8c #x8d #x8e #x8f #x90 #x91 #x9c #x9d #x9e)))
            (define codec (iconv-codec "CP1252"))
            (define transcoder
              (make-transcoder codec
                (eol-style none)
                (error-handling-mode raise)))
            (define ip (transcoded-port bp transcoder))
            (equal?
              (get-string-all ip)
              "\x20ac;\xfffd;\x201a;\x0152;\xfffd;\x017d;\xfffd;\xfffd;\x2018;\x0153;\xfffd;\x017e;")))
      (let () ; SBCS CP1252
        (define cp1252
          '((#x00 #x0000) (#x01 #x0001) (#x02 #x0002) (#x03 #x0003)
                          (#x04 #x0004) (#x05 #x0005) (#x06 #x0006) (#x07 #x0007)
                          (#x08 #x0008) (#x09 #x0009) (#x0A #x000A) (#x0B #x000B)
                          (#x0C #x000C) (#x0D #x000D) (#x0E #x000E) (#x0F #x000F)
                          (#x10 #x0010) (#x11 #x0011) (#x12 #x0012) (#x13 #x0013)
                          (#x14 #x0014) (#x15 #x0015) (#x16 #x0016) (#x17 #x0017)
                          (#x18 #x0018) (#x19 #x0019) (#x1A #x001A) (#x1B #x001B)
                          (#x1C #x001C) (#x1D #x001D) (#x1E #x001E) (#x1F #x001F)
                          (#x20 #x0020) (#x21 #x0021) (#x22 #x0022) (#x23 #x0023)
                          (#x24 #x0024) (#x25 #x0025) (#x26 #x0026) (#x27 #x0027)
                          (#x28 #x0028) (#x29 #x0029) (#x2A #x002A) (#x2B #x002B)
                          (#x2C #x002C) (#x2D #x002D) (#x2E #x002E) (#x2F #x002F)
                          (#x30 #x0030) (#x31 #x0031) (#x32 #x0032) (#x33 #x0033)
                          (#x34 #x0034) (#x35 #x0035) (#x36 #x0036) (#x37 #x0037)
                          (#x38 #x0038) (#x39 #x0039) (#x3A #x003A) (#x3B #x003B)
                          (#x3C #x003C) (#x3D #x003D) (#x3E #x003E) (#x3F #x003F)
                          (#x40 #x0040) (#x41 #x0041) (#x42 #x0042) (#x43 #x0043)
                          (#x44 #x0044) (#x45 #x0045) (#x46 #x0046) (#x47 #x0047)
                          (#x48 #x0048) (#x49 #x0049) (#x4A #x004A) (#x4B #x004B)
                          (#x4C #x004C) (#x4D #x004D) (#x4E #x004E) (#x4F #x004F)
                          (#x50 #x0050) (#x51 #x0051) (#x52 #x0052) (#x53 #x0053)
                          (#x54 #x0054) (#x55 #x0055) (#x56 #x0056) (#x57 #x0057)
                          (#x58 #x0058) (#x59 #x0059) (#x5A #x005A) (#x5B #x005B)
                          (#x5C #x005C) (#x5D #x005D) (#x5E #x005E) (#x5F #x005F)
                          (#x60 #x0060) (#x61 #x0061) (#x62 #x0062) (#x63 #x0063)
                          (#x64 #x0064) (#x65 #x0065) (#x66 #x0066) (#x67 #x0067)
                          (#x68 #x0068) (#x69 #x0069) (#x6A #x006A) (#x6B #x006B)
                          (#x6C #x006C) (#x6D #x006D) (#x6E #x006E) (#x6F #x006F)
                          (#x70 #x0070) (#x71 #x0071) (#x72 #x0072) (#x73 #x0073)
                          (#x74 #x0074) (#x75 #x0075) (#x76 #x0076) (#x77 #x0077)
                          (#x78 #x0078) (#x79 #x0079) (#x7A #x007A) (#x7B #x007B)
                          (#x7C #x007C) (#x7D #x007D) (#x7E #x007E) (#x7F #x007F)
                          (#x80 #x20AC) (#x82 #x201A) (#x83 #x0192) (#x84 #x201E)
                          (#x85 #x2026) (#x86 #x2020) (#x87 #x2021) (#x88 #x02C6)
                          (#x89 #x2030) (#x8A #x0160) (#x8B #x2039) (#x8C #x0152)
                          (#x8E #x017D) (#x91 #x2018) (#x92 #x2019) (#x93 #x201C)
                          (#x94 #x201D) (#x95 #x2022) (#x96 #x2013) (#x97 #x2014)
                          (#x98 #x02DC) (#x99 #x2122) (#x9A #x0161) (#x9B #x203A)
                          (#x9C #x0153) (#x9E #x017E) (#x9F #x0178) (#xA0 #x00A0)
                          (#xA1 #x00A1) (#xA2 #x00A2) (#xA3 #x00A3) (#xA4 #x00A4)
                          (#xA5 #x00A5) (#xA6 #x00A6) (#xA7 #x00A7) (#xA8 #x00A8)
                          (#xA9 #x00A9) (#xAA #x00AA) (#xAB #x00AB) (#xAC #x00AC)
                          (#xAD #x00AD) (#xAE #x00AE) (#xAF #x00AF) (#xB0 #x00B0)
                          (#xB1 #x00B1) (#xB2 #x00B2) (#xB3 #x00B3) (#xB4 #x00B4)
                          (#xB5 #x00B5) (#xB6 #x00B6) (#xB7 #x00B7) (#xB8 #x00B8)
                          (#xB9 #x00B9) (#xBA #x00BA) (#xBB #x00BB) (#xBC #x00BC)
                          (#xBD #x00BD) (#xBE #x00BE) (#xBF #x00BF) (#xC0 #x00C0)
                          (#xC1 #x00C1) (#xC2 #x00C2) (#xC3 #x00C3) (#xC4 #x00C4)
                          (#xC5 #x00C5) (#xC6 #x00C6) (#xC7 #x00C7) (#xC8 #x00C8)
                          (#xC9 #x00C9) (#xCA #x00CA) (#xCB #x00CB) (#xCC #x00CC)
                          (#xCD #x00CD) (#xCE #x00CE) (#xCF #x00CF) (#xD0 #x00D0)
                          (#xD1 #x00D1) (#xD2 #x00D2) (#xD3 #x00D3) (#xD4 #x00D4)
                          (#xD5 #x00D5) (#xD6 #x00D6) (#xD7 #x00D7) (#xD8 #x00D8)
                          (#xD9 #x00D9) (#xDA #x00DA) (#xDB #x00DB) (#xDC #x00DC)
                          (#xDD #x00DD) (#xDE #x00DE) (#xDF #x00DF) (#xE0 #x00E0)
                          (#xE1 #x00E1) (#xE2 #x00E2) (#xE3 #x00E3) (#xE4 #x00E4)
                          (#xE5 #x00E5) (#xE6 #x00E6) (#xE7 #x00E7) (#xE8 #x00E8)
                          (#xE9 #x00E9) (#xEA #x00EA) (#xEB #x00EB) (#xEC #x00EC)
                          (#xED #x00ED) (#xEE #x00EE) (#xEF #x00EF) (#xF0 #x00F0)
                          (#xF1 #x00F1) (#xF2 #x00F2) (#xF3 #x00F3) (#xF4 #x00F4)
                          (#xF5 #x00F5) (#xF6 #x00F6) (#xF7 #x00F7) (#xF8 #x00F8)
                          (#xF9 #x00F9) (#xFA #x00FA) (#xFB #x00FB) (#xFC #x00FC)
                          (#xFD #x00FD) (#xFE #x00FE) (#xFF #x00FF)))
        (define transcoder
          (make-transcoder (iconv-codec "CP1252")
            (eol-style none)
            (error-handling-mode raise)))
        (define ls
          (append cp1252
            (let ([v (list->vector cp1252)])
              (let f ([n 100000])
                (if (fx= n 0)
                    '()
                    (cons
                      (vector-ref v (random (vector-length v)))
                      (f (fx- n 1))))))))
        (define s (apply string (map integer->char (map cadr ls))))
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace) (buffer-mode block)
            transcoder))
        #;(put-string op s)
        (let loop ([i 0] [n (string-length s)])
          (unless (fx= n 0)
            (let ([k (fx+ (random n) 1)])
              (put-string op s i k)
              (loop (fx+ i k) (fx- n k)))))
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss")
              get-bytevector-all)
            (apply bytevector (map car ls)))
          (equal?
            (call-with-port (open-file-input-port "testfile.ss"
                              (file-options) (buffer-mode block)
                              transcoder)
              #;get-string-all
              (lambda (ip)
                (let ([t (make-string (string-length s))])
                  (let loop ([i 0] [n (string-length s)])
                    (unless (fx= n 0)
                      (let ([k (fx+ (random n) 1)])
                        (get-string-n! ip t i k)
                        (loop (fx+ i k) (fx- n k)))))
                  t)))
            s)))
      (let () ; MBCS UTF-8
        (define transcoder
          (make-transcoder (iconv-codec "UTF-8")
            (eol-style none)
            (error-handling-mode raise)))
        (define ls1
          (let f ([i 0])
            (if (fx= i #x11000)
                '()
                (if (fx= i #xD800)
                    (f #xE000)
                    (cons i (f (fx+ i 1)))))))
        (define ls2
          (let f ([n 1000000])
            (if (fx= n 0)
                '()
                (cons
                  (let ([n (random (- #x110000 (- #xE000 #xD800)))])
                    (if (<= #xD800 n #xDFFF)
                        (+ n (- #xE000 #xD800))
                        n))
                  (f (fx- n 1))))))
        (define s (apply string (map integer->char (append ls1 ls2))))
        #;(define s (apply string (map integer->char ls1)))
        #;(define s "hello\x1447A;")
        (define op
          (open-file-output-port "testfile.ss"
            (file-options replace) (buffer-mode block)
            transcoder))
        #;(put-string op s)
        (let loop ([i 0] [n (string-length s)])
          (unless (fx= n 0)
            (let ([k (fx+ (random n) 1)])
              (put-string op s i k)
              (loop (fx+ i k) (fx- n k)))))
        (close-port op)
        (and
          (equal?
            (call-with-port (open-file-input-port "testfile.ss"
                              (file-options) (buffer-mode block)
                              (make-transcoder (utf-8-codec) (eol-style none)
                                (error-handling-mode raise)))
              get-string-all)
            s)
          (equal?
            (call-with-port (open-file-input-port "testfile.ss"
                              (file-options) (buffer-mode block)
                              transcoder)
              #;get-string-all
              (lambda (ip)
                (let ([t (make-string (string-length s))])
                  (let loop ([i 0] [n (string-length s)])
                    (unless (fx= n 0)
                      (let ([k (fx+ (random n) 1)])
                        (get-string-n! ip t i k)
                        (loop (fx+ i k) (fx- n k)))))
                  t)))
            s)))
      (error? ; encoding error
        (let ()
          (define transcoder
            (make-transcoder (latin-1-codec)
              (eol-style ls)
              (error-handling-mode raise)))
          (define-values (bp get) (open-bytevector-output-port))
          (define op (transcoded-port bp transcoder))
          (newline op)
          (close-port op)))
      ; NB: keep this last among the iconv-codec mats
      ; close any files left open by failing iconv tests.  this is particulary
      ; important on windows when the iconv dll isn't available and where keeping
      ; file open can prevent it from being reopened.
      (begin (collect (collect-maximum-generation)) #t)
      ))

(mat port-operations4
  (begin
    (define po4-tx (make-transcoder (latin-1-codec) (eol-style none) (error-handling-mode raise)))
    #t)
  (transcoder? po4-tx)
  (not (transcoder? (latin-1-codec)))
  (eq? (call-with-port 
         (open-file-output-port "testfile.ss" (file-options replace)
           (buffer-mode block) po4-tx)
         (lambda (op) (put-string op "hi there")))
       (void))
 ; binary input port
  (begin
    (define po4-p (open-file-input-port "testfile.ss"))
    #t)
  (and (input-port? po4-p) (binary-port? po4-p) (file-port? po4-p))
  (error? (put-string po4-p "hello"))
  (error? (put-bytevector po4-p #vu8(100)))
  (error? (get-string-all po4-p))
  (error? (get-char po4-p))
  (error? (lookahead-char po4-p))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eq? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (equal? (get-bytevector-n po4-p 5) (string->bytevector "there" po4-tx))
  (eof-object? (get-bytevector-n po4-p 1))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 8)
  (not (port-has-set-port-length!? po4-p))
  (error? (set-port-length! po4-p 7))
  (eq? (close-port po4-p) (void))
 ; textual input port
  (begin
    (define po4-p
      (open-file-input-port "testfile.ss" (file-options)
        (buffer-mode block) po4-tx))
    #t)
  (and (input-port? po4-p) (textual-port? po4-p) (file-port? po4-p))
  (error? (put-string po4-p "hello"))
  (error? (put-bytevector po4-p #vu8(100)))
  (error? (get-bytevector-all po4-p))
  (error? (get-u8 po4-p))
  (error? (lookahead-u8 po4-p))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eqv? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (equal? (get-string-n po4-p 5) "there")
  (eof-object? (get-string-n po4-p 1))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 8)
  (not (port-has-set-port-length!? po4-p))
  (error? (set-port-length! po4-p 7))
  (eq? (close-port po4-p) (void))
 ; binary output port
  (begin
    (define po4-p
      (open-file-output-port "testfile.ss" (file-options replace)))
    #t)
  (and (output-port? po4-p) (binary-port? po4-p) (file-port? po4-p))
  (error? (get-string-all po4-p))
  (error? (get-char po4-p))
  (error? (lookahead-char po4-p))
  (error? (get-bytevector-all po4-p))
  (error? (get-u8 po4-p))
  (error? (lookahead-u8 po4-p))
  (error? (put-string po4-p "hello"))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eq? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (eq? (put-bytevector po4-p (string->bytevector "123456" po4-tx)) (void))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 9)
  (port-has-set-port-length!? po4-p)
  (eq? (set-port-length! po4-p 7) (void))
  (eq? (set-port-position! po4-p 0) (void))
  (eq? (put-bytevector po4-p (string->bytevector "abcd" po4-tx)) (void))
  (eq? (close-port po4-p) (void))
  (equal?
    (call-with-port
      (open-file-input-port "testfile.ss" (file-options)
        (buffer-mode block) po4-tx)
      get-string-all)
    "abcd234")
 ; textual output port
  (begin
    (define po4-p
      (open-file-output-port "testfile.ss" (file-options replace)
        (buffer-mode block) po4-tx))
    #t)
  (and (output-port? po4-p) (textual-port? po4-p) (file-port? po4-p))
  (error? (get-string-all po4-p))
  (error? (get-char po4-p))
  (error? (lookahead-char po4-p))
  (error? (get-bytevector-all po4-p))
  (error? (get-u8 po4-p))
  (error? (lookahead-u8 po4-p))
  (error? (put-bytevector po4-p #vu8()))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eq? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (eq? (put-string po4-p "abcdef") (void))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 9)
  (port-has-set-port-length!? po4-p)
  (eq? (set-port-length! po4-p 7) (void))
  (eq? (set-port-position! po4-p 0) (void))
  (eq? (put-string po4-p "1234") (void))
  (eq? (close-port po4-p) (void))
  (equal?
    (call-with-port
      (open-file-input-port "testfile.ss" (file-options)
        (buffer-mode block) po4-tx)
      get-string-all)
    "1234bcd")
 ; binary input/output port
  (begin
    (define po4-p
      (open-file-input/output-port "testfile.ss" (file-options replace)))
    #t)
  (and (output-port? po4-p) (binary-port? po4-p) (file-port? po4-p))
  (and (input-port? po4-p) (binary-port? po4-p) (file-port? po4-p))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eq? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (eq? (put-bytevector po4-p (string->bytevector "foobar" po4-tx)) (void))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 9)
  (port-has-set-port-length!? po4-p)
  (eq? (set-port-length! po4-p 7) (void))
  (eq? (set-port-position! po4-p 0) (void))
  (eq? (put-bytevector po4-p (string->bytevector "4321" po4-tx)) (void))
  (equal? (get-bytevector-all po4-p) (string->bytevector "oob" po4-tx))
  (eq? (set-port-position! po4-p 0) (void))
  (equal? (get-bytevector-all po4-p) (string->bytevector "4321oob" po4-tx))
  (eq? (close-port po4-p) (void))
  (equal?
    (call-with-port
      (open-file-input-port "testfile.ss" (file-options)
        (buffer-mode block) po4-tx)
      get-string-all)
    "4321oob")
 ; textual input/output port
  (begin
    (define po4-p
      (open-file-input/output-port "testfile.ss" (file-options replace)
        (buffer-mode block) po4-tx))
    #t)
  (and (output-port? po4-p) (textual-port? po4-p) (file-port? po4-p))
  (and (input-port? po4-p) (textual-port? po4-p) (file-port? po4-p))
  (fixnum? (port-file-descriptor po4-p))
  (port-has-port-position? po4-p)
  (eqv? (port-position po4-p) 0)
  (port-has-set-port-position!? po4-p)
  (eq? (set-port-position! po4-p 3) (void))
  (eqv? (port-position po4-p) 3)
  (eq? (put-string po4-p "abcdef") (void))
  (port-has-port-length? po4-p)
  (eqv? (port-length po4-p) 9)
  (port-has-set-port-length!? po4-p)
  (eq? (set-port-length! po4-p 7) (void))
  (eq? (set-port-position! po4-p 0) (void))
  (eq? (put-string po4-p "1234") (void))
  (equal? (get-string-all po4-p) "bcd")
  (eq? (set-port-position! po4-p 0) (void))
  (equal? (get-string-all po4-p) "1234bcd")
  (eq? (close-port po4-p) (void))
  (equal?
    (call-with-port
      (open-file-input-port "testfile.ss" (file-options)
        (buffer-mode block) po4-tx)
      get-string-all)
    "1234bcd")
)

(mat get-line
  (error? ; not a port
    (get-line "current-input-port"))
  (error? ; not a port
    (get-line 3))
  (error? ; not a textual input port
    (get-line (open-bytevector-input-port #vu8(1 2 3 4 5))))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "hello from line 1!\n")
        (display (make-string 1017 #\a))
        (display " hello from line 2!\n")
        (display "goodbye from (incomplete) line 3!"))
      'replace)
    (define $tip (open-input-file "testfile.ss"))
    #t)
  (equal? (get-line $tip) "hello from line 1!")
  (equal? (get-line $tip) (format "~a hello from line 2!" (make-string 1017 #\a)))
  (equal? (get-line $tip) "goodbye from (incomplete) line 3!")
  (eof-object? (get-line $tip))
  (eqv? (close-port $tip) (void))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (display "hello from line 1!\n")
        (display "\n")
        (display "goodbye from (complete) line 3!\n"))
      'replace)
    (define $tip (open-input-file "testfile.ss"))
    #t)
  (equal? (get-line $tip) "hello from line 1!")
  (equal? (get-line $tip) "")
  (equal? (get-line $tip) "goodbye from (complete) line 3!")
  (eof-object? (get-line $tip))
  (eqv? (close-port $tip) (void))
)

(mat low-level-port-operations
  (<= (textual-port-input-index (console-input-port))
      (textual-port-input-size (console-input-port))
      (string-length (textual-port-input-buffer (console-input-port))))
  (<= (textual-port-input-count (console-input-port))
      (string-length (textual-port-input-buffer (console-input-port))))
  (<= (textual-port-output-index (console-output-port))
      (textual-port-output-size (console-output-port))
      (string-length (textual-port-output-buffer (console-output-port))))
  (<= (textual-port-output-count (console-output-port))
      (string-length (textual-port-output-buffer (console-output-port))))
  (begin
    (define $tip (open-string-input-port "hello"))
    (define $top (let-values ([(op get) (open-string-output-port)]) (set-textual-port-output-buffer! op "hello") op))
    (define $bip (open-bytevector-input-port #vu8(1 2 3 4 5)))
    (define $bop (let-values ([(op get) (open-bytevector-output-port)]) (set-binary-port-output-buffer! op #vu8(1 2 3 4 5)) op))
    #t)
  ; textual input
  (andmap (lambda (str)
            (equal?
             (let ([ip (open-string-input-port str)])
               (let ([buffer0 (textual-port-input-buffer ip)]
                     [index0 (textual-port-input-index ip)]
                     [size0 (textual-port-input-size ip)]
                     [count0 (textual-port-input-count ip)])
                 (read-char ip)
                 (list
                  (list buffer0 index0 size0 count0)
                  (list
                   (textual-port-input-buffer ip)
                   (textual-port-input-index ip)
                   (textual-port-input-size ip)
                   (textual-port-input-count ip)))))
             '(("hello" 0 5 5) ("hello" 1 5 4))))
          (list "hello"
                (string->immutable-string "hello")))
  (equal?
    (let ([ip (open-string-input-port "hello")])
      (let ([buffer0 (textual-port-input-buffer ip)]
            [index0 (textual-port-input-index ip)]
            [size0 (textual-port-input-size ip)]
            [count0 (textual-port-input-count ip)])
        (read-char ip)
        (set-textual-port-input-buffer! ip "goodbye")
        (read-char ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (textual-port-input-buffer ip)
            (textual-port-input-index ip)
            (textual-port-input-size ip)
            (textual-port-input-count ip)))))
    '(("hello" 0 5 5) ("goodbye" 1 7 6)))
  (equal?
    (let ([ip (open-string-input-port "hello")])
      (let ([buffer0 (textual-port-input-buffer ip)]
            [index0 (textual-port-input-index ip)]
            [size0 (textual-port-input-size ip)]
            [count0 (textual-port-input-count ip)])
        (read-char ip)
        (set-textual-port-input-size! ip 4)
        (read-char ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (textual-port-input-buffer ip)
            (textual-port-input-index ip)
            (textual-port-input-size ip)
            (textual-port-input-count ip)))))
    '(("hello" 0 5 5) ("hello" 1 4 3)))
  (equal?
    (let ([ip (open-string-input-port "hello")])
      (let ([buffer0 (textual-port-input-buffer ip)]
            [index0 (textual-port-input-index ip)]
            [size0 (textual-port-input-size ip)]
            [count0 (textual-port-input-count ip)])
        (read-char ip)
        (set-textual-port-input-index! ip 4)
        (read-char ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (textual-port-input-buffer ip)
            (textual-port-input-index ip)
            (textual-port-input-size ip)
            (textual-port-input-count ip)))))
    '(("hello" 0 5 5) ("hello" 5 5 0)))
  (error? ; not a textual input port
    (textual-port-input-buffer $top))
  (error? ; not a textual input port
    (textual-port-input-buffer $bip))
  (error? ; not a textual input port
    (textual-port-input-buffer $bop))
  (error? ; not a textual input port
    (textual-port-input-buffer 75))
  (error? ; not a textual input port
    (textual-port-input-index $top))
  (error? ; not a textual input port
    (textual-port-input-index $bip))
  (error? ; not a textual input port
    (textual-port-input-index $bop))
  (error? ; not a textual input port
    (textual-port-input-index 75))
  (error? ; not a textual input port
    (textual-port-input-size $top))
  (error? ; not a textual input port
    (textual-port-input-size $bip))
  (error? ; not a textual input port
    (textual-port-input-size $bop))
  (error? ; not a textual input port
    (textual-port-input-size 75))
  (error? ; not a textual input port
    (textual-port-input-count $top))
  (error? ; not a textual input port
    (textual-port-input-count $bip))
  (error? ; not a textual input port
    (textual-port-input-count $bop))
  (error? ; not a textual input port
    (textual-port-input-count 75))
  (error? ; not a textual input port
    (set-textual-port-input-buffer! $top ""))
  (error? ; not a textual input port
    (set-textual-port-input-buffer! $bip ""))
  (error? ; not a textual input port
    (set-textual-port-input-buffer! $bop ""))
  (error? ; not a textual input port
    (set-textual-port-input-buffer! 75 ""))
  (error? ; not a textual input port
    (set-textual-port-input-index! $top 0))
  (error? ; not a textual input port
    (set-textual-port-input-index! $bip 0))
  (error? ; not a textual input port
    (set-textual-port-input-index! $bop 0))
  (error? ; not a textual input port
    (set-textual-port-input-index! 75 0))
  (error? ; not a textual input port
    (set-textual-port-input-size! $top 0))
  (error? ; not a textual input port
    (set-textual-port-input-size! $bip 0))
  (error? ; not a textual input port
    (set-textual-port-input-size! $bop 0))
  (error? ; not a textual input port
    (set-textual-port-input-size! 75 0))
  (error? ; not a string
    (set-textual-port-input-buffer! $tip #vu8(1 2 3)))
  (error? ; not a string
    (set-textual-port-input-buffer! $tip 0))
  (error? ; invalid index
    (set-textual-port-input-index! $tip "hello"))
  (error? ; invalid index
    (set-textual-port-input-index! $tip -1))
  (error? ; invalid index
    (set-textual-port-input-index! $tip 6))
  (error? ; invalid size
    (set-textual-port-input-size! $tip "hello"))
  (error? ; invalid size
    (set-textual-port-input-size! $tip -1))
  (error? ; invalid size
    (set-textual-port-input-size! $tip 6))
  ; textual output
  (equal?
    (let ([op (make-output-port (lambda args (error #f "oops")) (make-string 10 #\$))])
      (let ([buffer0 (string-copy (textual-port-output-buffer op))]
            [index0 (textual-port-output-index op)]
            [size0 (textual-port-output-size op)]
            [count0 (textual-port-output-count op)])
        (display "hey!" op)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (textual-port-output-buffer op)
            (textual-port-output-index op)
            (textual-port-output-size op)
            (textual-port-output-count op)))))
    '(("$$$$$$$$$$" 0 10 10)
      ("hey!$$$$$$" 4 10 6)))
  (equal?
    (let-values ([(op get) (open-string-output-port)])
      (let ([buffer (make-string 8 #\$)])
        (set-textual-port-output-buffer! op buffer)
        (let ([buffer0 (string-copy (textual-port-output-buffer op))]
              [index0 (textual-port-output-index op)]
              [size0 (textual-port-output-size op)]
              [count0 (textual-port-output-count op)])
          (display "yo!" op)
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (textual-port-output-buffer op)
              (textual-port-output-index op)
              (textual-port-output-size op)
              (textual-port-output-count op))))))
    '("yo!$$$$$"
      ("$$$$$$$$" 0 8 8)
      ("yo!$$$$$" 3 8 5)))
  (equal?
    (let-values ([(op get) (open-string-output-port)])
      (let ([buffer (make-string 8 #\$)])
        (set-textual-port-output-buffer! op buffer)
        (let ([buffer0 (string-copy (textual-port-output-buffer op))]
              [index0 (textual-port-output-index op)]
              [size0 (textual-port-output-size op)]
              [count0 (textual-port-output-count op)])
          (display "yo" op)
          (set-textual-port-output-buffer! op (string #\a #\b #\c))
          (display "!?" op)
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (textual-port-output-buffer op)
              (textual-port-output-index op)
              (textual-port-output-size op)
              (textual-port-output-count op))))))
    '("yo$$$$$$"
      ("$$$$$$$$" 0 8 8)
      ("!?c" 2 3 1)))
  (equal?
    (let-values ([(op get) (open-string-output-port)])
      (let ([buffer (make-string 8 #\$)])
        (set-textual-port-output-buffer! op buffer)
        (let ([buffer0 (string-copy (textual-port-output-buffer op))]
              [index0 (textual-port-output-index op)]
              [size0 (textual-port-output-size op)]
              [count0 (textual-port-output-count op)])
          (display "yo" op)
          (set-textual-port-output-index! op 4)
          (display "!?" op)
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (textual-port-output-buffer op)
              (textual-port-output-index op)
              (textual-port-output-size op)
              (textual-port-output-count op))))))
    '("yo$$!?$$"
      ("$$$$$$$$" 0 8 8)
      ("yo$$!?$$" 6 8 2)))
  (equal?
    (let-values ([(op get) (open-string-output-port)])
      (let ([buffer (make-string 8 #\$)])
        (set-textual-port-output-buffer! op buffer)
        (let ([buffer0 (string-copy (textual-port-output-buffer op))]
              [index0 (textual-port-output-index op)]
              [size0 (textual-port-output-size op)]
              [count0 (textual-port-output-count op)])
          (display "yo" op)
          (set-textual-port-output-size! op 4)
          (display "!?" op)
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (textual-port-output-buffer op)
              (textual-port-output-index op)
              (textual-port-output-size op)
              (textual-port-output-count op))))))
    '("!?$$$$$$"
      ("$$$$$$$$" 0 8 8)
      ("!?$$$$$$" 2 4 2)))
  (error? ; not a textual output port
    (textual-port-output-buffer $tip))
  (error? ; not a textual output port
    (textual-port-output-buffer $bip))
  (error? ; not a textual output port
    (textual-port-output-buffer $bop))
  (error? ; not a textual output port
    (textual-port-output-buffer 75))
  (error? ; not a textual output port
    (textual-port-output-index $tip))
  (error? ; not a textual output port
    (textual-port-output-index $bip))
  (error? ; not a textual output port
    (textual-port-output-index $bop))
  (error? ; not a textual output port
    (textual-port-output-index 75))
  (error? ; not a textual output port
    (textual-port-output-size $tip))
  (error? ; not a textual output port
    (textual-port-output-size $bip))
  (error? ; not a textual output port
    (textual-port-output-size $bop))
  (error? ; not a textual output port
    (textual-port-output-size 75))
  (error? ; not a textual output port
    (textual-port-output-count $tip))
  (error? ; not a textual output port
    (textual-port-output-count $bip))
  (error? ; not a textual output port
    (textual-port-output-count $bop))
  (error? ; not a textual output port
    (textual-port-output-count 75))
  (error? ; not a textual output port
    (set-textual-port-output-buffer! $tip ""))
  (error? ; not a textual output port
    (set-textual-port-output-buffer! $bip ""))
  (error? ; not a textual output port
    (set-textual-port-output-buffer! $bop ""))
  (error? ; not a textual output port
    (set-textual-port-output-buffer! 75 ""))
  (error? ; not a textual output port
    (set-textual-port-output-index! $tip 0))
  (error? ; not a textual output port
    (set-textual-port-output-index! $bip 0))
  (error? ; not a textual output port
    (set-textual-port-output-index! $bop 0))
  (error? ; not a textual output port
    (set-textual-port-output-index! 75 0))
  (error? ; not a textual output port
    (set-textual-port-output-size! $tip 0))
  (error? ; not a textual output port
    (set-textual-port-output-size! $bip 0))
  (error? ; not a textual output port
    (set-textual-port-output-size! $bop 0))
  (error? ; not a textual output port
    (set-textual-port-output-size! 75 0))
  (error? ; not a string
    (set-textual-port-output-buffer! $top #vu8(1 2 3)))
  (error? ; not a string
    (set-textual-port-output-buffer! $top 0))
  (error? ; invalid index
    (set-textual-port-output-index! $top "hello"))
  (error? ; invalid index
    (set-textual-port-output-index! $top -1))
  (error? ; invalid index
    (set-textual-port-output-index! $top 6))
  (error? ; invalid size
    (set-textual-port-output-size! $top "hello"))
  (error? ; invalid size
    (set-textual-port-output-size! $top -1))
  (error? ; invalid size
    (set-textual-port-output-size! $top 6))
  ; binary input
  (equal?
    (let ([ip (open-bytevector-input-port (string->utf8 "hello"))])
      (let ([buffer0 (binary-port-input-buffer ip)]
            [index0 (binary-port-input-index ip)]
            [size0 (binary-port-input-size ip)]
            [count0 (binary-port-input-count ip)])
        (get-u8 ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (binary-port-input-buffer ip)
            (binary-port-input-index ip)
            (binary-port-input-size ip)
            (binary-port-input-count ip)))))
    `((,(string->utf8 "hello") 0 5 5) (,(string->utf8 "hello") 1 5 4)))
  (equal?
    (let ([ip (open-bytevector-input-port (string->utf8 "hello"))])
      (let ([buffer0 (binary-port-input-buffer ip)]
            [index0 (binary-port-input-index ip)]
            [size0 (binary-port-input-size ip)]
            [count0 (binary-port-input-count ip)])
        (get-u8 ip)
        (set-binary-port-input-buffer! ip (string->utf8 "goodbye"))
        (get-u8 ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (binary-port-input-buffer ip)
            (binary-port-input-index ip)
            (binary-port-input-size ip)
            (binary-port-input-count ip)))))
    `((,(string->utf8 "hello") 0 5 5) (,(string->utf8 "goodbye") 1 7 6)))
  (equal?
    (let ([ip (open-bytevector-input-port (string->utf8 "hello"))])
      (let ([buffer0 (binary-port-input-buffer ip)]
            [index0 (binary-port-input-index ip)]
            [size0 (binary-port-input-size ip)]
            [count0 (binary-port-input-count ip)])
        (get-u8 ip)
        (set-binary-port-input-size! ip 3)
        (get-u8 ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (binary-port-input-buffer ip)
            (binary-port-input-index ip)
            (binary-port-input-size ip)
            (binary-port-input-count ip)))))
    `((,(string->utf8 "hello") 0 5 5) (,(string->utf8 "hello") 1 3 2)))
  (equal?
    (let ([ip (open-bytevector-input-port (string->utf8 "hello"))])
      (let ([buffer0 (binary-port-input-buffer ip)]
            [index0 (binary-port-input-index ip)]
            [size0 (binary-port-input-size ip)]
            [count0 (binary-port-input-count ip)])
        (get-u8 ip)
        (set-binary-port-input-index! ip 3)
        (get-u8 ip)
        (list
          (list buffer0 index0 size0 count0)
          (list
            (binary-port-input-buffer ip)
            (binary-port-input-index ip)
            (binary-port-input-size ip)
            (binary-port-input-count ip)))))
    `((,(string->utf8 "hello") 0 5 5) (,(string->utf8 "hello") 4 5 1)))
  (error? ; not a binary input port
    (binary-port-input-buffer $tip))
  (error? ; not a binary input port
    (binary-port-input-buffer $top))
  (error? ; not a binary input port
    (binary-port-input-buffer $bop))
  (error? ; not a binary input port
    (binary-port-input-buffer 75))
  (error? ; not a binary input port
    (binary-port-input-index $tip))
  (error? ; not a binary input port
    (binary-port-input-index $top))
  (error? ; not a binary input port
    (binary-port-input-index $bop))
  (error? ; not a binary input port
    (binary-port-input-index 75))
  (error? ; not a binary input port
    (binary-port-input-size $tip))
  (error? ; not a binary input port
    (binary-port-input-size $top))
  (error? ; not a binary input port
    (binary-port-input-size $bop))
  (error? ; not a binary input port
    (binary-port-input-size 75))
  (error? ; not a binary input port
    (binary-port-input-count $tip))
  (error? ; not a binary input port
    (binary-port-input-count $top))
  (error? ; not a binary input port
    (binary-port-input-count $bop))
  (error? ; not a binary input port
    (binary-port-input-count 75))
  (error? ; not a binary input port
    (set-binary-port-input-buffer! $tip ""))
  (error? ; not a binary input port
    (set-binary-port-input-buffer! $top ""))
  (error? ; not a binary input port
    (set-binary-port-input-buffer! $bop ""))
  (error? ; not a binary input port
    (set-binary-port-input-buffer! 75 ""))
  (error? ; not a binary input port
    (set-binary-port-input-index! $tip 0))
  (error? ; not a binary input port
    (set-binary-port-input-index! $top 0))
  (error? ; not a binary input port
    (set-binary-port-input-index! $bop 0))
  (error? ; not a binary input port
    (set-binary-port-input-index! 75 0))
  (error? ; not a binary input port
    (set-binary-port-input-size! $tip 0))
  (error? ; not a binary input port
    (set-binary-port-input-size! $top 0))
  (error? ; not a binary input port
    (set-binary-port-input-size! $bop 0))
  (error? ; not a binary input port
    (set-binary-port-input-size! 75 0))
  (error? ; not a bytevector
    (set-binary-port-input-buffer! $bip "hello"))
  (error? ; not a bytevector
    (set-binary-port-input-buffer! $bip 0))
  (error? ; invalid index
    (set-binary-port-input-index! $bip #vu8(1 2 3)))
  (error? ; invalid index
    (set-binary-port-input-index! $bip -1))
  (error? ; invalid index
    (set-binary-port-input-index! $bip 6))
  (error? ; invalid size
    (set-binary-port-input-size! $bip #vu8(1 2 3)))
  (error? ; invalid size
    (set-binary-port-input-size! $bip -1))
  (error? ; invalid size
    (set-binary-port-input-size! $bip 6))
  ; binary output
  (equal?
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([buffer (string->utf8 "hello")])
        (set-binary-port-output-buffer! op buffer)
        (let ([buffer0 (bytevector-copy (binary-port-output-buffer op))]
              [index0 (binary-port-output-index op)]
              [size0 (binary-port-output-size op)]
              [count0 (binary-port-output-count op)])
          (put-u8 op (char->integer #\j))
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (binary-port-output-buffer op)
              (binary-port-output-index op)
              (binary-port-output-size op)
              (binary-port-output-count op))))))
    `(,(string->utf8 "jello")
      (,(string->utf8 "hello") 0 5 5)
      (,(string->utf8 "jello") 1 5 4)))
  (equal?
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([buffer (string->utf8 "hello")])
        (set-binary-port-output-buffer! op buffer)
        (let ([buffer0 (bytevector-copy (binary-port-output-buffer op))]
              [index0 (binary-port-output-index op)]
              [size0 (binary-port-output-size op)]
              [count0 (binary-port-output-count op)])
          (put-u8 op (char->integer #\j))
          (set-binary-port-output-buffer! op (bytevector 1 2 3 4 5 6))
          (put-u8 op 31)
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (binary-port-output-buffer op)
              (binary-port-output-index op)
              (binary-port-output-size op)
              (binary-port-output-count op))))))
    `(,(string->utf8 "jello")
      (,(string->utf8 "hello") 0 5 5)
      (#vu8(31 2 3 4 5 6) 1 6 5)))
  (equal?
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([buffer (string->utf8 "hello")])
        (set-binary-port-output-buffer! op buffer)
        (let ([buffer0 (bytevector-copy (binary-port-output-buffer op))]
              [index0 (binary-port-output-index op)]
              [size0 (binary-port-output-size op)]
              [count0 (binary-port-output-count op)])
          (put-u8 op (char->integer #\j))
          (set-binary-port-output-index! op 4)
          (put-u8 op (char->integer #\y))
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (binary-port-output-buffer op)
              (binary-port-output-index op)
              (binary-port-output-size op)
              (binary-port-output-count op))))))
    `(,(string->utf8 "jelly")
      (,(string->utf8 "hello") 0 5 5)
      (,(string->utf8 "jelly") 5 5 0)))
  (equal?
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([buffer (string->utf8 "hello")])
        (set-binary-port-output-buffer! op buffer)
        (let ([buffer0 (bytevector-copy (binary-port-output-buffer op))]
              [index0 (binary-port-output-index op)]
              [size0 (binary-port-output-size op)]
              [count0 (binary-port-output-count op)])
          (put-u8 op (char->integer #\j))
          (set-binary-port-output-size! op 4)
          (put-u8 op (char->integer #\b))
          (list
            buffer
            (list buffer0 index0 size0 count0)
            (list
              (binary-port-output-buffer op)
              (binary-port-output-index op)
              (binary-port-output-size op)
              (binary-port-output-count op))))))
    `(,(string->utf8 "bello")
      (,(string->utf8 "hello") 0 5 5)
      (,(string->utf8 "bello") 1 4 3)))
  (error? ; not a binary output port
    (binary-port-output-buffer $tip))
  (error? ; not a binary output port
    (binary-port-output-buffer $top))
  (error? ; not a binary output port
    (binary-port-output-buffer $bip))
  (error? ; not a binary output port
    (binary-port-output-buffer 75))
  (error? ; not a binary output port
    (binary-port-output-index $tip))
  (error? ; not a binary output port
    (binary-port-output-index $top))
  (error? ; not a binary output port
    (binary-port-output-index $bip))
  (error? ; not a binary output port
    (binary-port-output-index 75))
  (error? ; not a binary output port
    (binary-port-output-size $tip))
  (error? ; not a binary output port
    (binary-port-output-size $top))
  (error? ; not a binary output port
    (binary-port-output-size $bip))
  (error? ; not a binary output port
    (binary-port-output-size 75))
  (error? ; not a binary output port
    (binary-port-output-count $tip))
  (error? ; not a binary output port
    (binary-port-output-count $top))
  (error? ; not a binary output port
    (binary-port-output-count $bip))
  (error? ; not a binary output port
    (binary-port-output-count 75))
  (error? ; not a binary output port
    (set-binary-port-output-buffer! $tip ""))
  (error? ; not a binary output port
    (set-binary-port-output-buffer! $top ""))
  (error? ; not a binary output port
    (set-binary-port-output-buffer! $bip ""))
  (error? ; not a binary output port
    (set-binary-port-output-buffer! 75 ""))
  (error? ; not a binary output port
    (set-binary-port-output-index! $tip 0))
  (error? ; not a binary output port
    (set-binary-port-output-index! $top 0))
  (error? ; not a binary output port
    (set-binary-port-output-index! $bip 0))
  (error? ; not a binary output port
    (set-binary-port-output-index! 75 0))
  (error? ; not a binary output port
    (set-binary-port-output-size! $tip 0))
  (error? ; not a binary output port
    (set-binary-port-output-size! $top 0))
  (error? ; not a binary output port
    (set-binary-port-output-size! $bip 0))
  (error? ; not a binary output port
    (set-binary-port-output-size! 75 0))
  (error? ; not a string
    (set-binary-port-output-buffer! $bop "hello"))
  (error? ; not a string
    (set-binary-port-output-buffer! $bop 0))
  (error? ; invalid index
    (set-binary-port-output-index! $bop #vu8(1 2 3)))
  (error? ; invalid index
    (set-binary-port-output-index! $bop -1))
  (error? ; invalid index
    (set-binary-port-output-index! $bop 6))
  (error? ; invalid size
    (set-binary-port-output-size! $bop #vu8(1 2 3)))
  (error? ; invalid size
    (set-binary-port-output-size! $bop -1))
  (error? ; invalid size
    (set-binary-port-output-size! $bop 6))
  (begin
    (define $handler-standin (#%$port-handler (open-string-input-port "hi")))
    #t)
  (let ([name "foo"] [ib "hey!"])
    (let ([p (#%$make-textual-input-port name $handler-standin ib)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (input-port? p)
           (not (output-port? p))
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (textual-port-input-buffer p) ib)
           (eqv? (textual-port-input-size p) (string-length ib))
           (eqv? (textual-port-input-index p) 0)
           (eqv? (textual-port-input-count p) (string-length ib)))))
  (let ([name "foo"] [info "info"] [ib "hey!"])
    (let ([p (#%$make-textual-input-port name $handler-standin ib info)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (input-port? p)
           (not (output-port? p))
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (textual-port-input-buffer p) ib)
           (eqv? (textual-port-input-size p) (string-length ib))
           (eqv? (textual-port-input-index p) 0)
           (eqv? (textual-port-input-count p) (string-length ib)))))
  (let ([name "foo"] [ob "hey!"])
    (let ([p (#%$make-textual-output-port name $handler-standin ob)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (not (input-port? p))
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (textual-port-output-buffer p) ob)
           (eqv? (textual-port-output-size p) (string-length ob))
           (eqv? (textual-port-output-index p) 0)
           (eqv? (textual-port-output-count p) (string-length ob)))))
  (let ([name "foo"] [info "info"] [ob "hey!"])
    (let ([p (#%$make-textual-output-port name $handler-standin ob info)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (not (input-port? p))
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (textual-port-output-buffer p) ob)
           (eqv? (textual-port-output-size p) (string-length ob))
           (eqv? (textual-port-output-index p) 0)
           (eqv? (textual-port-output-count p) (string-length ob)))))
  (let ([name "foo"] [ib "hay!"] [ob "hey!"])
    (let ([p (#%$make-textual-input/output-port name $handler-standin ib ob)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (input-port? p)
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (textual-port-input-buffer p) ib)
           (eqv? (textual-port-input-size p) (string-length ib))
           (eqv? (textual-port-input-index p) 0)
           (eqv? (textual-port-input-count p) (string-length ib))
           (eq? (textual-port-output-buffer p) ob)
           (eqv? (textual-port-output-size p) (string-length ob))
           (eqv? (textual-port-output-index p) 0)
           (eqv? (textual-port-output-count p) (string-length ob)))))
  (let ([name "foo"] [info "info"] [ib "hay!"] [ob "hey!"])
    (let ([p (#%$make-textual-input/output-port name $handler-standin ib ob info)])
      (and (port? p)
           (textual-port? p)
           (not (binary-port? p))
           (input-port? p)
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (textual-port-input-buffer p) ib)
           (eqv? (textual-port-input-size p) (string-length ib))
           (eqv? (textual-port-input-index p) 0)
           (eqv? (textual-port-input-count p) (string-length ib))
           (eq? (textual-port-output-buffer p) ob)
           (eqv? (textual-port-output-size p) (string-length ob))
           (eqv? (textual-port-output-index p) 0)
           (eqv? (textual-port-output-count p) (string-length ob)))))
  (let ([name "foo"] [ib #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-input-port name $handler-standin ib)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (input-port? p)
           (not (output-port? p))
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (binary-port-input-buffer p) ib)
           (eqv? (binary-port-input-size p) (bytevector-length ib))
           (eqv? (binary-port-input-index p) 0)
           (eqv? (binary-port-input-count p) (bytevector-length ib)))))
  (let ([name "foo"] [info "info"] [ib #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-input-port name $handler-standin ib info)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (input-port? p)
           (not (output-port? p))
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (binary-port-input-buffer p) ib)
           (eqv? (binary-port-input-size p) (bytevector-length ib))
           (eqv? (binary-port-input-index p) 0)
           (eqv? (binary-port-input-count p) (bytevector-length ib)))))
  (let ([name "foo"] [ob #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-output-port name $handler-standin ob)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (not (input-port? p))
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (binary-port-output-buffer p) ob)
           (eqv? (binary-port-output-size p) (bytevector-length ob))
           (eqv? (binary-port-output-index p) 0)
           (eqv? (binary-port-output-count p) (bytevector-length ob)))))
  (let ([name "foo"] [info "info"] [ob #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-output-port name $handler-standin ob info)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (not (input-port? p))
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (binary-port-output-buffer p) ob)
           (eqv? (binary-port-output-size p) (bytevector-length ob))
           (eqv? (binary-port-output-index p) 0)
           (eqv? (binary-port-output-count p) (bytevector-length ob)))))
  (let ([name "foo"] [ib #vu8(4 3 2 1)] [ob #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-input/output-port name $handler-standin ib ob)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (input-port? p)
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) #f)
           (eq? (binary-port-input-buffer p) ib)
           (eqv? (binary-port-input-size p) (bytevector-length ib))
           (eqv? (binary-port-input-index p) 0)
           (eqv? (binary-port-input-count p) (bytevector-length ib))
           (eq? (binary-port-output-buffer p) ob)
           (eqv? (binary-port-output-size p) (bytevector-length ob))
           (eqv? (binary-port-output-index p) 0)
           (eqv? (binary-port-output-count p) (bytevector-length ob)))))
  (let ([name "foo"] [info "info"] [ib #vu8(4 3 2 1)] [ob #vu8(1 2 3 4)])
    (let ([p (#%$make-binary-input/output-port name $handler-standin ib ob info)])
      (and (port? p)
           (not (textual-port? p))
           (binary-port? p)
           (input-port? p)
           (output-port? p)
           (eq? (port-name p) name)
           (eq? (#%$port-handler p) $handler-standin)
           (eq? (#%$port-info p) info)
           (eq? (binary-port-input-buffer p) ib)
           (eqv? (binary-port-input-size p) (bytevector-length ib))
           (eqv? (binary-port-input-index p) 0)
           (eqv? (binary-port-input-count p) (bytevector-length ib))
           (eq? (binary-port-output-buffer p) ob)
           (eqv? (binary-port-output-size p) (bytevector-length ob))
           (eqv? (binary-port-output-index p) 0)
           (eqv? (binary-port-output-count p) (bytevector-length ob)))))
 )

(mat file-buffer-size
  (let ([x (file-buffer-size)])
    (and (fixnum? x) (> x 0)))
  (error? (file-buffer-size 1024 15))
  (error? (file-buffer-size 'shoe))
  (error? (file-buffer-size 0))
  (error? (file-buffer-size -15))
  (error? (file-buffer-size (+ (most-positive-fixnum) 1)))
  (error? (file-buffer-size 1024.0))
  (parameterize ([file-buffer-size (* (file-buffer-size) 2)])
    (let ([ip (open-file-input-port prettytest.ss)])
      (let ([n (bytevector-length (binary-port-input-buffer ip))])
        (close-input-port ip)
        (eqv? n (file-buffer-size)))))
)

(mat custom-port-buffer-size
  (let ([x (custom-port-buffer-size)])
    (and (fixnum? x) (> x 0)))
  (error? (custom-port-buffer-size 1024 15))
  (error? (custom-port-buffer-size 'shoe))
  (error? (custom-port-buffer-size 0))
  (error? (custom-port-buffer-size -15))
  (error? (custom-port-buffer-size (+ (most-positive-fixnum) 1)))
  (error? (custom-port-buffer-size 1024.0))
  (parameterize ([custom-port-buffer-size (* (custom-port-buffer-size) 2)])
    (let ([q #f])
      (let ([ip (make-custom-textual-input-port "foo"
                  (lambda (str s c) (set! q c) 0)
                  #f #f #f)])
        (read-char ip)
        (= q (custom-port-buffer-size)))))
)

(mat compress-parameters
  (error? ; unsupported format
    (compress-format 'foo))
  (error? ; unsupported format
    (compress-format "gzip"))
  (eq? (compress-format) 'lz4)
  (eq? (parameterize ([compress-format 'gzip]) (compress-format)) 'gzip)
  (eq? (parameterize ([compress-format 'lz4]) (compress-format)) 'lz4)
  (error? ; unsupported level
    (compress-level 'foo))
  (error? ; unsupported level
    (compress-level 1))
  (eq? (compress-level) 'medium)
  (eq? (parameterize ([compress-level 'low]) (compress-level)) 'low)
  (eq? (parameterize ([compress-level 'medium]) (compress-level)) 'medium)
  (eq? (parameterize ([compress-level 'high]) (compress-level)) 'high)
  (eq? (parameterize ([compress-level 'maximum]) (compress-level)) 'maximum)
  (begin
    (define (fnlength ifn) (call-with-port (open-file-input-port ifn) port-length))
    (define (compress-file ifn ofn fmt lvl)
      (call-with-port (open-file-input-port ifn)
        (lambda (ip)
          (call-with-port (parameterize ([compress-format fmt] [compress-level lvl])
                            (open-file-output-port ofn (file-options compressed replace)))
            (lambda (op) (put-bytevector op (get-bytevector-all ip))))))
      (fnlength ofn))
    (define (compress-file-test fmt)
      (let ([orig (fnlength prettytest.ss)]
            [low (compress-file prettytest.ss "testfile.ss" fmt 'low)]
            [medium (compress-file prettytest.ss "testfile.ss" fmt 'medium)]
            [high (compress-file prettytest.ss "testfile.ss" fmt 'high)]
            [maximum (compress-file prettytest.ss "testfile.ss" fmt 'maximum)])
        (define-syntax test1
          (syntax-rules ()
            [(_ level)
             (unless (< level orig) (errorf #f "~s ~s did not compress" fmt 'level))]))
        (define-syntax test2
          (syntax-rules ()
            [(_ level1 level2)
             (unless (<= level2 level1) (errorf #f "~s ~s did worse than ~s" fmt 'level2 'level1))]))
        (test1 low)
        (test1 medium)
        (test1 high)
        (test1 maximum)
        (test2 low medium)
        (test2 medium high)
        (test2 high maximum)
        (unless (< maximum low) (errorf #f "~s maximum didn't do better than low" fmt))))
    (compress-file-test 'lz4)
    (compress-file-test 'gzip)
    #t)
)

(mat compression
  (parameters [compress-format 'gzip 'lz4] [compress-level 'low 'medium 'high 'maximum])
  (and (memq (compress-format) '(gzip lz4)) #t)
  (and (memq (compress-level) '(low medium high maximum)) #t)
  (let ()
    (define cp
      (lambda (src dst)
        (define buf-size 4096)
        (let ([buf (make-bytevector buf-size)])
          (call-with-port dst
            (lambda (op)
              (call-with-port src
                (lambda (ip)
                  (let loop ()
                    (let ([n (get-bytevector-n! ip buf 0 buf-size)])
                      (unless (eof-object? n)
                        (put-bytevector op buf 0 n)
                        (loop)))))))))))

    (define cmp
      (lambda (src1 src2)
        (define buf-size 4096)
        (let ([buf1 (make-bytevector buf-size)]
              [buf2 (make-bytevector buf-size)])
          (call-with-port src1
            (lambda (ip1)
              (call-with-port src2
                (lambda (ip2)
                  (let loop ()
                    (let ([n1 (get-bytevector-n! ip1 buf1 0 buf-size)]
                          [n2 (get-bytevector-n! ip2 buf2 0 buf-size)])
                      (if (eof-object? n1)
                          (eof-object? n2)
                          (and (= n1 n2)
                               (let test ([i 0])
                                 (or (= i n1)
                                     (and (= (bytevector-u8-ref buf1 i)
                                             (bytevector-u8-ref buf2 i))
                                          (test (+ 1 i)))))
                               (loop))))))))))))
    (and
     (cmp (open-file-input-port prettytest.ss)
          (open-file-input-port prettytest.ss))
     (cmp (open-file-input-port prettytest.ss (file-options compressed))
          (open-file-input-port prettytest.ss))
     (cmp (open-file-input-port prettytest.ss)
          (open-file-input-port prettytest.ss (file-options compressed)))
     (cmp (open-file-input-port prettytest.ss (file-options compressed))
          (open-file-input-port prettytest.ss (file-options compressed)))
     (begin
       (cp (open-file-input-port prettytest.ss)
           (open-file-output-port "testfile.ss" (file-options replace compressed)))
       #t)
     (cmp (open-file-input-port "testfile.ss" (file-options compressed))
          (open-file-input-port prettytest.ss))
     (not (cmp (open-file-input-port "testfile.ss")
               (open-file-input-port prettytest.ss)))
     (begin
       (cp (open-file-input-port prettytest.ss)
           (open-file-output-port "testfile.ss" (file-options no-fail no-truncate append compressed)))
       #t)
     (not (cmp (open-file-input-port "testfile.ss" (file-options compressed))
               (open-file-input-port prettytest.ss)))))
  ; test workaround for bogus gzclose error return for empty input files
  (and
   (eqv? (call-with-port
          (open-file-output-port "testfile.ss" (file-options replace))
          (lambda (x) (void)))
         (void))
   (eof-object? (call-with-port
                 (open-file-input-port "testfile.ss" (file-options compressed))
                 get-u8)))
  (begin
    (let ([op (open-file-output-port "testfile.ss" (file-options replace))])
      (put-bytevector op #vu8(#x23 #x88 #x09 #x72 #xf3 #x72))
      (port-file-compressed! op)
      (put-bytevector op #vu8(#x93 #x21 #x88 #xe7 #x67))
      (let ([op (transcoded-port op (native-transcoder))])
        (display "hello!\n" op)
        (close-port op)))
    #t)
  (equal?
    (let ([ip (open-file-input-port "testfile.ss")])
      (let ([bv1 (get-bytevector-n ip 6)])
        (port-file-compressed! ip)
        (let ([bv2 (get-bytevector-n ip 5)])
          (let ([ip (transcoded-port ip (native-transcoder))])
            (let ([s (get-string-all ip)])
              (close-port ip)
              (list bv1 bv2 s))))))
    '(#vu8(#x23 #x88 #x09 #x72 #xf3 #x72)
      #vu8(#x93 #x21 #x88 #xe7 #x67)
      "hello!\n"))
  (not
    (equal?
      (let ([ip (open-file-input-port "testfile.ss")])
        (let ([bv1 (get-bytevector-n ip 6)])
          (let ([bv2 (get-bytevector-n ip 5)])
            (close-port ip)
            (list bv1 bv2))))
      '(#vu8(#x23 #x88 #x09 #x72 #xf3 #x72)
        #vu8(#x93 #x21 #x88 #xe7 #x67))))
  (begin
    (let ([op (open-file-output-port "testfile.ss" (file-options replace) (buffer-mode block) (native-transcoder))])
      (put-string op "uncompressed string")
      (port-file-compressed! op)
      (put-string op "compressed string")
      (close-port op))
    #t)
  (equal?
    (let ([ip (open-file-input-port "testfile.ss" (file-options) (buffer-mode block) (native-transcoder))])
      (let ([s1 (get-string-n ip (string-length "uncompressed string"))])
        (port-file-compressed! ip)
        (let ([s2 (get-string-all ip)])
          (close-port ip)
          (list s1 s2))))
    '("uncompressed string" "compressed string"))
  (error? ; not a file port
    (call-with-string-output-port port-file-compressed!))
  (error? ; input/output ports aren't supported
    (let ([iop (open-file-input/output-port "testfile.ss" (file-options replace))])
      (guard (c [else (close-port iop) (raise c)])
        (port-file-compressed! iop))))
  (begin
    (let ([op (open-file-output-port "testfile.ss" (file-options compressed replace) (buffer-mode block) (native-transcoder))])
      (port-file-compressed! op)
      (put-string op "compressed string")
      (close-port op))
    #t)
  (equal?
    (let ([ip (open-file-input-port "testfile.ss" (file-options compressed) (buffer-mode block) (native-transcoder))])
      (port-file-compressed! ip)
      (let ([s (get-string-all ip)])
        (close-port ip)
        s))
    '"compressed string")
)

(mat bytevector-input-port
  (error? ; incorrect number of arguments
    (open-bytevector-input-port))
  (error? ; not a bytevector
    (open-bytevector-input-port '#(1 2 3 4)))
  (error? ; none is not a transcoder
    (open-bytevector-input-port #vu8(1 2 3 4) 'none))
  (error? ; incorrect number of arguments
    (open-bytevector-input-port #vu8(1 2 3 4) #f 'none))
  (let ()
    (define x (open-bytevector-input-port #vu8(1 2 3 4)))
    (and (eq? (get-u8 x) 1)
         (eq? (get-u8 x) 2)
         (eq? (get-u8 x) 3)
         (eq? (get-u8 x) 4)
         (eq? (get-u8 x) (eof-object))))
  (let ()
    (define x (open-bytevector-input-port #vu8(1 2 3 4)))
    (and (port-has-port-position? x)
         (eq? (port-position x) 0)
         (eq? (get-u8 x) 1)
         (eq? (port-position x) 1)
         (eq? (get-u8 x) 2)
         (eq? (port-position x) 2)
         (eq? (get-u8 x) 3)
         (eq? (port-position x) 3)
         (eq? (get-u8 x) 4)
         (eq? (port-position x) 4)
         (eq? (get-u8 x) #!eof)
         (eq? (port-position x) 4)
         (eq? (get-u8 x) #!eof)
         (eq? (port-position x) 4)
         (eq? (get-u8 x) #!eof)
         (eq? (port-position x) 4)))
  (let ()
    (define x (open-bytevector-input-port #vu8(1 2 3 4)))
    (and (port-has-set-port-position!? x)
         (eq? (port-position x) 0)
         (eq? (get-u8 x) 1)
         (eq? (port-position x) 1)
         (eq? (get-u8 x) 2)
         (eq? (port-position x) 2)
         (begin (set-port-position! x 0) #t)
         (eq? (get-u8 x) 1)
         (begin (set-port-position! x 4) #t)
         (eq? (get-u8 x) #!eof)))
  (error? (set-port-position! (open-bytevector-input-port #vu8(1 2 3 4)) -1))
  (error? (set-port-position! (open-bytevector-input-port #vu8(1 2 3 4)) 5))

  (let ()
    (define x (open-bytevector-input-port #vu8(1 2 3 4)))
    (and (eq? (lookahead-u8 x) 1)
         (eq? (lookahead-u8 x) 1)
         (eq? (lookahead-u8 x) 1)
         (eq? (get-u8 x) 1)
         (eq? (lookahead-u8 x) 2)
         (eq? (get-u8 x) 2)
         (eq? (lookahead-u8 x) 3)
         (eq? (get-u8 x) 3)
         (eq? (lookahead-u8 x) 4)
         (eq? (get-u8 x) 4)
         (eq? (lookahead-u8 x) #!eof)
         (eq? (get-u8 x) #!eof)
         (eq? (lookahead-u8 x) #!eof)
         (eq? (get-u8 x) #!eof)))
  (eq? (buffer-mode none) 'none)
  (eq? (buffer-mode line) 'line)
  (eq? (buffer-mode block) 'block)
  (error? (buffer-mode bar))
  (error? (buffer-mode 'none))
  (eq? (buffer-mode? 'none) #t)
  (eq? (buffer-mode? 'line) #t)
  (eq? (buffer-mode? 'block) #t)
  (eq? (buffer-mode? 'foo) #f)
)

(mat bytevector-output-port
  (error? ; not a transcoder
    (open-bytevector-output-port 'oops))
  (error? ; incorrect number of arguments
    (open-bytevector-output-port #f 'none))
)

(mat custom-binary-ports
  (begin
    (define $cp-ip
      (let ([pos 0])
        (make-custom-binary-input-port "foo"
          (lambda (bv s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (bytevector-u8-set! bv i (modulo (+ pos i) 256))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          #f)))
    #t)
  (eq? (port-position $cp-ip) 0)
  (error? ; cannot unget
    (unget-u8 $cp-ip 255))
  (begin (unget-u8 $cp-ip (eof-object)) #t)
  (port-eof? $cp-ip)
  (eof-object? (lookahead-u8 $cp-ip))
  (eof-object? (get-u8 $cp-ip))
  (equal?
    (get-bytevector-n $cp-ip 10)
    #vu8(0 1 2 3 4 5 6 7 8 9))
  (eqv? (port-position $cp-ip) 10)
  (eqv? (get-u8 $cp-ip) 10)
  (begin (set-port-position! $cp-ip 256000) #t)
  (eqv? (get-u8 $cp-ip) 0)
  (eqv? (port-position $cp-ip) 256001)
  (error? ; not a binary output port
    (put-u8 $cp-ip 255))
  (not (port-has-port-length? $cp-ip))
  (not (port-has-set-port-length!? $cp-ip))
  (not (port-has-port-nonblocking?? $cp-ip))
  (not (port-has-set-port-nonblocking!? $cp-ip))
  (error? ; not supported
    (port-length $cp-ip))
  (error? ; not supported
    (set-port-length! $cp-ip 50))
  (error? ; not supported
    (port-nonblocking? $cp-ip))
  (error? ; not supported
    (set-port-nonblocking! $cp-ip #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-ip #f))
  (begin
    (define $cp-op
      (let ([pos 0])
        (make-custom-binary-output-port "foo"
          (lambda (bv s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (eq? (port-position $cp-op) 0)
  (error? ; not a binary input port
    (unget-u8 $cp-op 255))
  (not (port-has-port-length? $cp-op))
  (not (port-has-set-port-length!? $cp-op))
  (not (port-has-port-nonblocking?? $cp-op))
  (not (port-has-set-port-nonblocking!? $cp-op))
  (error? ; not supported
    (port-length $cp-op))
  (error? ; not supported
    (set-port-length! $cp-op 50))
  (error? ; not supported
    (port-nonblocking? $cp-op))
  (error? ; not supported
    (set-port-nonblocking! $cp-op #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-op #f))
  (begin (put-u8 $cp-op 255) #t)
  (eqv? (port-position $cp-op) 1)
  (begin (set-port-position! $cp-op 17) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (put-bytevector $cp-op #vu8(17 18 19 20))
        (put-bytevector $cp-op #vu8(20 21 22 23 24 25) 1)
        (put-bytevector $cp-op #vu8(24 25 26 27 28 29) 1 4)))
    "")
  (equal? ; in our current implementation...
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-op))))
    "pos = 30\n")
  (equal? ; ... actual flush won't happen until here
    (with-output-to-string
      (lambda ()
        (r6rs:flush-output-port $cp-op)))
    "write 13\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-op))))
    "pos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (put-bytevector $cp-op #vu8(17 18 19 20))
        (put-bytevector $cp-op #vu8(20 21 22 23 24 25) 1)
        (put-bytevector $cp-op #vu8(24 25 26 27 28 29) 1 4)))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (close-port $cp-op)))
    "write 13\nclosed\n")
  (error? ; closed
    (put-u8 $cp-op 0))
  (error? ; closed
    (put-bytevector $cp-op #vu8(3)))
  (error? ; closed
    (r6rs:flush-output-port $cp-op))
  (begin
    (define $cp-iop
      (let ([pos 0])
        (make-custom-binary-input/output-port "foo"
          (lambda (bv s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (bytevector-u8-set! bv i (modulo (+ pos i) 256))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (bv s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (eq? (port-position $cp-iop) 0)
  (error? ; cannot unget
    (unget-u8 $cp-iop 255))
  (begin (unget-u8 $cp-iop (eof-object)) #t)
  (port-eof? $cp-iop)
  (eof-object? (lookahead-u8 $cp-iop))
  (eof-object? (get-u8 $cp-iop))
  (equal?
    (get-bytevector-n $cp-iop 10)
    #vu8(0 1 2 3 4 5 6 7 8 9))
  (eqv? (port-position $cp-iop) 10)
  (eqv? (lookahead-u8 $cp-iop) 10)
  (eqv? (get-u8 $cp-iop) 10)
  (begin (set-port-position! $cp-iop 256000) #t)
  (eqv? (get-u8 $cp-iop) 0)
  (eqv? (port-position $cp-iop) 256001)
  (not (port-has-port-length? $cp-iop))
  (not (port-has-set-port-length!? $cp-iop))
  (not (port-has-port-nonblocking?? $cp-iop))
  (not (port-has-set-port-nonblocking!? $cp-iop))
  (error? ; not supported
    (port-length $cp-iop))
  (error? ; not supported
    (set-port-length! $cp-iop 50))
  (error? ; not supported
    (port-nonblocking? $cp-iop))
  (error? ; not supported
    (set-port-nonblocking! $cp-iop #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-iop #f))
  (begin (put-u8 $cp-iop 255) #t)
  (eqv? (port-position $cp-iop) 256002)
  (begin (set-port-position! $cp-iop 17) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (put-bytevector $cp-iop #vu8(17 18 19 20))
        (put-bytevector $cp-iop #vu8(20 21 22 23 24 25) 1)
        (put-bytevector $cp-iop #vu8(24 25 26 27 28 29) 1 4)))
    "")
  (equal? ; in our current implementation...
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-iop))))
    "pos = 30\n")
  (equal? ; ... actual flush won't happen until here
    (with-output-to-string
      (lambda ()
        (r6rs:flush-output-port $cp-iop)))
    "write 13\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-iop))))
    "pos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (put-bytevector $cp-iop #vu8(17 18 19 20))
        (put-bytevector $cp-iop #vu8(20 21 22 23 24 25) 1)
        (put-bytevector $cp-iop #vu8(24 25 26 27 28 29) 1 4)))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (close-port $cp-iop)))
    "write 13\nclosed\n")
  (error? ; closed
    (put-u8 $cp-iop 0))
  (error? ; closed
    (put-bytevector $cp-iop #vu8(3)))
  (error? ; closed
    (r6rs:flush-output-port $cp-iop))

  (begin
    (define $cp-iop
      (let ([pos 0])
        (make-custom-binary-input/output-port "foo"
          (lambda (bv s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (bytevector-u8-set! bv i (modulo (+ pos i) 256))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (bv s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          #f
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-port-position? $cp-iop))
  (error? ; operation not supported
    (port-position $cp-iop))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (eqv? (get-u8 $cp-iop) 1)
  (custom-port-warning? ; can't determine position for write
    (put-u8 $cp-iop 255))
  (begin (set-port-position! $cp-iop 50) #t)
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (eqv? (get-u8 $cp-iop) 51)
  (custom-port-warning? ; can't determine position for write
    (put-bytevector $cp-iop #vu8(17)))

  (begin
    (define $cp-iop
      (let ([pos 0])
        (make-custom-binary-input/output-port "foo"
          (lambda (bv s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (bytevector-u8-set! bv i (modulo (+ pos i) 256))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (bv s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          #f
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-set-port-position!? $cp-iop))
  (error? ; operation not supported
    (set-port-position! $cp-iop 3))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (eqv? (get-u8 $cp-iop) 1)
  (custom-port-warning? ; can't set position for write
   ; convoluted because we want warning to return normally so that operation
   ; is completed
    (let ([hit? #f])
      (with-exception-handler
        (lambda (c) (if (warning? c) (set! hit? c) (raise c)))
        (lambda () (put-u8 $cp-iop 255)))
      (when hit? (raise hit?))))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (begin (get-u8 $cp-iop) #t) ; position undefined, so value undefined
  (custom-port-warning? ; can't set position for write
    (put-bytevector $cp-iop #vu8(17)))

  (begin
    (define $cp-iop
      (let ([pos 0])
        (make-custom-binary-input/output-port "foo"
          (lambda (bv s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (bytevector-u8-set! bv i (modulo (+ pos i) 256))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (bv s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          #f
          #f
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-port-position? $cp-iop))
  (error? ; operation not supported
    (port-position $cp-iop))
  (not (port-has-set-port-position!? $cp-iop))
  (error? ; operation not supported
    (set-port-position! $cp-iop 3))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (eqv? (get-u8 $cp-iop) 1)
  (custom-port-warning? ; can't determine position for write
   ; convoluted because we want warning to return normally so that operation
   ; is completed
    (let ([hit? #f])
      (with-exception-handler
        (lambda (c) (if (warning? c) (set! hit? c) (raise c)))
        (lambda () (put-u8 $cp-iop 255)))
      (when hit? (raise hit?))))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-u8 $cp-iop 255))
    #t)
  (begin (get-u8 $cp-iop) #t) ; position undefined, so value undefined
  (custom-port-warning? ; can't determine position for write
    (put-bytevector $cp-iop #vu8(17)))
)

(mat custom-textual-ports
  (begin
    (define $cp-ip
      (let ([pos 0] [chars "0123456789abcdefghijklmnopqrstuvwxyz"])
        (make-custom-textual-input-port "foo"
          (lambda (str s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (string-set! str i (string-ref chars (modulo (+ pos i) 36)))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          #f)))
    #t)
  (eq? (port-position $cp-ip) 0)
  (error? ; cannot unget
    (unget-char $cp-ip #\q))
  (begin (unget-char $cp-ip (eof-object)) #t)
  (port-eof? $cp-ip)
  (eof-object? (lookahead-char $cp-ip))
  (eof-object? (get-char $cp-ip))
  (equal?
    (get-string-n $cp-ip 10)
    "0123456789")
  (eqv? (port-position $cp-ip) 10)
  (eqv? (get-char $cp-ip) #\a)
  (begin (set-port-position! $cp-ip 36000) #t)
  (eqv? (get-char $cp-ip) #\0)
  (custom-port-warning? (port-position $cp-ip))
  (error? ; not a textual output port
    (put-char $cp-ip #\a))
  (not (port-has-port-length? $cp-ip))
  (not (port-has-set-port-length!? $cp-ip))
  (not (port-has-port-nonblocking?? $cp-ip))
  (not (port-has-set-port-nonblocking!? $cp-ip))
  (error? ; not supported
    (port-length $cp-ip))
  (error? ; not supported
    (set-port-length! $cp-ip 50))
  (error? ; not supported
    (port-nonblocking? $cp-ip))
  (error? ; not supported
    (set-port-nonblocking! $cp-ip #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-ip #f))

  (begin
    (define $cp-op
      (let ([pos 0])
        (make-custom-textual-output-port "foo"
          (lambda (str s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (eq? (port-position $cp-op) 0)
  (error? ; not a textual output port
    (unget-char $cp-op 255))
  (not (port-has-port-length? $cp-op))
  (not (port-has-set-port-length!? $cp-op))
  (not (port-has-port-nonblocking?? $cp-op))
  (not (port-has-set-port-nonblocking!? $cp-op))
  (error? ; not supported
    (port-length $cp-op))
  (error? ; not supported
    (set-port-length! $cp-op 50))
  (error? ; not supported
    (port-nonblocking? $cp-op))
  (error? ; not supported
    (set-port-nonblocking! $cp-op #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-op #f))
  (begin (put-char $cp-op #\$) #t)
  (eqv? (port-position $cp-op) 1)
  (begin (set-port-position! $cp-op 17) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (put-string $cp-op "abcd")
        (put-string $cp-op "defghi" 1)
        (put-string $cp-op "hijklm" 1 4)))
    "")
  (equal? ; in our current implementation...
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-op))))
    "write 13\npos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-op))))
    "pos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (put-string $cp-op "abcd")
        (put-string $cp-op "defghi" 1)
        (put-string $cp-op "hijklm" 1 4)))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (close-port $cp-op)))
    "write 13\nclosed\n")
  (error? ; closed
    (put-char $cp-op #\$))
  (error? ; closed
    (put-string $cp-op "3"))
  (error? ; closed
    (r6rs:flush-output-port $cp-op))

  (begin
    (define $cp-iop
      (let ([pos 0] [chars "0123456789abcdefghijklmnopqrstuvwxyz"])
        (make-custom-textual-input/output-port "foo"
          (lambda (str s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (string-set! str i (string-ref chars (modulo (+ pos i) 36)))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (str s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (eq? (port-position $cp-iop) 0)
  (error? ; cannot unget
    (unget-char $cp-iop #\$))
  (begin (unget-char $cp-iop (eof-object)) #t)
  (port-eof? $cp-iop)
  (eof-object? (lookahead-char $cp-iop))
  (eof-object? (get-char $cp-iop))
  (equal?
    (get-string-n $cp-iop 10)
    "0123456789")
  (eqv? (port-position $cp-iop) 10)
  (eqv? (get-char $cp-iop) #\a)
  (begin (set-port-position! $cp-iop 36000) #t)
  (eqv? (get-char $cp-iop) #\0)
  (custom-port-warning? (port-position $cp-iop))
  (not (port-has-port-length? $cp-iop))
  (not (port-has-set-port-length!? $cp-iop))
  (not (port-has-port-nonblocking?? $cp-iop))
  (not (port-has-set-port-nonblocking!? $cp-iop))
  (error? ; not supported
    (port-length $cp-iop))
  (error? ; not supported
    (set-port-length! $cp-iop 50))
  (error? ; not supported
    (port-nonblocking? $cp-iop))
  (error? ; not supported
    (set-port-nonblocking! $cp-iop #t))
  (error? ; not supported
    (set-port-nonblocking! $cp-iop #f))
  (custom-port-warning? (put-char $cp-iop #\$))
  (begin (set-port-position! $cp-iop 17) #t)
  (eqv? (port-position $cp-iop) 17)
  (equal?
    (with-output-to-string
      (lambda ()
        (put-string $cp-iop "abcd")
        (put-string $cp-iop "defghi" 1)
        (put-string $cp-iop "hijklm" 1 4)))
    "")
  (equal? ; in our current implementation...
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-iop))))
    "write 13\npos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (printf "pos = ~s\n" (port-position $cp-iop))))
    "pos = 30\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (put-string $cp-iop "abcd")
        (put-string $cp-iop "defghi" 1)
        (put-string $cp-iop "hijklm" 1 4)))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (close-port $cp-iop)))
    "write 13\nclosed\n")
  (error? ; closed
    (put-char $cp-iop #\$))
  (error? ; closed
    (put-string $cp-iop "3"))
  (error? ; closed
    (r6rs:flush-output-port $cp-iop))

  (begin
    (define $cp-iop
      (let ([pos 0] [chars "0123456789abcdefghijklmnopqrstuvwxyz"])
        (make-custom-textual-input/output-port "foo"
          (lambda (str s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (string-set! str i (string-ref chars (modulo (+ pos i) 36)))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (str s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          #f
          (lambda (x) (set! pos x))
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-port-position? $cp-iop))
  (error? ; operation not supported
    (port-position $cp-iop))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (eqv? (get-char $cp-iop) #\1)
  (custom-port-warning? ; can't determine position for write
    (put-char $cp-iop #\$))
  (begin (set-port-position! $cp-iop 50) #t)
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (eqv? (get-char $cp-iop) #\f)
  (custom-port-warning? ; can't determine position for write
    (put-string $cp-iop "a"))

  (begin
    (define $cp-iop
      (let ([pos 0] [chars "0123456789abcdefghijklmnopqrstuvwxyz"])
        (make-custom-textual-input/output-port "foo"
          (lambda (str s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (string-set! str i (string-ref chars (modulo (+ pos i) 36)))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (str s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          (lambda () pos)
          #f
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-set-port-position!? $cp-iop))
  (error? ; operation not supported
    (set-port-position! $cp-iop 3))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (eqv? (get-char $cp-iop) #\1)
  (custom-port-warning? ; can't set position for write
   ; convoluted because we want warning to return normally so that operation
   ; is completed
    (let ([hit? #f])
      (with-exception-handler
        (lambda (c) (if (warning? c) (set! hit? c) (raise c)))
        (lambda () (put-char $cp-iop #\$)))
      (when hit? (raise hit?))))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (begin (get-char $cp-iop) #t) ; position undefined, so value undefined
  (custom-port-warning? ; can't set position for write
    (put-string $cp-iop "a"))

  (begin
    (define $cp-iop
      (let ([pos 0] [chars "0123456789abcdefghijklmnopqrstuvwxyz"])
        (make-custom-textual-input/output-port "foo"
          (lambda (str s c)
            (let loop ([i s])
              (unless (eq? i (+ s c))
                (string-set! str i (string-ref chars (modulo (+ pos i) 36)))
                (loop (+ 1 i))))
            (set! pos (+ pos c))
            c)
          (lambda (str s c)
            (set! pos (+ pos c))
            (printf "write ~s\n" c)
            c)
          #f
          #f
          (lambda () (printf "closed\n")))))
    #t)
  (not (port-has-port-position? $cp-iop))
  (error? ; operation not supported
    (port-position $cp-iop))
  (not (port-has-set-port-position!? $cp-iop))
  (error? ; operation not supported
    (set-port-position! $cp-iop 3))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (eqv? (get-char $cp-iop) #\1)
  (custom-port-warning? ; can't determine position for write
   ; convoluted because we want warning to return normally so that operation
   ; is completed
    (let ([hit? #f])
      (with-exception-handler
        (lambda (c) (if (warning? c) (set! hit? c) (raise c)))
        (lambda () (put-char $cp-iop #\$)))
      (when hit? (raise hit?))))
  (begin
    (guard (c [(warning? c) (errorf #f "unexpected warning ~s" c)])
      (put-char $cp-iop #\$))
    #t)
  (begin (get-char $cp-iop) #t) ; position undefined, so value undefined
  (custom-port-warning? ; can't determine position for write
    (put-string $cp-iop "a"))

  (equal?
    (let-values ([(sop get) (open-string-output-port)])
      (define op
        (make-custom-textual-output-port "foo"
          (lambda (str s c)
            (put-string sop str s c)
            c)
          #f #f #f))
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello")
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello")
      (flush-output-port op)
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello\n")
      (flush-output-port op)
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello\n")
      (fresh-line op)
      (close-port op)
      (get))
    "hello\nhello\nhello\nhello\n")

  (equal?
    (let-values ([(sop get) (open-string-output-port)])
      (define op
        (make-custom-textual-input/output-port "foo"
          (lambda (str s c) (errorf #f "oops"))
          (lambda (str s c)
            (put-string sop str s c)
            c)
          #f #f #f))
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello")
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello")
      (flush-output-port op)
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello\n")
      (flush-output-port op)
      (fresh-line op)
      (fresh-line op)
      (put-string op "hello\n")
      (fresh-line op)
      (close-port op)
      (get))
    "hello\nhello\nhello\nhello\n")
)

(mat compression-textual
  (parameters [compress-format 'gzip 'lz4] [compress-level 'low 'medium 'high 'maximum])
  (let ()
    (define cp
      (lambda (src dst)
        (define buf-size 103)
        (let ([buf (make-string buf-size)])
          (call-with-port dst
            (lambda (op)
              (call-with-port src
                (lambda (ip)
                  (let loop ()
                    (do ([i 0 (fx+ i 1)])
                        ((fx= i buf-size))
                      (let ([c (get-char ip)])
                        (unless (eof-object? c) (put-char op c))))
                    (let ([n (get-string-n! ip buf 0 buf-size)])
                      (unless (eof-object? n)
                        (put-string op buf 0 n)
                        (loop)))))))))))
    (define cmp
      (lambda (src1 src2)
        (define buf-size 128)
        (let ([buf (make-string buf-size)])
          (call-with-port src1
            (lambda (ip1)
              (call-with-port src2
                (lambda (ip2)
                  (let loop ([pos 0])
                    (let ([n (get-string-n! ip1 buf 0 buf-size)])
                      (if (eof-object? n)
                          (unless (eof-object? (lookahead-char ip2)) (errorf #f "ip1 eof before ip2"))
                          (if (eof-object? (lookahead-char ip2))
                              (errorf #f "ip2 eof before ip1")
                              (let test ([i 0] [pos pos])
                                (if (= i n)
                                    (loop pos)
                                    (let ([c1 (string-ref buf i)] [c2 (get-char ip2)])
                                      (if (char=? c1 c2)
                                          (test (+ 1 i) (+ pos 1))
                                          (errorf #f "ip1 c = ~s =/= ip2 c = ~s at pos ~s" c1 c2 pos))))))))))))))))
    (define (in fn compressed? codec)
      (open-file-input-port fn
        (if compressed? (file-options compressed) (file-options))
        (buffer-mode block)
        (make-transcoder codec)))
    (define (out fn compressed? codec)
      (open-file-output-port fn
        (if compressed? (file-options compressed replace) (file-options replace))
        (buffer-mode block)
        (make-transcoder codec)))
    (time (cmp (in prettytest.ss #f (latin-1-codec)) (in prettytest.ss #f (latin-1-codec))))
    (time (cmp (in prettytest.ss #t (latin-1-codec)) (in prettytest.ss #f (latin-1-codec))))
    (time (cmp (in prettytest.ss #f (latin-1-codec)) (in prettytest.ss #t (latin-1-codec))))
    (time (cmp (in prettytest.ss #t (latin-1-codec)) (in prettytest.ss #t (latin-1-codec))))
    (time (cmp (in prettytest.ss #f (utf-8-codec)) (in prettytest.ss #f (utf-8-codec))))
    (time (cmp (in prettytest.ss #t (utf-8-codec)) (in prettytest.ss #f (utf-8-codec))))
    (time (cmp (in prettytest.ss #f (utf-8-codec)) (in prettytest.ss #t (utf-8-codec))))
    (time (cmp (in prettytest.ss #t (utf-8-codec)) (in prettytest.ss #t (utf-8-codec))))
    (cp (in prettytest.ss #f (latin-1-codec)) (out "testfile.ss" #t (latin-1-codec)))
    (cmp (in prettytest.ss #f (latin-1-codec)) (in "testfile.ss" #t (latin-1-codec)))
    (cmp (in prettytest.ss #t (latin-1-codec)) (in "testfile.ss" #t (latin-1-codec)))
    (cmp (in prettytest.ss #f (utf-8-codec)) (in "testfile.ss" #t (utf-8-codec)))
    (cmp (in prettytest.ss #t (utf-8-codec)) (in "testfile.ss" #t (utf-8-codec)))
    (cp (in prettytest.ss #f (utf-8-codec)) (out "testfile.ss" #t (utf-8-codec)))
    (cmp (in prettytest.ss #f (latin-1-codec)) (in "testfile.ss" #t (latin-1-codec)))
    (cmp (in prettytest.ss #t (latin-1-codec)) (in "testfile.ss" #t (latin-1-codec)))
    (cmp (in prettytest.ss #f (utf-8-codec)) (in "testfile.ss" #t (utf-8-codec)))
    (cmp (in prettytest.ss #t (utf-8-codec)) (in "testfile.ss" #t (utf-8-codec)))
    #t)
  ; test workaround for bogus gzclose error return for empty input files
  (and
    (eqv? (call-with-port
            (open-file-output-port "testfile.ss" (file-options replace) (buffer-mode block) (native-transcoder))
            (lambda (x) (void)))
          (void))
    (eof-object?
      (call-with-port
        (open-file-input-port "testfile.ss" (file-options compressed)
          (buffer-mode block) (native-transcoder))
        get-char)))
)

(mat string-ports
  (let ()
    (define pretty-test-string
      (call-with-port
        (open-file-input-port prettytest.ss
          (file-options) (buffer-mode none) (native-transcoder))
        get-string-all))
    (define cp ; doesn't close the ports
      (lambda (ip op)
        (define buf-size 103)
        (let ([buf (make-string buf-size)])
          (let loop ()
            (do ([i 0 (fx+ i 1)])
                ((fx= i buf-size))
              (let ([c (get-char ip)])
                (unless (eof-object? c) (put-char op c))))
            (let ([n (get-string-n! ip buf 0 buf-size)])
              (unless (eof-object? n)
                (put-string op buf 0 n)
                (loop)))))))
    (define cmp
      (lambda (src1 src2)
        (define buf-size 64)
        (let ([buf (make-string buf-size)])
          (call-with-port src1
            (lambda (ip1)
              (call-with-port src2
                (lambda (ip2)
                  (let loop ([pos 0])
                    (let ([n (get-string-n! ip1 buf 0 buf-size)])
                      (if (eof-object? n)
                          (unless (eof-object? (lookahead-char ip2)) (errorf #f "ip1 eof before ip2"))
                          (if (eof-object? (lookahead-char ip2))
                              (errorf #f "ip2 eof before ip1")
                              (let test ([i 0] [pos pos])
                                (if (= i n)
                                    (loop pos)
                                    (let ([c1 (string-ref buf i)] [c2 (get-char ip2)])
                                      (if (char=? c1 c2)
                                          (test (+ 1 i) (+ pos 1))
                                          (errorf #f "ip1 c = ~s =/= ip2 c = ~s at pos ~s" c1 c2 pos))))))))))))))))
    (define (in fn compressed? codec)
      (open-file-input-port fn
        (if compressed? (file-options compressed) (file-options))
        (buffer-mode block)
        (make-transcoder codec)))
    (define (out fn compressed? codec)
      (open-file-output-port fn
        (if compressed? (file-options compressed replace) (file-options replace))
        (buffer-mode block)
        (make-transcoder codec)))
    (time (cmp (in prettytest.ss #f (latin-1-codec)) (open-string-input-port pretty-test-string)))
    (time (cmp (open-string-input-port pretty-test-string) (in prettytest.ss #f (latin-1-codec))))
    (let-values ([(op retrieve) (open-string-output-port)])
      (cp (open-string-input-port pretty-test-string) op)
      (cmp (in prettytest.ss #f (latin-1-codec)) (open-string-input-port (retrieve))))
    #t)
)

(mat current-ports
  (input-port? (current-input-port))
  (textual-port? (current-input-port))
  (not (output-port? (open-input-string "hello")))
  (output-port? (current-output-port))
  (textual-port? (current-output-port))
  (output-port? (current-error-port))
  (textual-port? (current-error-port))
  (not (input-port? (open-output-string)))
  (eq? (r6rs:current-input-port) (current-input-port))
  (eq? (r6rs:current-output-port) (current-output-port))
  (eq? (r6rs:current-error-port) (current-error-port))
  (equal?
    (with-output-to-string
      (lambda ()
        (write (list
                 (eq? (r6rs:current-input-port) (current-input-port))
                 (eq? (r6rs:current-output-port) (current-output-port))
                 (eq? (r6rs:current-error-port) (current-error-port))))))
    "(#t #t #t)")
  (error? (current-input-port (standard-input-port)))
  (error? (current-output-port (standard-output-port)))
  (error? (current-error-port (standard-output-port)))
  (error? (current-input-port (open-output-string)))
  (error? (current-output-port (open-input-string "")))
  (error? (current-error-port (open-input-string "")))
  (error? (console-input-port (standard-input-port)))
  (error? (console-output-port (standard-output-port)))
  (error? (console-error-port (standard-output-port)))
  (error? (console-input-port (open-output-string)))
  (error? (console-output-port (open-input-string "")))
  (error? (console-error-port (open-input-string "")))
)

(mat current-transcoder
  (transcoder? (current-transcoder))
  (eqv? (current-transcoder) (native-transcoder))
  (error? (current-transcoder (open-output-string)))
  (parameterize ([current-transcoder (native-transcoder)])
    (eqv? (current-transcoder) (native-transcoder)))
  (parameterize ([current-transcoder (make-transcoder (utf-16le-codec))])
    (with-output-to-file "testfile.ss" (lambda () (write '\x3bb;12345)) 'replace)
    (file-exists? "testfile.ss"))
  (parameterize ([current-transcoder (make-transcoder (utf-16le-codec))])
    (with-input-from-file "testfile.ss"
      (lambda ()
        (and (eqv? (read) '\x3bb;12345) (eof-object? (read))))))
  (equal?
    (call-with-port (open-file-input-port "testfile.ss") get-bytevector-all)
    #vu8(#xBB #x3 #x31 #x0 #x32 #x0 #x33 #x0 #x34 #x0 #x35 #x0))
)

(mat get/put-datum
  (error? (get-datum))
  (error? (get-datum (current-input-port) (current-input-port)))
  (error? (get-datum (open-output-string)))
  (error? (get-datum (open-bytevector-input-port #vu8())))
  (call-with-port
    (open-string-input-port "hey #;there dude!")
    (lambda (p)
      (and (eq? (get-datum p) 'hey)
           (eqv? (get-char p) #\space)
           (eq? (get-datum p) 'dude!)
           (eof-object? (get-datum p)))))
  (error? (put-datum))
  (error? (put-datum (current-output-port)))
  (error? (put-datum (current-output-port) 'a 'a))
  (error? (put-datum (open-input-string "hello") 'a))
  (error? (put-datum (let-values ([(p g) (open-bytevector-output-port)]) p) 'a))
  (equal?
    (let-values ([(p g) (open-string-output-port)])
      (put-datum p '(this is))
      (put-datum p "cool")
      (put-datum p '(or (maybe . not)))
      (g))
    "(this is)\"cool\"(or (maybe . not))")
  (call-with-port
    (open-string-input-port "#3(a b c) #!r6rs #(d e) #!chezscheme #3(f g)")
    (lambda (p)
      (and
        (equal? (get-datum p) '#(a b c))
        (equal? (get-datum p) '#(d e))
        (equal? (get-datum p) '#(f g g))
        (equal? (get-datum p) #!eof))))
 ; make sure that nel and ls are treated properly
  (call-with-port
    (open-string-input-port "#!r6rs \x85; \x2028; #\\x85 #\\x2028 \"\\x85; \\x2028;\"")
    (lambda (p)
      (and
        (equal? (get-datum p) (integer->char #x85))
        (equal? (get-datum p) (integer->char #x2028))
        (equal? (get-datum p) (string (integer->char #x85) #\space (integer->char #x2028))))))
  (equal?
    (call-with-string-output-port
      (lambda (p)
        (put-char p #\x85)
        (put-char p #\space)
        (put-char p #\x2028)
        (put-char p #\space)
        (put-datum p #\x85)
        (put-char p #\space)
        (put-datum p #\x2028)
        (put-char p #\space)
        (put-datum p "\x85; \x2028;")))
    "\x85; \x2028; #\\x85 #\\x2028 \"\\x85; \\x2028;\"")
  (let ()
    (define (rw? x1)
      (let ([str (let-values ([(p e) (open-string-output-port)])
                   (write x1 p)
                   (e))])
        (let ([x2 (read (open-string-input-port str))])
          (equal? x1 x2))))
    (and
      (rw? "  \x85;  ")
      (rw? "  \x2028;  ")
      (rw? #\x85)
      (rw? #\x2028)))
)

(mat utf-16-codec
  (error? (r6rs:utf-16-codec #f))
  (error? (utf-16-codec #f))
 ; test decoding
  (let ()
    (define utf-16->string
      (lambda (eol bv)
        (let ([ip (transcoded-port
                    (let ([n (bytevector-length bv)] [i 0])
                      (make-custom-binary-input-port "foo"
                        (lambda (buf start count)
                          (let ([count (min (+ (random (min count 3)) 1) (fx- n i))])
                            (bytevector-copy! bv i buf start count)
                            (set! i (+ i count))
                            count))
                        (lambda () i)
                        (lambda (p) (set! i p))
                        #f))
                    (make-transcoder (utf-16-codec) eol (error-handling-mode replace)))])
          (call-with-string-output-port
            (lambda (op)
              (define (deref s) (if (eof-object? s) s (string-ref s 0)))
              (let again ()
                (let ([c (if (= (random 5) 3) (deref (get-string-n ip 1)) (get-char ip))])
                  (if (eof-object? c)
                      (let ([pos (port-position ip)])
                        (unless (= pos (bytevector-length bv))
                          (errorf #f "wrong pos ~s at eof" pos)))
                      (begin (put-char op c) (again))))))))))
    (define (big bv)
      (let ([n (bytevector-length bv)])
        (let ([newbv (make-bytevector (+ n 2))])
          (bytevector-u8-set! newbv 0 #xfe)
          (bytevector-u8-set! newbv 1 #xff)
          (do ([i 0 (fx+ i 2)])
              ((fx>= i (fx- n 1))
               (unless (fx= i n)
                 (bytevector-u8-set! newbv (fx+ i 2) (bytevector-u8-ref bv i))))
            (bytevector-u8-set! newbv (fx+ i 2) (bytevector-u8-ref bv i))
            (bytevector-u8-set! newbv (fx+ i 3) (bytevector-u8-ref bv (fx+ i 1))))
          newbv)))
    (define (little bv)
      (let ([n (bytevector-length bv)])
        (let ([newbv (make-bytevector (+ n 2))])
          (bytevector-u8-set! newbv 0 #xff)
          (bytevector-u8-set! newbv 1 #xfe)
          (do ([i 0 (fx+ i 2)])
              ((fx>= i (fx- n 1))
               (unless (fx= i n)
                 (bytevector-u8-set! newbv (fx+ i 2) (bytevector-u8-ref bv i))))
            (bytevector-u8-set! newbv (fx+ i 2) (bytevector-u8-ref bv (fx+ i 1)))
            (bytevector-u8-set! newbv (fx+ i 3) (bytevector-u8-ref bv i)))
          newbv)))
    (define (test eol bv s)
      (do ([n 1000 (fx- n 1)])
          ((fx= n 0))
        (let ([seed (random-seed)])
          (unless (and (equal? (utf-16->string eol bv) s)
                       (equal? (utf-16->string eol (big bv)) s)
                       (equal? (utf-16->string eol (little bv)) s))
            (errorf #f "failed, seed = ~s, bv = ~s, s = ~s" seed bv s)))))
    (test 'lf #vu8(#x00 #x61 #x00 #x0a) "a\n")
    (test 'lf #vu8(#x00 #x61 #x00 #x0d #x00 #x0a #x00 #x0d #x00 #x85 #x00 #x0d #x00 #x0d #x00 #x0a #x00 #x0d #x20 #x28) "a\n\n\n\n\n\n")
    (test 'crlf #vu8(#x00 #x61 #x00 #x0d #x00 #x0a #x00 #x0d #x00 #x85 #x00 #x0d #x00 #x0d #x00 #x0a #x00 #x0d #x20 #x28) "a\n\n\n\n\n\n")
    (test 'none #vu8(#x00 #x61 #x00 #x0d #x00 #x0a #x00 #x0d #x00 #x85 #x00 #x0d #x00 #x0d #x00 #x0a #x00 #x0d #x20 #x28) "a\r\n\r\x85;\r\r\n\r\x2028;")
    (test 'lf #vu8(#x00 #x0a #xd8 #x00 #xdc #x00 #xdb #xff #xdf #xff #xdc #x00 #xd8 #x00 #x00 #x00 #x00) "\n\x10000;\x10ffff;\xfffd;\xfffd;\xfffd;")
    #t)
 ; test encoding
  (let ()
    (define string->utf-16
      (lambda (eol s)
        (let-values ([(op getbv)
                      (let-values ([(bvop getbv) (open-bytevector-output-port)])
                        (values
                          (transcoded-port
                            (let ([i 0])
                              (make-custom-binary-output-port "foo"
                                (lambda (buf start count)
                                  (let ([count (random (min (fx+ count 1) 4))])
                                    (put-bytevector bvop buf start count)
                                    (set! i (+ i count))
                                    count))
                                (lambda () i)
                                #f #f))
                            (make-transcoder (utf-16be-codec) eol (error-handling-mode replace)))
                          getbv))])
          (let ([sip (open-string-input-port s)])
            (define (deref s) (if (eof-object? s) s (string-ref s 0)))
            (let again ()
              (let ([c (get-char sip)])
                (if (eof-object? c)
                    (let ([pos (port-position op)])
                      (close-port op)
                      (let ([bv (getbv)])
                        (unless (= pos (bytevector-length bv))
                          (errorf #f "wrong pos ~s at eof" pos))
                        bv))
                    (begin
                      (if (= (random 5) 3)
                          (put-string op (string c))
                          (put-char op c))
                      (again)))))))))
    (define (test eol s bv)
      (do ([n 1000 (fx- n 1)])
          ((fx= n 0))
        (let ([seed (random-seed)])
          (unless (equal? (string->utf-16 eol s) bv)
            (errorf #f "failed, seed = ~s, s = ~s, bv = ~s" seed s bv)))))
    (test 'lf "a\n" #vu8(#x00 #x61 #x00 #x0a))
    (test 'crlf "a\n" #vu8(#x00 #x61 #x00 #x0d #x00 #x0a))
    (test 'crnel "a\n" #vu8(#x00 #x61 #x00 #x0d #x00 #x85))
    (test 'nel "a\n" #vu8(#x00 #x61 #x00 #x85))
    (test 'ls "a\n" #vu8(#x00 #x61 #x20 #x28))
    (test 'none "a\r\n\r\x85;\r\r\n\r\x2028;" #vu8(#x00 #x61 #x00 #x0d #x00 #x0a #x00 #x0d #x00 #x85 #x00 #x0d #x00 #x0d #x00 #x0a #x00 #x0d #x20 #x28))
    (test 'lf "a\x10000;\x10ffff;\n" #vu8(#x00 #x61 #xd8 #x00 #xdc #x00 #xdb #xff #xdf #xff #x00 #x0a))
    #t)
)

(mat utf-16-BOMs
  (let ()
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should write BOM
        (set-port-position! iop n)                ; should actually position past BOM (position 2)
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 2)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))
        (and
          (eqv? n 0)
          (eqv? (get-char iop) #\h)
          (eqv? (port-position iop) 4)
          (equal? (get-string-all iop) "ello\n")
          (eqv? (port-position iop) 14)
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 2)
          (put-string iop "something longer than hello\n")
          (eq? (set-port-position! iop n) (void))
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let () ; same as preceding w/slightly different transcoder
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style lf) (error-handling-mode replace)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should write BOM
        (set-port-position! iop n)                ; should actually position past BOM (position 2)
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 2)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))
        (and
          (eqv? n 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 2)
          (put-string iop "something longer than hello\n")
          (eq? (set-port-position! iop n) (void))
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
    (define utf-16be-tx (make-transcoder (utf-16be-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should write BOM
        (set-port-position! iop n)                ; should actually position past BOM (position 2)
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 2)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16-tx))
       ; lookahead-char should position port past the BOM
        (define c (lookahead-char iop))
        (define n (port-position iop))            ; should be 2
        (and
          (eqv? c #\h)
          (eqv? n 2)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eq? (put-string iop "something longer than hello\n") (void))
          (eq? (set-port-position! iop n) (void))
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16be-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eqv? (get-char iop) #\xfeff)
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (get-char iop) #\xfeff)
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16le-tx (make-transcoder (utf-16le-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16le-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should not write BOM
        (set-port-position! iop n)                ; should set to 0
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16le-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eq? n 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "something longer than hello\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16be-tx (make-transcoder (utf-16be-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16be-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should not write BOM
        (set-port-position! iop n)                ; should set to 0
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16be-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eq? n 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "something longer than hello\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16be-tx (make-transcoder (utf-16be-codec) (eol-style none) (error-handling-mode raise)))
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16be-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")                ; should not write BOM
        (set-port-position! iop n)                ; should set to 0
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eq? n 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "something longer than hello\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "something longer than hello\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16le-tx (make-transcoder (utf-16le-codec) (eol-style none) (error-handling-mode raise)))
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16le-tx))
        (define n0 (port-position iop))             ; should be 0
        (put-char iop #\xfeff)                      ; insert explicit BOM
        (let ()
          (define n (port-position iop))            ; should be 0
          (put-string iop "hello\n")                ; should not write BOM
          (set-port-position! iop n)                ; should set to 0
          (and
            (eqv? n0 0)
            (eqv? n 2)
            (equal? (get-string-all iop) "hello\n")
            (eq? (close-port iop) (void)))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16-tx))
        (define n (port-position iop))
        (and (equal? (get-string-all iop) "hello\n")
             (begin
               (set-port-position! iop n)
               (put-string iop "hello again\n")
               (set-port-position! iop n))
             (and (equal? (get-string-all iop) "hello again\n")
                  (eq? (close-port iop) (void)))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16le-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eqv? (get-char iop) #\xfeff)           ; BOM should still be there
          (equal? (get-string-all iop) "hello again\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "hello yet again!\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello yet again!\n")   ; BOM is gone now
          (eq? (close-port iop) (void))))))
  (let ()
    (define utf-16le-tx (make-transcoder (utf-16le-codec) (eol-style none) (error-handling-mode raise)))
    (define faux-utf-16-tx (make-transcoder (utf-16-codec 'little) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16le-tx))
        (define n (port-position iop))            ; should be 0
        (put-string iop "hello\n")
        (set-port-position! iop n)
        (and
          (eqv? n 0)
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) faux-utf-16-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eqv? n 0)
          (equal? (get-string-all iop) "hello\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "hello again\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello again\n")
          (eq? (close-port iop) (void))))
      (let ()
        (define iop
          (open-file-input/output-port "testfile.ss" (file-options no-fail no-truncate)
            (buffer-mode block) utf-16le-tx))
        (define n (port-position iop))            ; should be 0
        (and
          (eqv? n 0)
          (equal? (get-string-all iop) "hello again\n")
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (eq? (put-string iop "hello yet again!\n") (void))
          (eq? (set-port-position! iop n) (void))
          (eqv? (port-position iop) 0)
          (equal? (get-string-all iop) "hello yet again!\n")
          (eq? (close-port iop) (void))))))
  (let ()
    (define-syntax and
      (let ()
        (import scheme)
        (syntax-rules ()
          [(_ e ...)
           (and (let ([x e]) (pretty-print x) x) ...)])))
    (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
    (and
      (let ()
        (define op
          (open-file-output-port "testfile.ss" (file-options replace)
            (buffer-mode block) utf-16-tx))
        (define n (port-position op))                  ; should be 0
        (and
          (eqv? n 0)
          (eq? (put-string op "hello\n") (void))       ; should write BOM
          (eq? (set-port-position! op n) (void))       ; should actually position past BOM (position 2)
          (eqv? (port-position op) 2)
          (eq? (put-string op "not hello\n") (void))   ; should not write (another) BOM
          (eq? (close-port op) (void))))
      (let ()
        (define ip
          (open-file-input-port "testfile.ss" (file-options)
            (buffer-mode block) utf-16-tx))
        (define n (port-position ip))                  ; should be 0
        (define c (lookahead-char ip))                 ; should be #\n
        (and
          (eqv? n 0)
          (eqv? c #\n)
          (eqv? (port-position ip) 2)
          (equal? (get-string-all ip) "not hello\n")
          (eq? (set-port-position! ip 2) (void))
          (equal? (get-string-all ip) "not hello\n")
          (eq? (close-port ip) (void))))))
)

(mat encode/decode-consistency
 ; verify that encoding/decoding is consistent (but not necessarily correct)
 ; crank up loop bounds to stress test
  (let ()
    (define (random-string n)
      (define (random-char) (integer->char (random 256)))
      (let ([s (make-string n)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i n))
          (string-set! s i (random-char)))
        s))
    (define (check who s1 s2)
      (unless (string=? s1 s2)
        (errorf who "failed for ~a"
          (parameterize ([print-unicode #f]) (format "~s" s1)))))
    (time
      (let ([latin-1-tx (make-transcoder (latin-1-codec) (eol-style none) (error-handling-mode raise))])
        (do ([n 1000 (fx- n 1)])
            ((fx= n 0) #t)
          (let ([s (random-string (random 50))])
            (check 'latin-1-test4 s (bytevector->string (string->bytevector s latin-1-tx) latin-1-tx)))))))
  (let ()
    (define (random-string n)
      (define (random-char)
        (integer->char
          (let ([k (random (fx- #x110000 (fx- #xe000 #xd800)))])
            (if (fx>= k #xd800)
                (fx+ k (fx- #xe000 #xd800))
                k))))
      (let ([s (make-string n)])
        (unless (fx= n 0)
         ; don't let a BOM sneak in at first character
          (string-set! s 0
            (let f () (let ([c (random-char)]) (if (memv c '(#\xfeff #\xfffe)) (f) c))))
          (do ([i 1 (fx+ i 1)])
              ((fx= i n))
            (string-set! s i (random-char))))
        s))
    (define (check who s1 s2)
      (unless (string=? s1 s2)
        (errorf who "failed for ~a"
          (parameterize ([print-unicode #f]) (format "~s" s1)))))
    (time
      (let ()
        (define utf-8-tx (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))
        (define utf-16-tx (make-transcoder (utf-16-codec) (eol-style none) (error-handling-mode raise)))
        (define utf-16le-tx (make-transcoder (utf-16le-codec) (eol-style none) (error-handling-mode raise)))
        (define utf-16be-tx (make-transcoder (utf-16be-codec) (eol-style none) (error-handling-mode raise)))
        (do ([n 1000 (fx- n 1)])
            ((fx= n 0) #t)
          (let ([s (random-string (random 50))])
            (check 'utf-8-test1 s (utf8->string (string->utf8 s)))
            (check 'utf-8-test2 s (utf8->string (string->bytevector s utf-8-tx)))
            (check 'utf-8-test3 s (bytevector->string (string->utf8 s) utf-8-tx))
            (check 'utf-8-test4 s (bytevector->string (string->bytevector s utf-8-tx) utf-8-tx))
            (check 'utf-16-test1a s (utf16->string (string->utf16 s 'big) 'big))
            (check 'utf-16-test1b s (utf16->string (string->utf16 s 'big) 'big #t))
            (check 'utf-16-test2a s (utf16->string (string->bytevector s utf-16-tx) 'big))
            (check 'utf-16-test2b s (utf16->string (string->bytevector s utf-16be-tx) 'big #t))
            (check 'utf-16-test2c s (utf16->string (string->bytevector s utf-16le-tx) 'little #t))
            (check 'utf-16-test3a s (bytevector->string (string->utf16 s 'big) utf-16-tx))
            (check 'utf-16-test3b s (bytevector->string (string->utf16 s 'big) utf-16be-tx))
            (check 'utf-16-test3c s (bytevector->string (string->utf16 s 'little) utf-16le-tx))
            (check 'utf-16-test4a s (bytevector->string (string->bytevector s utf-16-tx) utf-16-tx))
            (check 'utf-16-test4b s (bytevector->string (string->bytevector s utf-16le-tx) utf-16le-tx))
            (check 'utf-16-test4c s (bytevector->string (string->bytevector s utf-16be-tx) utf-16be-tx))
            (check 'utf-16-test5a s (utf16->string (string->utf16 s 'little) 'little))
            (check 'utf-16-test5b s (utf16->string (string->utf16 s 'little) 'little #t))
            (let* ([bv (string->bytevector s utf-16be-tx)]
                   [bvn (bytevector-length bv)]
                   [bv^ (make-bytevector (fx+ bvn 2))])
             ; insert big-endian BOM
              (bytevector-u8-set! bv^ 0 #xfe)
              (bytevector-u8-set! bv^ 1 #xff)
              (bytevector-copy! bv 0 bv^ 2 bvn)
              (check 'utf-16-test6 s (utf16->string bv^ 'big))
              (check 'utf-16-test7 s (bytevector->string bv^ utf-16-tx)))
            (let* ([bv (string->utf16 s 'little)]
                   [bvn (bytevector-length bv)]
                   [bv^ (make-bytevector (fx+ bvn 2))])
             ; insert little-endian BOM
              (bytevector-u8-set! bv^ 0 #xff)
              (bytevector-u8-set! bv^ 1 #xfe)
              (bytevector-copy! bv 0 bv^ 2 bvn)
              (check 'utf-16-test8 s (utf16->string bv^ 'little))
              (check 'utf-16-test9 s (bytevector->string bv^ utf-16-tx)))
          (check 'utf-32-test1a s (utf32->string (string->utf32 s 'big) 'big))
          (check 'utf-32-test1a s (utf32->string (string->utf32 s 'big) 'big #t))
          (check 'utf-32-test2a s (utf32->string (string->utf32 s 'little) 'little))
          (check 'utf-32-test2b s (utf32->string (string->utf32 s 'little) 'little #f)))))))
)

(mat string<->bytevector-conversions
 ; adapted with minor modifications from bv2string.sch, which is:
 ;
 ; Copyright 2007 William D Clinger.
 ;
 ; Permission to copy this software, in whole or in part, to use this
 ; software for any lawful purpose, and to redistribute this software
 ; is granted subject to the restriction that all copies made of this
 ; software must include this copyright notice in full.
 ;
 ; I also request that you send me a copy of any improvements that you
 ; make to this software so that they may be incorporated within it to
 ; the benefit of the Scheme community.
  (begin
    (library (bv2string) (export main)
      (import (rnrs base)
              (rnrs unicode)
              (rename (rnrs bytevectors)
                (utf8->string rnrs:utf8->string)
                (string->utf8 rnrs:string->utf8))
              (rnrs control)
              (rnrs io simple)
              (rnrs mutable-strings))

      ; Crude test rig, just for benchmarking.

      (define utf8->string)
      (define string->utf8)

      (define (test name actual expected)
        (if (not (equal? actual expected))
            (error 'test name)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;
      ; The R6RS doesn't specify exactly how many replacement
      ; characters get generated by an encoding or decoding error,
      ; so the results of some tests are compared by treating any
      ; sequence of consecutive replacement characters the same as
      ; a single replacement character.
      ;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (string~? s1 s2)
        (define (replacement? c)
          (char=? c #\xfffd))
        (define (canonicalized s)
          (let loop ((rchars (reverse (string->list s)))
                     (cchars '()))
            (cond ((or (null? rchars) (null? (cdr rchars)))
                   (list->string cchars))
                  ((and (replacement? (car rchars))
                        (replacement? (cadr rchars)))
                   (loop (cdr rchars) cchars))
                  (else
                   (loop (cdr rchars) (cons (car rchars) cchars))))))
        (string=? (canonicalized s1) (canonicalized s2)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;
      ; Basic sanity tests, followed by stress tests on random inputs.
      ;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (string-bytevector-tests
               *random-stress-tests* *random-stress-test-max-size*)

        (define (test-roundtrip bvec tostring tobvec)
          (let* ((s1 (tostring bvec))
                 (b2 (tobvec s1))
                 (s2 (tostring b2)))
            (test "round trip of string conversion" (string=? s1 s2) #t)))

        ; This random number generator doesn't have to be good.
        ; It just has to be fast.

        (define random
          (letrec ((random14
                    (lambda (n)
                      (set! x (mod (+ (* a x) c) (+ m 1)))
                      (mod (div x 8) n)))
                   (a 701)
                   (x 1)
                   (c 743483)
                   (m 524287)
                   (loop
                    (lambda (q r n)
                      (if (zero? q)
                          (mod r n)
                          (loop (div q 16384)
                                (+ (* 16384 r) (random14 16384))
                                n)))))
            (lambda (n)
              (if (< n 16384)
                  (random14 n)
                  (loop (div n 16384) (random14 16384) n)))))
       
        ; Returns a random bytevector of length up to n.

        (define (random-bytevector n)
          (let* ((n (random n))
                 (bv (make-bytevector n)))
            (do ((i 0 (+ i 1)))
                ((= i n) bv)
              (bytevector-u8-set! bv i (random 256)))))

        ; Returns a random bytevector of even length up to n.

        (define (random-bytevector2 n)
          (let* ((n (random n))
                 (n (if (odd? n) (+ n 1) n))
                 (bv (make-bytevector n)))
            (do ((i 0 (+ i 1)))
                ((= i n) bv)
              (bytevector-u8-set! bv i (random 256)))))

        ; Returns a random bytevector of multiple-of-4 length up to n.

        (define (random-bytevector4 n)
          (let* ((n (random n))
                 (n (* 4 (round (/ n 4))))
                 (bv (make-bytevector n)))
            (do ((i 0 (+ i 1)))
                ((= i n) bv)
              (bytevector-u8-set! bv i (random 256)))))

        (test "utf-8, BMP"
              (bytevector=? (string->utf8 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                            '#vu8(#x6b
                                  #x7f
                                  #b11000010 #b10000000
                                  #b11011111 #b10111111
                                  #b11100000 #b10100000 #b10000000
                                  #b11101111 #b10111111 #b10111111))
              #t)

        (test "utf-8, supplemental"
              (bytevector=? (string->utf8 "\x010000;\x10ffff;")
                            '#vu8(#b11110000 #b10010000 #b10000000 #b10000000
                                  #b11110100 #b10001111 #b10111111 #b10111111))
              #t)

        (test "utf-8, errors 1"
              (string~? (utf8->string '#vu8(#x61                             ; a
                                            #xc0 #x62                        ; ?b
                                            #xc1 #x63                        ; ?c
                                            #xc2 #x64                        ; ?d
                                            #x80 #x65                        ; ?e
                                            #xc0 #xc0 #x66                   ; ??f
                                            #xe0 #x67                        ; ?g
                                           ))
                        "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e\xfffd;\xfffd;f\xfffd;g")
              #t)

        (test "utf-8, errors 2"
              (string~? (utf8->string '#vu8(#xe0 #x80 #x80 #x68              ; ???h
                                            #xe0 #xc0 #x80 #x69              ; ???i
                                            #xf0 #x6a                        ; ?j
                                           ))
                        "\xfffd;\xfffd;\xfffd;h\xfffd;\xfffd;\xfffd;i\xfffd;j")
              #t)

        (test "utf-8, errors 3"
              (string~? (utf8->string '#vu8(#x61                             ; a
                                            #xf0 #x80 #x80 #x80 #x62         ; ????b
                                            #xf0 #x90 #x80 #x80 #x63         ; .c
                                           ))
                        "a\xfffd;\xfffd;\xfffd;\xfffd;b\x10000;c")
              #t)

        (test "utf-8, errors 4"
              (string~? (utf8->string '#vu8(#x61                             ; a
                                            #xf0 #xbf #xbf #xbf #x64         ; .d
                                            #xf0 #xbf #xbf #x65              ; ?e
                                            #xf0 #xbf #x66                   ; ?f
                                           ))
                        "a\x3ffff;d\xfffd;e\xfffd;f")
              #t)

        (test "utf-8, errors 5"
              (string~? (utf8->string '#vu8(#x61                             ; a
                                            #xf4 #x8f #xbf #xbf #x62         ; .b
                                            #xf4 #x90 #x80 #x80 #x63         ; ????c
                                           ))

                        "a\x10ffff;b\xfffd;\xfffd;\xfffd;\xfffd;c")
              #t)

        (test "utf-8, errors 6"
              (string~? (utf8->string '#vu8(#x61                             ; a
                                            #xf5 #x80 #x80 #x80 #x64         ; ????d
                                           ))

                        "a\xfffd;\xfffd;\xfffd;\xfffd;d")
              #t)

        ; ignores BOM signature
        ; Officially, there is no BOM signature for UTF-8,
        ; so this test is commented out.

      #;(test "utf-8, BOM"
              (string=? (utf8->string '#vu8(#xef #xbb #xbf #x61 #x62 #x63 #x64))
                        "abcd")
              #t)

        (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

        (do ((i 0 (+ i 1)))
            ((= i *random-stress-tests*))
          (test-roundtrip (random-bytevector *random-stress-test-max-size*)
                          utf8->string string->utf8))

        (test "utf-16, BMP"
              (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                            '#vu8(#x00 #x6b
                                  #x00 #x7f
                                  #x00 #x80
                                  #x07 #xff
                                  #x08 #x00
                                  #xff #xff))
              #t)

        (test "utf-16le, BMP"
              (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                                           'little)
                            '#vu8(#x6b #x00
                                  #x7f #x00
                                  #x80 #x00
                                  #xff #x07
                                  #x00 #x08
                                  #xff #xff))
              #t)

        (test "utf-16, supplemental"
              (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;")
                            '#vu8(#xd8 #x00 #xdc #x00
                                  #xdb #xb7 #xdc #xba
                                  #xdb #xff #xdf #xff))
              #t)

        (test "utf-16le, supplemental"
              (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;" 'little)
                            '#vu8(#x00 #xd8 #x00 #xdc
                                  #xb7 #xdb #xba #xdc
                                  #xff #xdb #xff #xdf))
              #t)

        (test "utf-16be"
              (bytevector=? (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd")
                            (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd" 'big))
              #t)

        (test "utf-16, errors 1"
              (string~? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                        (utf16->string
                         '#vu8(#x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)
                         'big))
              #t)

        (test "utf-16, errors 2"
              (string~? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                        (utf16->string
                         '#vu8(#x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)
                         'big #t))
              #t)

        (test "utf-16, errors 3"
              (string~? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                        (utf16->string
                         '#vu8(#xfe #xff     ; big-endian BOM
                               #x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)
                         'big))
              #t)

        (test "utf-16, errors 4"
              (string~? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                        (utf16->string
                         '#vu8(#x6b #x00
                               #x7f #x00
                               #x80 #x00
                               #xff #x07
                               #x00 #x08
                               #xff #xff)
                         'little #t))
              #t)

        (test "utf-16, errors 5"
              (string~? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                        (utf16->string
                         '#vu8(#xff #xfe     ; little-endian BOM
                               #x6b #x00
                               #x7f #x00
                               #x80 #x00
                               #xff #x07
                               #x00 #x08
                               #xff #xff)
                         'big))
              #t)

        (let ((tostring        (lambda (bv) (utf16->string bv 'big)))
              (tostring-big    (lambda (bv) (utf16->string bv 'big #t)))
              (tostring-little (lambda (bv) (utf16->string bv 'little #t)))
              (tobvec          string->utf16)
              (tobvec-big      (lambda (s) (string->utf16 s 'big)))
              (tobvec-little   (lambda (s) (string->utf16 s 'little))))

          (do ((i 0 (+ i 1)))
              ((= i *random-stress-tests*))
            (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                            tostring tobvec)
            (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                            tostring-big tobvec-big)
            (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                            tostring-little tobvec-little)))

        (test "utf-32"
              (bytevector=? (string->utf32 "abc")
                            '#vu8(#x00 #x00 #x00 #x61
                                  #x00 #x00 #x00 #x62
                                  #x00 #x00 #x00 #x63))
              #t)

        (test "utf-32be"
              (bytevector=? (string->utf32 "abc" 'big)
                            '#vu8(#x00 #x00 #x00 #x61
                                  #x00 #x00 #x00 #x62
                                  #x00 #x00 #x00 #x63))
              #t)

        (test "utf-32le"
              (bytevector=? (string->utf32 "abc" 'little)
                            '#vu8(#x61 #x00 #x00 #x00
                                  #x62 #x00 #x00 #x00
                                  #x63 #x00 #x00 #x00))
              #t)

        (test "utf-32, errors 1"
              (string~? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                         'big))
              #t)

        (test "utf-32, errors 2"
              (string~? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                         'big #t))
              #t)

        (test "utf-32, errors 3"
              (string~? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                               #x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                         'big))
              #t)

        (test "utf-32, errors 4"
              (string~? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                               #x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                         'big #t))
              #t)

        (test "utf-32, errors 5"
              (string~? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)
                         'little #t))
              #t)

        (test "utf-32, errors 6"
              (string~? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                               #x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)
                         'big))
              #t)

        (test "utf-32, errors 7"
              (string~? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                        (utf32->string
                         '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                               #x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)
                         'little #t))
              #t)

        (let ((tostring        (lambda (bv) (utf32->string bv 'big)))
              (tostring-big    (lambda (bv) (utf32->string bv 'big #t)))
              (tostring-little (lambda (bv) (utf32->string bv 'little #t)))
              (tobvec          string->utf32)
              (tobvec-big      (lambda (s) (string->utf32 s 'big)))
              (tobvec-little   (lambda (s) (string->utf32 s 'little))))

          (do ((i 0 (+ i 1)))
              ((= i *random-stress-tests*))
            (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                            tostring tobvec)
            (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                            tostring-big tobvec-big)
            (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                            tostring-little tobvec-little)))

      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;
      ; Exhaustive tests.
      ;
      ; Tests string <-> bytevector conversion on strings
      ; that contain every Unicode scalar value.
      ;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (exhaustive-string-bytevector-tests)

        ; Tests throughout an inclusive range.

        (define (test-char-range lo hi tostring tobytevector)
          (let* ((n (+ 1 (- hi lo)))
                 (s (make-string n))
                 (replacement-character (integer->char #xfffd)))
            (do ((i lo (+ i 1)))
                ((> i hi))
              (let ((c (if (or (<= 0 i #xd7ff)
                               (<= #xe000 i #x10ffff))
                           (integer->char i)
                           replacement-character)))
                (string-set! s (- i lo) c)))
            (test "test of long string conversion"
                  (string=? (tostring (tobytevector s)) s) #t)))

        (define (test-exhaustively name tostring tobytevector)
         ;(display "Testing ")
         ;(display name)
         ;(display " conversions...")
         ;(newline)
          (test-char-range 0 #xffff tostring tobytevector)
          (test-char-range #x10000 #x1ffff tostring tobytevector)
          (test-char-range #x20000 #x2ffff tostring tobytevector)
          (test-char-range #x30000 #x3ffff tostring tobytevector)
          (test-char-range #x40000 #x4ffff tostring tobytevector)
          (test-char-range #x50000 #x5ffff tostring tobytevector)
          (test-char-range #x60000 #x6ffff tostring tobytevector)
          (test-char-range #x70000 #x7ffff tostring tobytevector)
          (test-char-range #x80000 #x8ffff tostring tobytevector)
          (test-char-range #x90000 #x9ffff tostring tobytevector)
          (test-char-range #xa0000 #xaffff tostring tobytevector)
          (test-char-range #xb0000 #xbffff tostring tobytevector)
          (test-char-range #xc0000 #xcffff tostring tobytevector)
          (test-char-range #xd0000 #xdffff tostring tobytevector)
          (test-char-range #xe0000 #xeffff tostring tobytevector)
          (test-char-range #xf0000 #xfffff tostring tobytevector)
          (test-char-range #x100000 #x10ffff tostring tobytevector))

        ; Feel free to replace this with your favorite timing macro.

        (define (timeit x) x)

        (timeit (test-exhaustively "UTF-8" utf8->string string->utf8))

        ; NOTE:  An unfortunate misunderstanding led to a late deletion
        ; of single-argument utf16->string from the R6RS.  To get the
        ; correct effect of single-argument utf16->string, you have to
        ; use two arguments, as below.
        ;
        ;(timeit (test-exhaustively "UTF-16" utf16->string string->utf16))

        (timeit (test-exhaustively "UTF-16"
                                   (lambda (bv) (utf16->string bv 'big))
                                   string->utf16))

        ; NOTE:  To get the correct effect of two-argument utf16->string,
        ; you have to use three arguments, as below.

        (timeit (test-exhaustively "UTF-16BE"
                                   (lambda (bv) (utf16->string bv 'big #t))
                                   (lambda (s) (string->utf16 s 'big))))

        (timeit (test-exhaustively "UTF-16LE"
                                   (lambda (bv) (utf16->string bv 'little #t))
                                   (lambda (s) (string->utf16 s 'little))))

        ; NOTE:  An unfortunate misunderstanding led to a late deletion
        ; of single-argument utf32->string from the R6RS.  To get the
        ; correct effect of single-argument utf32->string, you have to
        ; use two arguments, as below.
        ;
        ;(timeit (test-exhaustively "UTF-32" utf32->string string->utf32))

        (timeit (test-exhaustively "UTF-32"
                                   (lambda (bv) (utf32->string bv 'big))
                                   string->utf32))

        ; NOTE:  To get the correct effect of two-argument utf32->string,
        ; you have to use three arguments, as below.

        (timeit (test-exhaustively "UTF-32BE"
                                   (lambda (bv) (utf32->string bv 'big #t))
                                   (lambda (s) (string->utf32 s 'big))))

        (timeit (test-exhaustively "UTF-32LE"
                                   (lambda (bv) (utf32->string bv 'little #t))
                                   (lambda (s) (string->utf32 s 'little)))))

      (define (main p1 p2)
        (set! utf8->string p1)
        (set! string->utf8 p2)
        (string-bytevector-tests 2 1000)
        (exhaustive-string-bytevector-tests)))
    #t)
 ; first test w/built-in utf8->string and string->utf8
  (begin
    (let () (import (bv2string)) (main utf8->string string->utf8))
    #t)
 ; next test w/utf8->string and string->utf8 synthesized from utf-8-codec
  (let ()
    (define (utf8->string bv)
      (get-string-all (open-bytevector-input-port bv
                        (make-transcoder (utf-8-codec) 'none))))
    (define (string->utf8 s)
      (let-values ([(op get) (open-bytevector-output-port
                               (make-transcoder (utf-8-codec) 'none))])
        (put-string op s)
        (get)))
    (let () (import (bv2string)) (main utf8->string string->utf8))
    #t)
)

(mat open-process-ports ; see also unix.ms (mat nonblocking ...)
  (begin
    (define ($check-port p xput-port? bt-port?)
      (define-syntax err? 
        (syntax-rules ()
          [(_ e1 e2 ...) (guard (c [#t #t]) e1 e2 ... #f)]))
      (unless (and (xput-port? p) (bt-port? p) (file-port? p))
        (errorf #f "~s is not as it should be" p))
      (let ([fd (port-file-descriptor p)])
        (unless (fixnum? fd)
          (errorf #f "unexpected file descriptor ~s" fd)))
      (when (or (port-has-port-position? p)
                (port-has-set-port-position!? p)
                (port-has-port-length? p)
                (port-has-set-port-length!? p))
        (errorf #f "unexpected port-has-xxx results for ~s" p))
      (unless (and (err? (port-position p))
                   (err? (set-port-position! p 0))
                   (err? (port-length p))
                   (err? (set-port-length! p 0)))
        (errorf #f "no error for getting/setting port position/length on ~s" p)))
    (define $emit-dot
      (let ([n 0])
        (lambda ()
          (display ".")
          (set! n (modulo (+ n 1) 72))
          (when (= n 0) (newline))
          (flush-output-port))))
    #t)
 ; test binary ports
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (patch-exec-path $cat_flush))])
    (define put-string
      (lambda (bp s)
        (put-bytevector bp (string->utf8 s))))
    (define get-string-some
      (lambda (bp)
        (let ([x (get-bytevector-some bp)])
          (if (eof-object? x) x (utf8->string x)))))
    (define get-string-n
      (lambda (bp n)
        (let ([x (get-bytevector-n bp n)])
          (if (eof-object? x) x (utf8->string x)))))
    (dynamic-wind
      void
      (lambda ()
        (put-string to-stdin "life in the fast lane\n")
        (flush-output-port to-stdin)
        (let f ()
          ($check-port to-stdin output-port? binary-port?)
          ($check-port from-stdout input-port? binary-port?)
          ($check-port from-stderr input-port? binary-port?)
          (when (input-port-ready? from-stderr)
            (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
          (if (input-port-ready? from-stdout)
              (let ([s (get-string-n from-stdout 10)])
                (unless (equal? s "life in th")
                  (errorf #f "unexpected from-stdout string ~s" s)))
              (begin
                ($emit-dot)
                (f))))
        (let f ([all ""])
          (unless (equal? all "e fast lane\n")
            (when (input-port-ready? from-stderr)
              (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
            (let ([s (get-string-some from-stdout)])
              ($emit-dot)
              (f (string-append all s)))))
        (and
          (not (input-port-ready? from-stderr))
          (not (input-port-ready? from-stdout))
          (begin
            (close-port to-stdin)
            (let f ()
              (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                ($emit-dot)
                (f)))
            #t)))
      (lambda ()
        (close-port to-stdin)
        (close-port from-stdout)
        (close-port from-stderr))))
 ; test binary ports w/buffer-mode none
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (patch-exec-path $cat_flush) (buffer-mode none))])
    (define put-string
      (lambda (bp s)
        (put-bytevector bp (string->utf8 s))))
    (define get-string-some
      (lambda (bp)
        (let ([x (get-bytevector-some bp)])
          (if (eof-object? x) x (utf8->string x)))))
    (define get-string-n
      (lambda (bp n)
        (let ([x (get-bytevector-n bp n)])
          (if (eof-object? x) x (utf8->string x)))))
    (dynamic-wind
      void
      (lambda ()
        ($check-port to-stdin output-port? binary-port?)
        ($check-port from-stdout input-port? binary-port?)
        ($check-port from-stderr input-port? binary-port?)
        (put-string to-stdin "life in the fast lane\n")
        (flush-output-port to-stdin)
        (let f ()
          (when (input-port-ready? from-stderr)
            (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
          (if (input-port-ready? from-stdout)
              (let ([s (get-string-n from-stdout 10)])
                (unless (equal? s "life in th")
                  (errorf #f "unexpected from-stdout string ~s" s)))
              (begin
                ($emit-dot)
                (f))))
        (let f ([all ""])
          (unless (equal? all "e fast lane\n")
            (when (input-port-ready? from-stderr)
              (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
            (let ([s (get-string-some from-stdout)])
              ($emit-dot)
              (f (string-append all s)))))
        (and
          (not (input-port-ready? from-stderr))
          (not (input-port-ready? from-stdout))
          (begin
            (close-port to-stdin)
            (let f ()
              (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                ($emit-dot)
                (f)))
            #t)))
      (lambda ()
        (close-port to-stdin)
        (close-port from-stdout)
        (close-port from-stderr))))
 ; test textual ports
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (patch-exec-path $cat_flush) (buffer-mode block) (native-transcoder))])
    (dynamic-wind
      void
      (lambda ()
        ($check-port to-stdin output-port? textual-port?)
        ($check-port from-stdout input-port? textual-port?)
        ($check-port from-stderr input-port? textual-port?)
        (put-string to-stdin "life in the fast lane\n")
        (flush-output-port to-stdin)
        (let f ()
          (when (input-port-ready? from-stderr)
            (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
          (if (input-port-ready? from-stdout)
              (let ([s (get-string-n from-stdout 10)])
                (unless (equal? s "life in th")
                  (errorf #f "unexpected from-stdout string ~s" s)))
              (begin
                ($emit-dot)
                (f))))
        (let f ([all ""])
          (unless (equal? all "e fast lane\n")
            (when (input-port-ready? from-stderr)
              (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
            (let ([s (get-string-some from-stdout)])
              ($emit-dot)
              (f (string-append all s)))))
        (and
          (not (input-port-ready? from-stderr))
          (not (input-port-ready? from-stdout))
          (begin
            (close-port to-stdin)
            (let f ()
              (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                ($emit-dot)
                (f)))
            #t)))
      (lambda ()
        (close-port to-stdin)
        (close-port from-stdout)
        (close-port from-stderr))))
 ; test textual ports w/buffer-mode none
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (patch-exec-path $cat_flush) (buffer-mode none) (native-transcoder))])
    (dynamic-wind
      void
      (lambda ()
        ($check-port to-stdin output-port? textual-port?)
        ($check-port from-stdout input-port? textual-port?)
        ($check-port from-stderr input-port? textual-port?)
        (put-string to-stdin "life in the fast lane\n")
        (flush-output-port to-stdin)
        (let f ()
          (when (input-port-ready? from-stderr)
            (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
          (if (input-port-ready? from-stdout)
              (let ([s (get-string-n from-stdout 10)])
                (unless (equal? s "life in th")
                  (errorf #f "unexpected from-stdout string ~s" s)))
              (begin
                ($emit-dot)
                (f))))
        (let f ([all ""])
          (unless (equal? all "e fast lane\n")
            (when (input-port-ready? from-stderr)
              (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
            (let ([s (get-string-some from-stdout)])
              ($emit-dot)
              (f (string-append all s)))))
        (and
          (not (input-port-ready? from-stderr))
          (not (input-port-ready? from-stdout))
          (begin
            (close-port to-stdin)
            (let f ()
              (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                ($emit-dot)
                (f)))
            #t)))
      (lambda ()
        (close-port to-stdin)
        (close-port from-stdout)
        (close-port from-stderr))))
 ; test textual ports w/buffer-mode line
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (patch-exec-path $cat_flush) (buffer-mode line) (native-transcoder))])
    (dynamic-wind
      void
      (lambda ()
        ($check-port to-stdin output-port? textual-port?)
        ($check-port from-stdout input-port? textual-port?)
        ($check-port from-stderr input-port? textual-port?)
        (put-string to-stdin "life in the fast lane\n")
        (flush-output-port to-stdin)
        (let f ()
          (when (input-port-ready? from-stderr)
            (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
          (if (input-port-ready? from-stdout)
              (let ([s (get-string-n from-stdout 10)])
                (unless (equal? s "life in th")
                  (errorf #f "unexpected from-stdout string ~s" s)))
              (begin
                ($emit-dot)
                (f))))
        (let f ([all ""])
          (unless (equal? all "e fast lane\n")
            (when (input-port-ready? from-stderr)
              (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
            (let ([s (get-string-some from-stdout)])
              ($emit-dot)
              (f (string-append all s)))))
        (and
          (not (input-port-ready? from-stderr))
          (not (input-port-ready? from-stdout))
          (begin
            (close-port to-stdin)
            (let f ()
              (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                ($emit-dot)
                (f)))
            #t)))
      (lambda ()
        (close-port to-stdin)
        (close-port from-stdout)
        (close-port from-stderr))))
)

(mat to-fold-or-not-to-fold
  (begin
    (define ($readit cs? s)
      (define (string-append* s1 . ls)
        (let f ([s1 s1] [ls ls] [n 0])
          (let ([n1 (string-length s1)])
            (if (null? ls)
                (let ([s (make-string (fx+ n n1))])
                  (string-copy! s1 0 s n n1)
                  s)
                (let ([s (f (car ls) (cdr ls) (fx+ n n1 1))])
                  (string-copy! s1 0 s n n1)
                  (string-set! s (fx+ n n1) #\$)
                  s)))))
      (apply string-append*
        (let ([sip (open-input-string s)])
          (parameterize ([case-sensitive cs?])
            (let f ()
              (let ([x (get-datum sip)])
                (if (eof-object? x)
                    '()
                    (cons (cond
                            [(gensym? x)
                             (string-append (symbol->string x) "%"
                               (gensym->unique-string x))]
                            [(symbol? x) (symbol->string x)]
                            [(char? x) (string x)]
                            [else (error 'string-append* "unexpected ~s" x)])
                          (f)))))))))
    #t)
  (case-sensitive)
  (equal?
    ($readit #t "To be or NOT to bE")
    "To$be$or$NOT$to$bE")
  (equal?
    ($readit #f "To be or NOT to bE")
    "to$be$or$not$to$be")
  (equal?
    ($readit #t "To be #!no-fold-case or NOT #!fold-case to bE")
    "To$be$or$NOT$to$be")
  (equal?
    ($readit #t "To be #!fold-case or NOT #!no-fold-case to bE")
    "To$be$or$not$to$bE")
  (equal?
    ($readit #f "To be #!no-fold-case or NOT #!fold-case to bE")
    "to$be$or$NOT$to$be")
  (equal?
    ($readit #f "To be #!fold-case or NOT #!no-fold-case to bE")
    "to$be$or$not$to$bE")
 ; check delimiting
  (equal?
    ($readit #f "To be#!fold-caseor NOT#!no-fold-caseto bE")
    "to$be$or$not$to$bE")
 ; verify case folding is not disabled when Unicode hex escape seen
  (equal?
    ($readit #t "ab\\x43;de")
    "abCde")
  (equal?
    ($readit #f "ab\\x43;de")
    "abcde")
  (equal?
    ($readit #t "#!fold-case ab\\x43;de")
    "abcde")
  (equal?
    ($readit #f "#!fold-case ab\\x43;de")
    "abcde")
  (equal?
    ($readit #t "#!no-fold-case ab\\x43;de")
    "abCde")
  (equal?
    ($readit #f "#!no-fold-case ab\\x43;de")
    "abCde")
 ; verify case folding still works when string changes size
  (equal?
    ($readit #t "Stra\xDF;e #!no-fold-case Stra\xDF;e #!fold-case Stra\xDF;e")
    "Stra\xDF;e$Stra\xDF;e$strasse")
  (equal?
    ($readit #f "Stra\xDF;e #!no-fold-case Stra\xDF;e #!fold-case Stra\xDF;e")
    "strasse$Stra\xDF;e$strasse")
  (equal?
    ($readit #t "Stra\xDF;e #!fold-case Stra\xDF;e #!no-fold-case Stra\xDF;e")
    "Stra\xDF;e$strasse$Stra\xDF;e")
  (equal?
    ($readit #f "Stra\xDF;e #!fold-case Stra\xDF;e #!no-fold-case Stra\xDF;e")
    "strasse$strasse$Stra\xDF;e")
  (equal?
    ($readit #t "Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
 ; verify case folding is disabled when vertical bars or backslashes
 ; (other than those for Unicode hex escapes) appear
  (equal?
    ($readit #f "Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
  (equal?
    ($readit #t "#!fold-case Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
  (equal?
    ($readit #f "#!fold-case Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
  (equal?
    ($readit #t "#!no-fold-case Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
  (equal?
    ($readit #f "#!no-fold-case Aa|b C|dE abC|D e|Fg \\#Ab\\ C a\\B\\ cd")
    "Aab CdE$abCD eFg$#Ab C$aB cd")
 ; verify proper case folding for gensyms
  (equal?
    ($readit #t "#{aBc DeF1}")
    "aBc%DeF1")
  (equal?
    ($readit #f "#{aBc DeF2}")
    "abc%def2")
  (equal?
    ($readit #t "#!fold-case #{aBc DeF3}")
    "abc%def3")
  (equal?
    ($readit #f "#!fold-case #{aBc DeF4}")
    "abc%def4")
  (equal?
    ($readit #t "#!no-fold-case #{aBc DeF5}")
    "aBc%DeF5")
  (equal?
    ($readit #f "#!no-fold-case #{aBc DeF6}")
    "aBc%DeF6")
  (equal?
    ($readit #t "#{aBc De\\F7}")
    "aBc%DeF7")
  (equal?
    ($readit #f "#{aBc De\\F8}")
    "abc%DeF8")
  (equal?
    ($readit #t "#!fold-case #{aBc De\\F9}")
    "abc%DeF9")
  (equal?
    ($readit #f "#!fold-case #{aBc De\\F10}")
    "abc%DeF10")
  (equal?
    ($readit #t "#!no-fold-case #{aBc De\\F11}")
    "aBc%DeF11")
  (equal?
    ($readit #f "#!no-fold-case #{aBc De\\F12}")
    "aBc%DeF12")
  (equal?
    ($readit #t "#{a\\Bc DeF13}")
    "aBc%DeF13")
  (equal?
    ($readit #f "#{a\\Bc DeF14}")
    "aBc%def14")
  (equal?
    ($readit #t "#!fold-case #{a\\Bc DeF15}")
    "aBc%def15")
  (equal?
    ($readit #f "#!fold-case #{a\\Bc DeF16}")
    "aBc%def16")
  (equal?
    ($readit #t "#!no-fold-case #{a\\Bc DeF17}")
    "aBc%DeF17")
  (equal?
    ($readit #f "#!no-fold-case #{a\\Bc DeF18}")
    "aBc%DeF18")
  (equal?
    ($readit #t "#{a\\Bc De\\F19}")
    "aBc%DeF19")
  (equal?
    ($readit #f "#{a\\Bc De\\F20}")
    "aBc%DeF20")
  (equal?
    ($readit #t "#!fold-case #{a\\Bc De\\F21}")
    "aBc%DeF21")
  (equal?
    ($readit #f "#!fold-case #{a\\Bc De\\F22}")
    "aBc%DeF22")
  (equal?
    ($readit #t "#!no-fold-case #{a\\Bc De\\F23}")
    "aBc%DeF23")
  (equal?
    ($readit #f "#!no-fold-case #{a\\Bc De\\F24}")
    "aBc%DeF24")
  (equal?
    ($readit #t "#\\newline")
    "\n")
  (equal?
    ($readit #f "#\\newline")
    "\n")
  (equal?
    ($readit #f "#!fold-case #\\newline")
    "\n")
  (equal?
    ($readit #f "#!fold-case #\\newline")
    "\n")
  (equal?
    ($readit #f "#!no-fold-case #\\newline")
    "\n")
  (equal?
    ($readit #f "#!no-fold-case #\\newline")
    "\n")
  (error? ($readit #t "#\\newLine"))
  (equal?
    ($readit #f "#\\newLine")
    "\n")
  (equal?
    ($readit #t "#!fold-case #\\newLine")
    "\n")
  (equal?
    ($readit #f "#!fold-case #\\newLine")
    "\n")
  (error? ($readit #t "#!no-fold-case #\\newLine"))
  (error? ($readit #f "#!no-fold-case #\\newLine"))
)
