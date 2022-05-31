;;; unix.ms
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

(if (or (windows?) (equal? (getenv "USER") "root") (embedded?))
    (mat unix-file-io
      (error? (errorf 'open-output-file "failed for testfile.ss: file exists"))
      (error? (errorf 'open-output-file "failed for testfile.ss: file exists"))
      (error? (errorf 'open-output-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'open-output-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'open-output-file "failed for testfile.ss: file exists"))
      (error? (errorf 'open-output-file "failed for testfile.ss: file exists"))
      (error? (errorf 'open-output-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'open-output-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'open-input-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'open-input-output-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'with-output-to-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'with-input-from-file "failed for testfile.ss: permission denied"))
      (error? (errorf 'call-with-input-file "failed for testfile.ss: permission denied"))
    )
    (mat unix-file-io
      (let ([p (open-output-file "/dev/null" 'truncate)])
        (close-output-port p)
        #t)
      (let ([p (open-output-file "testfile.ss" 'truncate)])
        (close-output-port p)
        (system "chmod -w testfile.ss")
        #t)
      (error? (open-output-file "testfile.ss"))
      (error? (open-output-file "testfile.ss" 'error))
      (error? (open-output-file "testfile.ss" 'truncate))
      (error? (open-output-file "testfile.ss" 'append))
      (let ([p (open-output-file "testfile.ss" 'replace)])
        (close-output-port p)
        #t)
      (delete-file "testfile.ss" #f)
      (eqv?
        (with-output-to-file "testfile.ss"
          (lambda () (display "hello\n"))
          '(mode #o000))
        (void))
      (error? (open-output-file "testfile.ss"))
      (error? (open-output-file "testfile.ss" 'error))
      (error? (open-output-file "testfile.ss" 'truncate))
      (error? (open-output-file "testfile.ss" 'append))
      (error? (open-input-file "testfile.ss"))
      (error? (open-input-output-file "testfile.ss"))
      (error? (with-output-to-file "testfile.ss" void '(truncate)))
      (error? (with-input-from-file "testfile.ss" void))
      (error? (call-with-input-file "testfile.ss" values))
      (delete-file "testfile.ss" #f)
    )
)

(mat system
  (error? ; not a string
    (system 5))
)

(unless (windows?)
  (mat system
    (eqv? (with-output-to-file "testfile.ss" void '(replace)) (void))
    (begin
      (system "rm -f testfile.ss")
      (system "echo hello > testfile.ss")
      (let ([p (open-input-file "testfile.ss")])
        (and (eq? (read p) 'hello)
             (begin (close-input-port p) #t))))
  )
)

(unless (windows?)
  (mat process-port
    (let ()
      (define make-process-port
        (let ()
          (define kill
            (lambda (pid sig)
              (if (= sig 0)
                  -1
                  (system (format "kill -~s ~s" sig pid)))))
          (define make-handler
            (lambda (name ip op pid)
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
                  [file-position (p . pos)
                    (if (null? pos)
                        (most-negative-fixnum)
                        (errorf 'process-port "cannot reposition"))]
                  [flush-output-port (p) (flush-output-port op)]
                  [kill (p signal) (kill pid signal)]
                  [peek-char (p) (peek-char ip)]
                  [port-name (p) name]
                  [read-char (p) (read-char ip)]
                  [unread-char (c p) (unread-char c ip)]
                  [write-char (c p) (write-char c op)]
                  [else (errorf 'process-port "operation ~s not handled" msg)]))))
          (lambda (command)
            (let ([handler
                    (apply
                      make-handler
                      (format "process ~s" command)
                      (process command))])
              (make-input/output-port handler "" "")))))
      (define port-kill
        (lambda (p s) ((port-handler p) 'kill p s)))
      (and (let ()
             (define p (make-process-port (format "exec ~a" $cat_flush)))
             (and (not (char-ready? p))
                  (begin (fprintf p "hello!~%") (eq? (read p) 'hello!))
                  (char-ready? p)
                  (char=? (read-char p) #\newline)
                  (not (char-ready? p))
                  (begin (close-port p) #t)
                  ; sleep 1 may not be enough on a loaded system...
                  (begin (system "sleep 5") (= (port-kill p 0) -1))))
           (let ()
             (define p (make-process-port (format "exec ~a" $cat_flush)))
             (and (not (char-ready? p))
                  (begin (fprintf p "hello!~%") (eq? (read p) 'hello!))
                  (char-ready? p)
                  (char=? (read-char p) #\newline)
                  (not (char-ready? p))
                  (= (port-kill p 15) 0)
                  (let f () (if (char-ready? p) (eof-object? (read-char p)) (f)))
                  ; sleep 1 may not be enough on a loaded system...
                  (begin (system "sleep 1") (eqv? (port-kill p 0) -1))))))
  )
)

(if (windows?)
    (mat register-signal-handler
      (error? (errorf 'register-signal-handler
                "#<procedure list> is not a fixnum"))
      (error? (errorf 'register-signal-handler "14 is not a procedure"))
      (error? (errorf 'register-signal-handler
                "#<procedure list> is not a fixnum"))
    )
    (mat register-signal-handler
      (error? (register-signal-handler list 14))
      (error? (register-signal-handler 14 14))
      (error? (register-signal-handler list list))
      (let ((x '()))
        (register-signal-handler 14 (lambda (sig) (set! x (cons sig x))))
        ; guard the call to system, since openbsd gets an EINTR error,
        ; probably in system's call to waitpid, causing s_system to
        ; raise an exception
        (guard (c [#t (display-condition c) (printf "\nexception ignored\n")])
          (system "exec kill -14 $PPID")
          (system "exec kill -14 $PPID")
          (system "exec kill -14 $PPID")
          (system "exec kill -14 $PPID"))
        (let f ((n 1000000))
          (or (equal? x '(14 14 14 14))
              (and (not (= n 0))
                   (f (- n 1))))))
    )
)

(if (or (windows?) (equal? (getenv "USER") "root") (embedded?))
    (mat file-operations
      (error? (errorf 'delete-directory "failed for ~a: ~a" "testlink1" "not a directory"))
      (error? (errorf 'delete-directory "failed for ~a: ~a" "testlink2" "not a directory"))
      (error? (errorf 'delete-directory "failed for ~a: ~a" "testdir/testfile.ss" "not a directory"))
      (error? (errorf 'delete-file "failed for ~a: ~a" "testdir/w" "permission denied"))
      (error? (errorf 'get-mode "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-access-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-change-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-modification-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'get-mode "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-access-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-change-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
      (error? (errorf 'file-modification-time "failed for ~s: ~(~a~)" "testlink" "no such file or directory"))
    )
    (mat file-operations
      (boolean? (delete-file "testlink1" #f))
      (boolean? (delete-file "testlink2" #f))
      (not (file-exists? "testdir"))
      (begin
        (system "ln -s testdir testlink1")
        (and
          (not (file-exists? "testlink1"))
          (not (file-exists? "testlink1" #t))
          (file-exists? "testlink1" #f))
        (and
          (not (file-regular? "testlink1"))
          (not (file-regular? "testlink1" #t))
          (not (file-regular? "testlink1" #f)))
        (and
          (not (file-directory? "testlink1"))
          (not (file-directory? "testlink1" #t))
          (not (file-directory? "testlink1" #f)))
        (file-symbolic-link? "testlink1"))
      (begin
        (system "ln -s testdir/testfile.ss testlink2")
        (and
          (not (file-exists? "testlink2"))
          (not (file-exists? "testlink2" #t))
          (file-exists? "testlink2" #f))
        (and
          (not (file-regular? "testlink2"))
          (not (file-regular? "testlink2" #t))
          (not (file-regular? "testlink2" #f)))
        (and
          (not (file-directory? "testlink2"))
          (not (file-directory? "testlink2" #t))
          (not (file-directory? "testlink2" #f)))
        (file-symbolic-link? "testlink2"))
      (begin
        (mkdir "testdir")
        (and
          (file-exists? "testlink1")
          (file-exists? "testlink1" #t)
          (file-exists? "testlink1" #f))
        (and
          (not (file-regular? "testlink1"))
          (not (file-regular? "testlink1" #t))
          (not (file-regular? "testlink1" #f)))
        (and
          (file-directory? "testlink1")
          (file-directory? "testlink1" #t)
          (not (file-directory? "testlink1" #f)))
        (file-symbolic-link? "testlink1"))
      (begin
        (and
          (not (file-exists? "testlink2"))
          (not (file-exists? "testlink2" #t))
          (file-exists? "testlink2" #f))
        (and
          (not (file-regular? "testlink2"))
          (not (file-regular? "testlink2" #t))
          (not (file-regular? "testlink2" #f)))
        (and
          (not (file-directory? "testlink2"))
          (not (file-directory? "testlink2" #t))
          (not (file-directory? "testlink2" #f)))
        (file-symbolic-link? "testlink2"))
      (begin
        (with-output-to-file "testdir/testfile.ss" values 'replace)
        (and
          (file-exists? "testlink2")
          (file-exists? "testlink2" #t)
          (file-exists? "testlink2" #f))
        (and
          (file-regular? "testlink2")
          (file-regular? "testlink2" #t)
          (not (file-regular? "testlink2" #f)))
        (and
          (not (file-directory? "testlink2"))
          (not (file-directory? "testlink2" #t))
          (not (file-directory? "testlink2" #f)))
        (file-symbolic-link? "testlink2"))
      (delete-file "testlink1" #f)
      (delete-file "testlink2" #f)
      (begin
        (system "ln -s testdir testlink1")
        (and
          (file-exists? "testlink1")
          (file-exists? "testlink1" #t)
          (file-exists? "testlink1" #f))
        (and
          (not (file-regular? "testlink1"))
          (not (file-regular? "testlink1" #t))
          (not (file-regular? "testlink1" #f)))
        (and
          (file-directory? "testlink1")
          (file-directory? "testlink1" #t)
          (not (file-directory? "testlink1" #f)))
        (file-symbolic-link? "testlink1"))
      (begin
        (system "ln -s testdir/testfile.ss testlink2")
        (and
          (file-exists? "testlink2")
          (file-exists? "testlink2" #t)
          (file-exists? "testlink2" #f))
        (and
          (file-regular? "testlink2")
          (file-regular? "testlink2" #t)
          (not (file-regular? "testlink2" #f)))
        (and
          (not (file-directory? "testlink2"))
          (not (file-directory? "testlink2" #t))
          (not (file-directory? "testlink2" #f)))
        (file-symbolic-link? "testlink2"))
      (error? (delete-directory "testlink1" #t))
      (error? (delete-directory "testlink2" #t))
      (delete-file "testlink1" #f)
      (delete-file "testlink2" #f)
      (guard (c [(and (i/o-filename-error? c)
                      (equal? (i/o-error-filename c) "testdir"))])
        (delete-directory "testdir" #t))
      (error? (delete-directory "testdir/testfile.ss" #t))
      (delete-file "testdir/testfile.ss" #f)
      (guard (c [(and (i/o-filename-error? c)
                      (equal? (i/o-error-filename c) "testdir"))])
        (delete-file "testdir" #t))
      (eqv? (delete-directory "testdir" #t) (void))
      (begin
        (mkdir "testdir" #o700)
        #t)
      (begin
        (with-output-to-file "testdir/r" values)
        (with-output-to-file "testdir/w" values)
        (with-output-to-file "testdir/x" values)
        (with-output-to-file "testdir/rx" values)
        (with-output-to-file "testdir/rw" values)
        (chmod "testdir/r" #o400)
        (chmod "testdir/w" #o200)
        (chmod "testdir/x" #o100)
        (chmod "testdir/rx" #o500)
        (chmod "testdir/rw" #o600)
        #t)
      (eqv? (chmod "testdir" #o500) (void))
      (error? (delete-file "testdir/w" #t))
      (eqv? (chmod "testdir" #o700) (void))
      (guard (c [(and (i/o-filename-error? c)
                      (equal? (i/o-error-filename c) "testdir"))])
        (delete-directory "testdir" #t))
      (eqv? (delete-file "testdir/w" #t) (void))
      (eqv? (delete-file "testdir/rw" #t) (void))
      (delete-file "testdir/r" #f)
      (delete-file "testdir/x" #f)
      (delete-file "testdir/rx")
      (delete-directory "testdir" #f)
      (begin
        (system "echo one > testfile.ss")
        (system "ln -s testfile.ss testlink")
        #t)
      (time=? (file-access-time "testlink") (file-access-time "testfile.ss"))
      (time=? (file-change-time "testlink") (file-change-time "testfile.ss"))
      (time=? (file-modification-time "testlink") (file-modification-time "testfile.ss"))
      ; no guarantee what times are returned for symbolic links.
      ; just make sure they return time objects
      (andmap time?
        (map (lambda (p) (p "testlink" #f))
          (list file-access-time file-change-time file-modification-time)))
      (= (get-mode "testlink") (get-mode "testfile.ss"))
      (begin
        (define $taccess (file-access-time "testfile.ss"))
        (define $tmodification (file-modification-time "testfile.ss"))
        (define $tchange (file-change-time "testfile.ss"))
        #t)
      (eq? (sleep (make-time 'time-duration 0 2)) (void))
      (symbol? (with-input-from-file "testfile.ss" read))
      ; following should be time<?, but access times are not updated on some
      ; file systems, particularly nfs file systems.  but we wouldn't expect
      ; time to run backwards (except for one hour for DST)
      (time<=? $taccess (file-access-time "testfile.ss"))
      (begin
        (system "echo two > testfile.ss")
        #t)
      ; for whatever reason, there seems to be no guarantee about this either ...
      (time<=? $tmodification (file-modification-time "testfile.ss"))
      (or (begin
            (chmod "testfile.ss" #o770)
            (not (= (get-mode "testlink" #f) (get-mode "testfile.ss"))))
          (begin
            (chmod "testfile.ss" #o777)
            (not (= (get-mode "testlink" #f) (get-mode "testfile.ss")))))
      ; ... or this
      (time>=? (file-change-time "testfile.ss") $tchange)
      (delete-file "testfile.ss" #f)
      (andmap time?
        (map (lambda (p) (p "testlink" #f))
          (list file-access-time file-change-time file-modification-time)))
      (error? (get-mode "testlink"))
      (error? (file-access-time "testlink"))
      (error? (file-change-time "testlink"))
      (error? (file-modification-time "testlink"))
      (error? (get-mode "testlink" #t))
      (error? (file-access-time "testlink" #t))
      (error? (file-change-time "testlink" #t))
      (error? (file-modification-time "testlink" #t))
      (delete-file "testlink" #f)
    )
)

(if (windows?)
    (mat nonblocking
      ; verify no windows nonblocking support for binary file ports
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush)])
        (dynamic-wind
          void
          (lambda ()
            (and (not (port-has-port-nonblocking?? to-stdin))
                 (not (port-has-set-port-nonblocking!? to-stdin))
                 (not (port-has-port-nonblocking?? from-stdout))
                 (not (port-has-set-port-nonblocking!? from-stdout))
                 (not (port-has-port-nonblocking?? from-stderr))
                 (not (port-has-set-port-nonblocking!? from-stderr))))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; verify no windows nonblocking support for textual file ports
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush (buffer-mode block) (native-transcoder))])
        (dynamic-wind
          void
          (lambda ()
            (and (not (port-has-port-nonblocking?? to-stdin))
                 (not (port-has-set-port-nonblocking!? to-stdin))
                 (not (port-has-port-nonblocking?? from-stdout))
                 (not (port-has-set-port-nonblocking!? from-stdout))
                 (not (port-has-port-nonblocking?? from-stderr))
                 (not (port-has-set-port-nonblocking!? from-stderr))))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
    )
    (mat nonblocking ; see also io.ms (mat open-process-ports ...)
      ; test get-bytevector-some on nonblocking binary input port
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush)])
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
              (when (input-port-ready? from-stderr)
                (errorf #f "input ready on from-stderr ~s" (get-string-some from-stderr)))
              (if (input-port-ready? from-stdout)
                  (let ([s (get-string-n from-stdout 10)])
                    (unless (equal? s "life in th")
                      (errorf #f "unexpected from-stdout string ~s" s)))
                  (begin
                    (display ".")
                    (flush-output-port)
                    (f))))
            (set-port-nonblocking! from-stdout #t)
            (let f ([all ""])
              (let ([s (get-string-some from-stdout)])
                (when (eof-object? s) (errorf #f "unexpected from-stdout eof"))
                (if (equal? s "")
                    (unless (equal? all "e fast lane\n")
                      (display ".")
                      (flush-output-port)
                      (f all))
                    (f (string-append all s)))))
            (and
              (equal? (get-string-some from-stdout) "")
              (not (input-port-ready? from-stdout))
              (equal? (get-string-some from-stdout) "")
              (begin
                (close-port to-stdin)
                (let f ()
                  (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                    (display ".")
                    (flush-output-port)
                    (f)))
                #t)))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; test get-string-some on nonblocking textual input port
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush (buffer-mode block) (native-transcoder))])
        (dynamic-wind
          void
          (lambda ()
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
                    (display ".")
                    (flush-output-port)
                    (f))))
            (set-port-nonblocking! from-stdout #t)
            (let f ([all ""])
              (let ([s (get-string-some from-stdout)])
                (when (eof-object? s) (errorf #f "unexpected from-stdout eof"))
                (if (equal? s "")
                    (unless (equal? all "e fast lane\n")
                      (display ".")
                      (flush-output-port)
                      (f all))
                    (f (string-append all s)))))
            (and
              (equal? (get-string-some from-stdout) "")
              (not (input-port-ready? from-stdout))
              (equal? (get-string-some from-stdout) "")
              (begin
                (close-port to-stdin)
                (let f ()
                  (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                    (display ".")
                    (flush-output-port)
                    (f)))
                #t)))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; test get-bytevector-some! on nonblocking binary input port
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush)])
        (define get-bytevector-some
          (lambda (bp)
            (let ([buf (make-bytevector 5)])
              (let ([n (get-bytevector-some! bp buf 0 (bytevector-length buf))])
                (if (eof-object? n)
                    n
                    (bytevector-truncate! buf n))))))
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
            (set-port-nonblocking! to-stdin #t)  ; not testing whether this does anything
            (set-port-nonblocking! from-stdout #t)
            (set-port-nonblocking! from-stderr #t)
            (put-string to-stdin "that don't impress me much\n")
            (flush-output-port to-stdin)
            (let f ([all ""])
              (unless (equal? all "that don't impress me much\n")
                (let ([s (get-string-some from-stderr)])
                  (when (eof-object? s) (errorf #f "unexpected from-stderr eof"))
                  (unless (equal? s "") (errorf #f "unexpected from-stderr input ~s" s)))
                (let ([s (get-string-some from-stdout)])
                  (when (eof-object? s) (errorf #f "unexpected from-stdout eof"))
                  (if (equal? s "")
                      (begin
                        (display ".")
                        (flush-output-port)
                        (f all))
                      (f (string-append all s))))))
            (and
              (equal? (get-string-some from-stdout) "")
              (not (input-port-ready? from-stdout))
              (equal? (get-string-some from-stdout) "")
              (begin
                (close-port to-stdin)
                (let f ()
                  (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                    (display ".")
                    (flush-output-port)
                    (f)))
                #t)))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; test get-string-some! on nonblocking textual input port
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush (buffer-mode block) (native-transcoder))])
        (define get-string-some
          (lambda (tp)
            (let ([buf (make-string 5)])
              (let ([n (get-string-some! tp buf 0 (string-length buf))])
                (if (eof-object? n)
                    n
                    (substring buf 0 n))))))
        (dynamic-wind
          void
          (lambda ()
            (set-port-nonblocking! to-stdin #t)  ; not testing whether this does anything
            (set-port-nonblocking! from-stdout #t)
            (set-port-nonblocking! from-stderr #t)
            (put-string to-stdin "that don't impress me much\n")
            (flush-output-port to-stdin)
            (let f ([all ""])
              (unless (equal? all "that don't impress me much\n")
                (let ([s (get-string-some from-stderr)])
                  (when (eof-object? s) (errorf #f "unexpected from-stderr eof"))
                  (unless (equal? s "") (errorf #f "unexpected from-stderr input ~s" s)))
                (let ([s (get-string-some from-stdout)])
                  (when (eof-object? s) (errorf #f "unexpected from-stdout eof"))
                  (if (equal? s "")
                      (begin
                        (display ".")
                        (flush-output-port)
                        (f all))
                      (f (string-append all s))))))
            (and
              (equal? (get-string-some from-stdout) "")
              (not (input-port-ready? from-stdout))
              (equal? (get-string-some from-stdout) "")
              (begin
                (close-port to-stdin)
                (let f ()
                  (unless (and (port-eof? from-stdout) (port-eof? from-stderr))
                    (display ".")
                    (flush-output-port)
                    (f)))
                #t)))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; test put-bytevector-some on nonblocking binary output port,
      ; counting on O/S to limit amount we can write to a pipe that
      ; no one has yet read from
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush (buffer-mode none))])
        (define put-string-some
          (lambda (bp s)
            (put-bytevector-some bp (string->utf8 s) 0 (string-length s))))
        (define get-string-some
          (lambda (bp)
            (let ([x (get-bytevector-some bp)])
              (if (eof-object? x) x (utf8->string x)))))
        (dynamic-wind
          void
          (lambda ()
            (define s "my future lies beyond the yellow brick road")
            (set-port-nonblocking! to-stdin #t)
            (set-port-nonblocking! from-stdout #t)
            (set-port-nonblocking! from-stderr #t)
            (let ([len (string-length s)])
              (let f ([n 0])
                (let ([i (put-string-some to-stdin s)])
                  (if (= i len)
                      (f (+ n 1))
                      (let f ()
                        (if (string=? (get-string-some from-stdout) "")
                            (or (= (put-string-some to-stdin "\n") 1)
                                (begin
                                  (display ".")
                                  (flush-output-port)
                                  (f)))
                            (f))))))))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
      ; test put-string-some on nonblocking textual output port,
      ; counting on O/S to limit amount we can write to a pipe that
      ; no one has yet read from
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports $cat_flush (buffer-mode none) (native-transcoder))])
        (dynamic-wind
          void
          (lambda ()
            (define s "my future lies beyond the yellow brick road")
            (set-port-nonblocking! to-stdin #t)
            (set-port-nonblocking! from-stdout #t)
            (set-port-nonblocking! from-stderr #t)
            (let ([len (string-length s)])
              (let f ([n 0])
                (let ([i (put-string-some to-stdin s)])
                  (if (= i len)
                      (f (+ n 1))
                      (let f ()
                        (if (string=? (get-string-some from-stdout) "")
                            (or (= (put-string-some to-stdin "\n") 1)
                                (begin
                                  (display ".")
                                  (flush-output-port)
                                  (f)))
                            (f))))))))
          (lambda ()
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))
    )
)
