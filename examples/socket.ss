;;; socket.ss
;;; R. Kent Dybvig May 1998
;;; Updated November 2005
;;; Public Domain
;;;
;;; bindings for socket operations and other items useful for writing
;;; programs that use sockets.

;;; Requires csocket.so, built from csocket.c.
(load-shared-object "./csocket.so")

;;; Requires from C library:
;;;   close, dup, execl, fork, kill, listen, tmpnam, unlink
(case (machine-type)
  [(i3le ti3le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx) (load-shared-object "libc.dylib")]
  [else (load-shared-object "libc.so")])

;;; basic C-library stuff

(define close
  (foreign-procedure "close" (integer-32)
    integer-32))

(define dup
  (foreign-procedure "dup" (integer-32)
    integer-32))

(define execl4
  (let ((execl-help
         (foreign-procedure "execl"
           (string string string string integer-32)
           integer-32)))
    (lambda (s1 s2 s3 s4)
      (execl-help s1 s2 s3 s4 0))))

(define fork
  (foreign-procedure "fork" ()
    integer-32))

(define kill
  (foreign-procedure "kill" (integer-32 integer-32)
    integer-32))

(define listen
  (foreign-procedure "listen" (integer-32 integer-32)
    integer-32))

(define tmpnam
  (foreign-procedure "tmpnam" (integer-32)
    string))

(define unlink
  (foreign-procedure "unlink" (string)
    integer-32))

;;; routines defined in csocket.c

(define accept
  (foreign-procedure "do_accept" (integer-32)
    integer-32))

(define bytes-ready?
  (foreign-procedure "bytes_ready" (integer-32)
    boolean))

(define bind
  (foreign-procedure "do_bind" (integer-32 string)
    integer-32))

(define c-error
  (foreign-procedure "get_error" ()
    string))

(define c-read
  (foreign-procedure "c_read" (integer-32 string integer-32)
    integer-32))

(define c-write
  (foreign-procedure "c_write" (integer-32 string integer-32)
    integer-32))

(define connect
  (foreign-procedure "do_connect" (integer-32 string)
    integer-32))

(define socket
  (foreign-procedure "do_socket" ()
    integer-32))

;;; higher-level routines

(define dodup
 ; (dodup old new) closes old and dups new, then checks to
 ; make sure that resulting fd is the same as old
  (lambda (old new)
    (check 'close (close old))
    (unless (= (dup new) old)
      (error 'dodup
        "couldn't set up child process io for fd ~s" old))))

(define dofork
 ; (dofork child parent) forks a child process and invokes child
 ; without arguments and parent with the child's pid
  (lambda (child parent)
    (let ([pid (fork)])
      (cond
        [(= pid 0) (child)]
        [(> pid 0) (parent pid)]
        [else (error 'fork (c-error))]))))

(define setup-server-socket
 ; create a socket, bind it to name, and listen for connections
  (lambda (name)
    (let ([sock (check 'socket (socket))])
      (unlink name)
      (check 'bind (bind sock name))
      (check 'listen (listen sock 1))
      sock)))

(define setup-client-socket
 ; create a socket and attempt to connect to server
  (lambda (name)
    (let ([sock (check 'socket (socket))])
      (check 'connect (connect sock name))
      sock)))

(define accept-socket
 ; accept a connection
  (lambda (sock)
    (check 'accept (accept sock))))

(define check
 ; signal an error if status x is negative, using c-error to
 ; obtain the operating-system's error message
  (lambda (who x)
    (if (< x 0)
        (error who (c-error))
        x)))

(define terminate-process
 ; kill the process identified by pid
  (lambda (pid)
    (define sigterm 15)
    (kill pid sigterm)
    (void)))

(define open-process
  (lambda (command)
    (define handler
      (lambda (pid socket)
        (define (flush-output who p)
          (let ([i (port-output-index p)])
            (when (fx> i 0)
              (check who (c-write socket (port-output-buffer p) i))
              (set-port-output-index! p 0))))
        (lambda (msg . args)
          (record-case (cons msg args)
            [block-read (p str cnt)
             (critical-section
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
                     (begin
                       (flush-output 'block-read p)
                       (let ([n (check 'block-read (c-read socket str cnt))])
                         (if (fx= n 0)
                             #!eof
                             n))))))]
            [char-ready? (p)
             (or (< (port-input-index p) (port-input-size p))
                 (bytes-ready? socket))]
            [clear-input-port (p)
             ; set size to zero rather than index to size
             ; in order to invalidate unread-char
             (set-port-input-size! p 0)]
            [clear-output-port (p) (set-port-output-index! p 0)]
            [close-port (p)
             (critical-section
               (flush-output 'close-port p)
               (set-port-output-size! p 0)
               (set-port-input-size! p 0)
               (mark-port-closed! p)
               (terminate-process pid))]
            [file-length (p) 0]
            [file-position (p . pos)
             (if (null? pos)
                 (most-negative-fixnum)
                 (error 'process-port "cannot reposition"))]
            [flush-output-port (p)
             (critical-section
               (flush-output 'flush-output-port p))]
            [peek-char (p)
             (critical-section
               (let ([b (port-input-buffer p)]
                     [i (port-input-index p)]
                     [s (port-input-size p)])
                 (if (fx< i s)
                     (string-ref b i)
                     (begin
                       (flush-output 'peek-char p)
                       (let ([s (check 'peek-char (c-read socket b (string-length b)))])
                         (if (fx= s 0)
                             #!eof
                             (begin (set-port-input-size! p s)
                                    (string-ref b 0))))))))]
            [port-name (p) "process"]
            [read-char (p)
             (critical-section
               (let ([b (port-input-buffer p)]
                     [i (port-input-index p)]
                     [s (port-input-size p)])
                 (if (fx< i s)
                     (begin
                       (set-port-input-index! p (fx+ i 1))
                       (string-ref b i))
                     (begin
                       (flush-output 'peek-char p)
                       (let ([s (check 'read-char (c-read socket b (string-length b)))])
                         (if (fx= s 0)
                             #!eof
                             (begin (set-port-input-size! p s)
                                    (set-port-input-index! p 1)
                                    (string-ref b 0))))))))]
            [unread-char (c p)
             (critical-section
               (let ([b (port-input-buffer p)]
                     [i (port-input-index p)]
                     [s (port-input-size p)])
                 (when (fx= i 0)
                   (error 'unread-char
                          "tried to unread too far on ~s"
                          p))
                 (set-port-input-index! p (fx- i 1))
                ; following could be skipped; supposed to be
                ; same character
                 (string-set! b (fx- i 1) c)))]
            [write-char (c p)
             (critical-section
               (let ([b (port-output-buffer p)]
                     [i (port-output-index p)]
                     [s (port-output-size p)])
                 (string-set! b i c)
                 (check 'write-char (c-write socket b (fx+ i 1)))
                 (set-port-output-index! p 0)))]
            [block-write (p str cnt)
             (critical-section
              ; flush buffered data
               (flush-output 'block-write p)
              ; write new data
               (check 'block-write (c-write socket str cnt)))]
            [else
             (error 'process-port "operation ~s not handled" msg)]))))
    (let* ([server-socket-name (tmpnam 0)]
           [server-socket (setup-server-socket server-socket-name)])
      (dofork 
        (lambda () ; child
          (check 'close (close server-socket))
          (let ([sock (setup-client-socket server-socket-name)])
            (dodup 0 sock)
            (dodup 1 sock))
          (check 'execl (execl4 "/bin/sh" "/bin/sh" "-c" command))
          (error 'open-process "subprocess exec failed"))
        (lambda (pid) ; parent
          (let ([sock (accept-socket server-socket)])
            (check 'close (close server-socket))
            (let ([ib (make-string 1024)] [ob (make-string 1024)])
              (let ([p (make-input/output-port
                         (handler pid sock)
                         ib ob)])
                (set-port-input-size! p 0)
                (set-port-output-size! p (fx- (string-length ob) 1))
                p))))))))

#!eof

;;; sample session using base socket functionality

> (define client-pid)
> (define client-socket)
> (let* ([server-socket-name (tmpnam 0)]
         [server-socket (setup-server-socket server-socket-name)])
   ; fork a child, use it to exec a client Scheme process, and set
   ; up server-side client-pid and client-socket variables.
    (dofork   ; child
      (lambda () 
       ; the child establishes the socket input/output fds as
       ; stdin and stdout, then starts a new Scheme session
        (check 'close (close server-socket))
        (let ([sock (setup-client-socket server-socket-name)])
          (dodup 0 sock)
          (dodup 1 sock))
        (check 'execl (execl4 "/bin/sh" "/bin/sh" "-c" "exec scheme"))
        (error 'client "returned!"))
      (lambda (pid) ; parent
       ; the parent waits for a connection from the client
        (set! client-pid pid)
        (set! client-socket (accept-socket server-socket))
        (check 'close (close server-socket)))))
> (define put ; procedure to send data to client
    (lambda (x)
      (let ([s (format "~s~%" x)])
        (c-write client-socket s (string-length s)))
      (void)))
> (define get ; procedure to read data from client
    (let ([buff (make-string 1024)])
      (lambda ()
        (let ([n (c-read client-socket buff (string-length buff))])
          (printf "client:~%~a~%server:~%" (substring buff 0 n))))))
> (get)
client:
Chez Scheme Version 7.0
Copyright (c) 1985-2005 Cadence Research Systems

>
server:
> (put '(let ((x 3)) x))
> (get)
client:
3
>
server:
> (terminate-process client-pid)
> (exit)


;;; sample session using process port

> (define p (open-process "exec scheme -q"))
> (define s (make-string 1000 #\nul))
> (pretty-print '(+ 3 4) p)
> (read p)
7
> (pretty-print '(define (f x) (if (= x 0) 1 (* x (f (- x 1))))) p)
> (pretty-print '(f 10) p)
> (read p)
3628800
> (pretty-print '(exit) p)
> (read p)
#!eof
> (close-port p)
