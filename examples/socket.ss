;;; socket.ss
;;; R. Kent Dybvig May 1998
;;; Updated November 2005
;;; Updated by Jamie Taylor, Sept 2016
;;; Public Domain
;;;
;;; bindings for socket operations and other items useful for writing
;;; programs that use sockets.

;;; Requires csocket.so, built from csocket.c.
;;; Example compilation command line from macOS:
;;;  cc -c csocket.c -o csocket.o
;;;  cc csocket.o -dynamic -dynamiclib -current_version 1.0 -compatibility_version 1.0 -o csocket.so
(load-shared-object "./csocket.so")

;;; Requires from C library:
;;;   close, dup, execl, fork, kill, listen, tmpnam, unlink
(case (machine-type)
  [(i3le ti3le a6le ta6le)
   (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx arm64osx tarm64osx
          a6ios ta6ios arm64ios tarm64ios)
   (load-shared-object "libc.dylib")]
  [else
   (load-shared-object "libc.so")])

;;; basic C-library stuff

(define close
  (foreign-procedure "close" (int)
    int))

(define dup
  (foreign-procedure "dup" (int)
    int))

(define execl4
  (let ((execl-help
         (foreign-procedure "execl"
           (string string string string void*)
           int)))
    (lambda (s1 s2 s3 s4)
      (execl-help s1 s2 s3 s4 0))))

(define fork
  (foreign-procedure "fork" ()
    int))

(define kill
  (foreign-procedure "kill" (int int)
    int))

(define listen
  (foreign-procedure "listen" (int int)
    int))

(define tmpnam
  (foreign-procedure "tmpnam" (void*)
    string))

(define unlink
  (foreign-procedure "unlink" (string)
    int))

;;; routines defined in csocket.c

(define accept
  (foreign-procedure "do_accept" (int)
    int))

(define bytes-ready?
  (foreign-procedure "bytes_ready" (int)
    boolean))

(define bind
  (foreign-procedure "do_bind" (int string)
    int))

(define c-error
  (foreign-procedure "get_error" ()
    string))

(define c-read
  (foreign-procedure "c_read" (int u8* size_t size_t)
    ssize_t))

(define c-write
  (foreign-procedure "c_write" (int u8* size_t ssize_t)
    ssize_t))

(define connect
  (foreign-procedure "do_connect" (int string)
    int))

(define socket
  (foreign-procedure "do_socket" ()
    int))

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
    (define (make-r! socket)
      (lambda (bv start n)
        (check 'r! (c-read socket bv start n))))
    (define (make-w! socket)
      (lambda (bv start n)
        (check 'w! (c-write socket bv start n))))
    (define (make-close pid socket)
      (lambda ()
        (check 'close (close socket))
        (terminate-process pid)))
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
            (make-custom-binary-input/output-port command
              (make-r! sock) (make-w! sock) #f #f (make-close pid sock))))))))

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
      (let* ([s (format "~s~%" x)]
             [bv (string->utf8 s)])
        (c-write client-socket bv 0 (bytevector-length bv)))
      (void)))
> (define get ; procedure to read data from client
    (let ([buff (make-bytevector 1024)])
      (lambda ()
        (let* ([n (c-read client-socket buff 0 (bytevector-length buff))]
               [bv (make-bytevector n)])
          (bytevector-copy! buff 0 bv 0 n)
          (printf "client:~%~a~%server:~%" (utf8->string bv))))))
> (get)
client:
Chez Scheme Version 9.5.1
Copyright 1984-2017 Cisco Systems, Inc.

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

> (define p (transcoded-port (open-process "exec scheme -q") (native-transcoder)))
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
