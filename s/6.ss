;;; 6.ss
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

(begin
(define with-output-to-string
  (lambda (th)
    (unless (procedure? th)
      ($oops 'with-output-to-string "~s is not a procedure" th))
    (parameterize ([current-output-port (open-output-string)])
      (th)
      (get-output-string (current-output-port)))))

(define with-input-from-string
  (lambda (s th)
    (unless (string? s)
      ($oops 'with-input-from-string "~s is not a string" s))
    (unless (procedure? th)
      ($oops 'with-input-from-string "~s is not a procedure" th))
    (let ([p (open-input-string s)])
      (call-with-values
        (lambda () (parameterize ([current-input-port p]) (th)))
        (lambda v (apply values v))))))

(let ()
  (define getwd
    (if (foreign-entry? "(cs)s_getwd")
        (foreign-procedure "(cs)s_getwd" () string)
        (lambda ()
          (let ([p (process "exec /bin/pwd")])
            (let ([ip (car p)] [op (open-output-string)])
              (let f ()
                (let ([c (read-char ip)])
                  (if (or (eof-object? c) (char=? c #\newline))
                      (begin
                        (close-output-port (cadr p))
                        (close-input-port ip)
                        (get-output-string op))
                      (begin (write-char c op) (f))))))))))
  (define chdir
    (foreign-procedure "(cs)s_chdir"
      (string)
      integer-32))
  (define $cd
    (case-lambda
      [(who) (or (getwd) ($oops who "cannot determine current directory"))]
      [(dir who)
       (unless (string? dir) ($oops who "~s is not a string" dir))
       (unless (= (chdir dir) 0)
         ($oops who "cannot set current directory to ~s" dir))]))
  (set-who! current-directory
    (case-lambda
      [() ($cd who)]
      [(dir) ($cd dir who)]))
  (set-who! cd
    (case-lambda
      [() ($cd who)]
      [(dir) ($cd dir who)])))

(let ()
  (define who 'mkdir)

  (define fp (foreign-procedure "(cs)mkdir" (string uptr) ptr))

  (define (do-mkdir path mode)
    (unless (string? path) ($oops who "~s is not a string" path))
    (unless (fixnum? mode) ($oops who "~s is not a fixnum" mode))
    (let ([x (fp path mode)])
      (cond
        [(eqv? x #t) (void)]
        [(string? x)
         ($oops/c who
           (make-i/o-filename-error path)
           "cannot create ~s: ~(~a~)" path x)]
        [else
         ($oops/c who
           (make-i/o-filename-error path)
           "cannot create ~s" path)])))

  (set! mkdir
    (case-lambda
      [(path) (do-mkdir path #o777)]
      [(path mode) (do-mkdir path mode)])))

(define-who chmod
  (let ([fp (foreign-procedure "(cs)chmod" (string fixnum) ptr)])
    (lambda (path mode)
      (unless (string? path) ($oops who "~s is not a string" path))
      (unless (fixnum? mode) ($oops who "~s is not a fixnum" mode))
      (let ([x (fp path mode)])
        (cond
          [(eqv? x #t) (void)]
          [(string? x)
           ($oops/c who
             (make-i/o-filename-error path)
             "cannot modify ~s: ~(~a~)" path x)]
          [else
           ($oops/c who
             (make-i/o-filename-error path)
             "cannot modify ~s" path)])))))

(define-who get-mode
  (let ([fp (foreign-procedure "(cs)getmod" (string boolean) ptr)])
    (rec get-mode
      (case-lambda
        [(path) (get-mode path #t)]
        [(path follow?)
         (define (err x)
           (if (string? x)
               ($oops/c who
                 (make-i/o-filename-error path)
                 "failed for ~s: ~(~a~)" path x)
               ($oops/c who
                 (make-i/o-filename-error path)
                 "failed for ~s" path)))
         (unless (string? path) ($oops who "~s is not a string" path))
         (let ([x (fp path follow?)])
           (if (fixnum? x)
               x
               (err x)))]))))

(let ()
  (define file-x-time
    (lambda (who path-fp fd-fp file follow?)
      (define (path-err path x)
        (if (string? x)
            ($oops/c who
              (make-i/o-filename-error path)
              "failed for ~s: ~(~a~)" path x)
            ($oops/c who
              (make-i/o-filename-error path)
              "failed for ~s" path)))
      (unless (or (string? file) (and (port? file) (file-port? file)))
        ($oops who "~s is not a string or file port" file))
      (if (string? file)
          (let ([x (path-fp file follow?)])
            (if (pair? x)
                (make-time 'time-utc (cdr x) (car x))
                (path-err file x)))
          (let ([x (fd-fp (port-file-descriptor file))])
            (cond
              [(pair? x) (make-time 'time-utc (cdr x) (car x))]
              [(string? x) ($oops who "failed for ~s: ~(~a~)" file x)]
              [else ($oops who "failed for ~s" file)])))))

  (define-syntax define-file-x-time
    (syntax-rules ()
      [(_ name path-name fd-name)
       (set-who! name
         (let ([path-fp (foreign-procedure path-name (string boolean) ptr)]
               [fd-fp (foreign-procedure fd-name (fixnum) ptr)])
           (case-lambda
             [(file) (file-x-time who path-fp fd-fp file #t)]
             [(file follow?) (file-x-time who path-fp fd-fp file follow?)])))]))

  (define-file-x-time file-access-time "(cs)path_atime" "(cs)fd_atime")
  (define-file-x-time file-change-time "(cs)path_ctime" "(cs)fd_atime")
  (define-file-x-time file-modification-time "(cs)path_mtime" "(cs)fd_mtime"))

(define directory-separator
  (lambda ()
    (#2%directory-separator)))

(define directory-separator?
  (lambda (c)
    (unless (char? c)
      ($oops 'directory-separator? "~s is not a character" c))
    (#3%directory-separator? c)))

(define-who directory-list
  (let ([dl (if-feature windows
              (let ([wl (foreign-procedure "(cs)find_files" (string) scheme-object)])
                (lambda (path)
                  (let ([n (string-length path)])
                    (unless (and (fx> n 0)
                                 (let nostars? ([i 0])
                                   (or (fx= i n)
                                       (and (not (char=? (string-ref path i) #\*))
                                            (nostars? (fx+ i 1))))))
                      ($oops who "invalid directory name ~s" path))
                    (wl (if (memv (string-ref path (fx- n 1)) '(#\\ #\/ #\:))
                            (string-append path "*")
                            (string-append path "\\*"))))))
              (foreign-procedure "(cs)directory_list" (string) scheme-object))])
    (lambda (path)
      (unless (string? path) ($oops who "~s is not a string" path))
      (let ([bv* (dl path)])
        (if (string? bv*)
            ($oops/c who
              (make-i/o-filename-error path)
              "failed for ~a: ~(~a~)" path bv*)
            (remp (lambda (s)
                    (let ([n (string-length s)])
                      (or (and (fx= n 1) (char=? (string-ref s 0) #\.))
                          (and (fx= n 2)
                               (char=? (string-ref s 0) #\.)
                               (char=? (string-ref s 1) #\.)))))
              (map (if-feature windows
                     (lambda (bv) (utf16->string bv 'little #t))
                     utf8->string)
                   bv*)))))))

(define-who file-exists?
  (let ([fp (foreign-procedure "(cs)file_existsp" (string boolean) boolean)])
    (rec file-exists?
      (case-lambda
        [(path) (file-exists? path #t)]
        [(path follow?)
         (unless (string? path) ($oops who "~s is not a string" path))
         (fp path follow?)]))))

(define-who #(r6rs: file-exists?)
  (lambda (path)
    (#2%file-exists? path #t)))

(define-who file-regular?
  (let ([fp (foreign-procedure "(cs)file_regularp" (string boolean) boolean)])
    (rec file-regular?
      (case-lambda
        [(path) (file-regular? path #t)]
        [(path follow?)
         (unless (string? path) ($oops who "~s is not a string" path))
         (fp path follow?)]))))

(define-who file-directory?
  (let ([fp (foreign-procedure "(cs)file_directoryp" (string boolean) boolean)])
    (rec file-directory?
      (case-lambda
        [(path) (file-directory? path #t)]
        [(path follow?)
         (unless (string? path) ($oops who "~s is not a string" path))
         (fp path follow?)]))))

(define-who file-symbolic-link?
  (let ([fp (foreign-procedure "(cs)file_symbolic_linkp" (string) boolean)])
    (lambda (path)
      (unless (string? path) ($oops who "~s is not a string" path))
      (fp path))))

(let ()
  (define fp-delete-file
    (foreign-procedure "(cs)delete_file"
      (string)
      scheme-object))

  (define fp-delete-directory
    (foreign-procedure "(cs)delete_directory"
      (string)
      scheme-object))

  (define (do-delete who fp path error?)
    (unless (string? path)
      ($oops who "~s is not a string" path))
    (let ([x (fp path)])
      (if error?
          (cond
            [(eqv? x #t) (void)]
            [(string? x)
             ($oops/c who
               (make-i/o-filename-error path)
               "failed for ~a: ~(~a~)" path x)]
            [else
             ($oops/c who
               (make-i/o-filename-error path)
               "failed for ~a" path)])
          (eq? x #t))))

  (set-who! delete-file
    (case-lambda
      [(path) (do-delete who fp-delete-file path #f)]
      [(path error?) (do-delete who fp-delete-file path error?)]))

  (set-who! #(r6rs: delete-file) ; implicit rec
    (lambda (path)
      (do-delete who fp-delete-file path #t)))

  (set-who! delete-directory
    (case-lambda
      [(path) (do-delete who fp-delete-directory path #f)]
      [(path error?) (do-delete who fp-delete-directory path error?)])))

(let ()
  (define fp (foreign-procedure "(cs)rename_file" (string string) ptr))

  (set-who! rename-file
    (lambda (path1 path2)
      (unless (string? path1)
        ($oops who "~s is not a string" path1))
      (unless (string? path2)
        ($oops who "~s is not a string" path2))
      (let ([x (fp path1 path2)])
        (cond
          [(eqv? x #t) (void)]
          [(string? x)
           ($oops/c who
             (condition
               (make-i/o-filename-error path1)
               (make-i/o-filename-error path2))
             "cannot rename ~s to ~s: ~(~a~)" path1 path2 x)]
          [else
           ($oops/c who
             (condition
               (make-i/o-filename-error path1)
               (make-i/o-filename-error path2))
             "cannot rename ~s to ~s" path1 path2)])))))

;;; path procedures

(let ()
  (define windows? (if-feature windows #t #f))

  (define directory-separator-predicate
    (lambda (s)
      (if (and windows?
               (string? s)
               (let ([n (string-length s)])
                 (and (fx>= n 4)
                      (char=? (string-ref s 0) #\\)
                      (char=? (string-ref s 1) #\\)
                      (char=? (string-ref s 2) #\?)
                      (char=? (string-ref s 3) #\\))))
          (lambda (c) (char=? c #\\))
          directory-separator?)))

  (define path-base
    (lambda (s n)
      (cond
        [(and windows?
              (fx>= n 2)
              (char=? (string-ref s 1) #\:)
              (let ([c (string-ref s 0)])
                (or (char<=? #\a c #\z) (char<=? #\A c #\Z))))
         (if (and (fx>= n 3) (directory-separator? (string-ref s 2))) 3 2)]
        [(and windows?
              (fx>= n 4)
              (char=? (string-ref s 0) #\\)
              (char=? (string-ref s 1) #\\)
              (char=? (string-ref s 2) #\?)
              (char=? (string-ref s 3) #\\))
         (cond
           [(and (fx>= n 6)
                 (char=? (string-ref s 5) #\:)
                 (let ([c (string-ref s 4)])
                   (or (char<=? #\a c #\z) (char<=? #\A c #\Z))))
            (if (and (fx>= n 7) (char=? (string-ref s 6) #\\)) 7 6)]
           [(and windows?
                 (fx>= n 8)
                 (char-ci=? (string-ref s 4) #\U)
                 (char-ci=? (string-ref s 5) #\N)
                 (char-ci=? (string-ref s 6) #\C)
                 (char=? (string-ref s 7) #\\))
            (let loop ([i (if (and (fx>= n 9) (char=? (string-ref s 8) #\\)) 9 8)])
              (if (or (fx= i n) (char=? (string-ref s i) #\\))
                  i
                  (loop (fx+ i 1))))]
           [else 4])]
        [(and windows?
              (fx>= n 2)
              (directory-separator? (string-ref s 0))
              (directory-separator? (string-ref s 1)))
         (let loop ([i 2])
           (if (or (fx= i n) (directory-separator? (string-ref s i)))
               i
               (loop (fx+ i 1))))]
        [(and (fx>= n 1) (directory-separator? (string-ref s 0))) 1]
        [(and (fx>= n 1) (char=? (string-ref s 0) #\.))
         (if (or (fx= n 1) (directory-separator? (string-ref s 1)))
             1
             (if (and (char=? (string-ref s 1) #\.)
                      (or (fx= n 2) (directory-separator? (string-ref s 2))))
                 2
                 0))]
        [(and (fx>= n 1) (char=? (string-ref s 0) #\~))
         (if (or (fx= n 1) (directory-separator? (string-ref s 1)))
             1
             (let loop ([i 2])
               (if (or (fx= i n) (directory-separator? (string-ref s i)))
                   i
                   (loop (fx+ i 1)))))]
        [else 0])))

  (set-who! path-absolute?
    (lambda (s)
      (unless (string? s) ($oops who "~s is not a string" s))
      (let ([n (string-length s)])
        (or (and (fx>= n 1) (directory-separator? (string-ref s 0)))
            (and (fx>= n 1) (char=? (string-ref s 0) #\~))
            (and windows?
                 (fx>= n 3)
                 (char=? (string-ref s 1) #\:)
                 (let ([c (string-ref s 0)])
                   (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))
                 (directory-separator? (string-ref s 2)))))))

  (set-who! path-extension
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (let loop ([i n])
          (let ([i (fx- i 1)])
            (if (or (fx< i base) (directory-separator? (string-ref s i)))
                ""
                (if (char=? (string-ref s i) #\.)
                    (if (and (fx= i (fx- n 1))
                             (or (fx= i base)
                                 (directory-separator? (string-ref s (fx- i 1)))
                                 (and (char=? (string-ref s (fx- i 1)) #\.)
                                      (or (fx= (fx- i 1) base)
                                          (directory-separator? (string-ref s (fx- i 2)))))))
                        ""
                        (substring s (fx+ i 1) n))
                    (loop i))))))))

  (set-who! path-root
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (let loop ([i n])
          (let ([i (fx- i 1)])
            (if (or (fx< i base) (directory-separator? (string-ref s i)))
                s
                (if (char=? (string-ref s i) #\.)
                    (if (and (fx= i (fx- n 1))
                             (or (fx= i base)
                                 (directory-separator? (string-ref s (fx- i 1)))
                                 (and (char=? (string-ref s (fx- i 1)) #\.)
                                      (or (fx= (fx- i 1) base)
                                          (directory-separator? (string-ref s (fx- i 2)))))))
                        s
                        (substring s 0 i))
                    (loop i))))))))
  
  (set-who! path-last
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (let loop ([i n])
          (cond
            [(fx= i base) (if (fx= base 0) s (substring s base n))]
            [(directory-separator? (string-ref s (fx- i 1))) (substring s i n)]
            [else (loop (fx- i 1))])))))
  
  (set-who! path-parent
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (define (skip-sep-backward s i base)
        (let ([i (fx- i 1)])
          (if (or (fx= i base) (not (directory-separator? (string-ref s (fx- i 1)))))
              i
              (skip-sep-backward s i base))))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (let loop ([i n])
          (cond
            [(fx= i base) (substring s 0 base)]
            [(directory-separator? (string-ref s (fx- i 1)))
             (substring s 0 (skip-sep-backward s i base))]
            [else (loop (fx- i 1))])))))
  
  (set-who! path-first
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (if (fx= base 0)
            (let loop ([i 0])
              (cond
                [(fx= i n) ""]
                [(directory-separator? (string-ref s i)) (substring s 0 i)]
                [else (loop (fx+ i 1))]))
            (if (fx= base n) s (substring s 0 base))))))
  
  (set-who! path-rest
    (lambda (s)
      (define directory-separator? (directory-separator-predicate s))
      (define (skip-sep s i n)
        (if (or (fx= i n) (not (directory-separator? (string-ref s i))))
            i
            (skip-sep s (fx+ i 1) n)))
      (unless (string? s) ($oops who "~s is not a string" s))
      (let* ([n (string-length s)] [base (path-base s n)])
        (if (fx= base 0)
            (let loop ([i 0])
              (cond
                [(fx= i n) s]
                [(directory-separator? (string-ref s i))
                 (substring s (skip-sep s (fx+ i 1) n) n)]
                [else (loop (fx+ i 1))]))
            (substring s (skip-sep s base n) n)))))
)
)
