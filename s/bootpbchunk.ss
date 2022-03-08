;; This script helps convert boot files to pbchunk format, staging
;; everything in a new boot-file directory

(define who 'bootpbchunk)

(define args (command-line-arguments))
(when (null? args)
  (error who "missing srcdir"))
(when (null? (cdr args))
  (error who "missing target"))

(define srcdir (car args))
(define destdir (cadr args))
(define target (caddr args))
(define args (cdddr args))

(define scheme? (and (pair? args)
                     (equal? "--scheme" (car args))))
(define petite? (and (pair? args)
                     (equal? "--petite" (car args))))
(define only? (and (pair? args)
                   (equal? "--only" (car args))))
(define more-boots (if (or petite? scheme? only?)
                       (cdr args)
                       args))
(when (pair? more-boots)
  (let ([s (car more-boots)])
    (when (and (positive? (string-length s))
               (eqv? #\- (string-ref s 0)))
      (error who "unrecognized flag ~s" s))))

(for-each (lambda (more-boot)
            (let loop ([ml (reverse (string->list more-boot))]
                       [bl (reverse (string->list ".boot"))])
              (unless (null? bl)
                (when (or (null? ml)
                          (not (eqv? (car ml) (car bl))))
                  (error who "~s does not end with \".boot\"" more-boot))
                (loop (cdr ml) (cdr bl)))))
          more-boots)

(define petite-boot "petite.boot")
(define scheme-boot "scheme.boot")

(define boots (cond
                [only? '()]
                [petite? (list petite-boot)]
                [else (list petite-boot scheme-boot)]))

(define src-target
  (list->string (let loop ([l (string->list target)])
                  (cond
                    [(null? l)
                     (error 'bootchunk "no `-` in target ~s" target)]
                    [(eqv? #\- (car l)) '()]
                    [else (cons (car l) (loop (cdr l)))]))))

(define src (let ([p (string-append destdir "/boot/" src-target)])
              (if (file-directory? p)
                  p
                  (string-append srcdir "/" src-target))))

(unless (file-directory? src)
  (error who "cannot find base bootfiles for ~s" src-target))

(define xpatch (and (not (equal? src-target (symbol->string (machine-type))))
                    (format "~a/xc-~a/s/xpatch" destdir src-target)))
(unless (or (not xpatch)
            (file-exists? xpatch))
  (error who "cannot find cross patch file ~s" xpatch))

(for-each (lambda (f)
            (unless (file-exists? f)
              (error who "file not found: ~s" f)))
          more-boots)

(define dest (string-append destdir "/boot/" target))

(for-each (lambda (f)
            (delete-file (string-append dest "/" f)))
          (directory-list dest))

(define (copy-file f)
  (let ([i (open-file-input-port (string-append src "/" f))]
        [o (open-file-output-port (string-append dest "/" f) (file-options no-fail))]
        [buf (make-bytevector 4096)])
    (let loop ()
      (let ([n (get-bytevector-n! i buf 0 (bytevector-length buf))])
        (unless (eof-object? n)
          (put-bytevector o buf 0 n)
          (loop))))
    (close-input-port i)
    (close-output-port o)))

(for-each (lambda (f)
            (unless (member f boots)
              (copy-file f)))
          (directory-list src))

(define src-boots (append (map (lambda (f) (string-append src "/" f))
                               boots)
                          more-boots))
(define dest-boots (append (map (lambda (f) (string-append dest "/" f))
                                boots)
                           (map (lambda (f)
                                  (string-append dest "/" (path-last f)))
                                more-boots)))

(define (extract-boot-name f)
  (list->string
   (let loop ([l (string->list (path-last f))])
     (cond
       [(null? l) '()]
       [(eqv? #\. (car l)) '()]
       [else (cons (car l) (loop (cdr l)))]))))

(define (many fmt)
  (let loop ([i 0])
    (if (eqv? i 10)
        '()
        (cons (format fmt i) (loop (add1 i))))))

(when xpatch
  (load xpatch))

(let ([o (open-file-output-port (string-append dest "/" "Mf-config")
                                (file-options no-fail)
                                (buffer-mode block)
                                (current-transcoder))])
  (fprintf o "extraBootFiles=~a\n"
           (apply string-append
                  (map (lambda (path) (format "\"~a\" " (path-last path)))
                       more-boots)))
  (fprintf o "extraCSources=~a~a\n"
           (apply string-append
                  (apply append
                         (map (lambda (src-boot)
                                (let ([name (extract-boot-name src-boot)])
                                  (many (string-append "pbchunk_" name "~a.c "))))
                              src-boots)))
           "pbchunk_register.c")
  (close-port o))

(let loop ([src-boots src-boots]
           [dest-boots dest-boots]
           [index 0]
           [c-files '()]
           [reg-names '()])
  (cond
    [(null? src-boots)
     (let ([o (open-file-output-port (string-append dest "/" "pbchunk_register.c")
                                     (file-options no-fail)
                                     (buffer-mode block)
                                     (current-transcoder))])
       (for-each (lambda (reg-name)
                   (fprintf o "extern void ~a();\n" reg-name))
                 reg-names)
       (fprintf o "\nvoid pbchunk_register() {\n")
       (fprintf o "  /* last first, since last reflects overall size */\n")
       (for-each (lambda (reg-name)
                   (fprintf o "  ~a();\n" reg-name))
                 reg-names)
       (fprintf o "}\n")
       (close-output-port o))]
    [else
     (printf "Convert ~s\n" (car src-boots))
     (let ([name (extract-boot-name (car src-boots))])
       (let ([new-c-files (many (string-append dest "/" "pbchunk_" name "~a.c"))]
             [new-reg-names (many (string-append "pbchunk_register_" name "~a"))])
         (let ([index (pbchunk-convert-file (car src-boots)
                                            (car dest-boots)
                                            new-c-files
                                            new-reg-names
                                            index)])
           (loop (cdr src-boots)
                 (cdr dest-boots)
                 index
                 (append (reverse new-c-files) c-files) 
                 (append (reverse new-reg-names) reg-names)))))]))
