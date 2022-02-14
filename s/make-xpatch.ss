;; This script is an alternative to using the `Mf-cross` makefile and
;; "xpatch" target. It should be run (twice) in the "s" directory of a
;; workarea that is set up simply as a copy of the "s", "nanopass",
;; and "unicode" source directories. When run multiple times, it skips
;; work if things have not changed (in terms of file timestamps).

;; First run:
;;   scheme --script make-xpatch.ss <target> patch
;; Second run:
;;   scheme --script make-xpatch.ss <target> build

(define target-machine-str (car (command-line-arguments)))
(define phase (cadr (command-line-arguments)))

(define target-machine (string->symbol target-machine-str))
(define t? (eqv? #\t (string-ref target-machine-str 0)))
(define target-machine-uni-str (if t?
                                   (substring target-machine-str 1 (string-length target-machine-str))
                                   target-machine-str))
(define target-machine-uni (string->symbol target-machine-uni-str))

(define starts?
  (case-lambda
   [(a b) (starts? a 0 b)]
   [(a delta b)
    (and (>= (- (string-length a) delta) (string-length b))
         (equal? b (substring a delta (+ delta (string-length b)))))]))
  
(define (replace s find repl)
  (let loop ([i 0])
    (cond
      [(= i (string-length s)) s]
      [(starts? s i find)
       (string-append (substring s 0 i)
                      (format "~a" repl)
                      (replace (substring s (+ i (string-length find)) (string-length s))
                               find
                               repl))]
      [else (loop (add1 i))])))

(define (split-line s after)
  (let loop ([l (string->list (substring s after (string-length s)))]
             [a '()])
    (cond
      [(null? l) (if (null? a)
                     '()
                     (list (list->string (reverse a))))]
      [(eqv? #\space (car l)) (append (loop '() a)
                                      (loop (cdr l) '()))]
      [else (loop (cdr l) (cons (car l) a))])))

(define (file->string f)
  (call-with-input-file
   f
   (lambda (in)
     (apply
      string-append
      (let loop ()
        (let ([s (get-string-n in 4096)])
          (if (eof-object? s)
              '()
              (cons s (loop)))))))))

(define (string->file s f)
  (when (file-exists? f) (delete-file f))
  (call-with-output-file f
    (lambda (o)
      (put-string o s))))

(define target-arch
  (ormap (lambda (arch-str)
           (and (starts? target-machine-uni-str arch-str)
                (string->symbol arch-str)))
         '("pb" "a6" "i3" "arm32" "arm64" "ppc32")))

(define newer? #f)
(define (newer! who)
  (unless newer?
    (printf "[newer ~s]\n" who)
    (set! newer? #t)))

;; This is duplicated from `workarea`:
(define (generate-def)
  (define def-str
    (cond
      [(file-exists? (format "~a.def" target-machine))
       (file->string (format "~a.def" target-machine))]
      [else
       (case target-arch
         [(pb)
          (let ([target-word (if (starts? target-machine-uni-str "pb64")
                                 64
                                 32)]
                [target-endianness (string (string-ref target-machine-str (sub1 (string-length target-machine-str))))])
            (let* ([s (file->string (if t? "tpbarch.def" "pbarch.def"))]
                   [s (replace s "$(M)" target-machine)]
                   [s (replace s "$(March)" target-arch)]
                   [s (replace s "$(Mword)" target-word)]
                   [s (replace s "$(Mend)" target-endianness)])
              s))]
         [else
          (let* ([s (file->string (if t? "tunix.def" "unix.def"))]
                 [s (replace s "$(M)" target-machine)]
                 [s (replace s "$(March)" target-arch)])
            s)])]))
  (unless (and (file-exists? "machine.def")
               (equal? def-str (file->string "machine.def")))
    (string->file def-str "machine.def")
    (newer! "machine.def")))

(define macroobjs '())
(define patchobjs '())
(define basesrcs '())
(define compilersrcs '())

(define (strip-cr s)
  (if (string? s)
      (let ([len (string-length s)])
        (if (and (positive? len)
                 (eqv? #\return (string-ref s (sub1 len))))
            (substring s 0 (sub1 len))
            s))
      s))

(define (read-continued-line in)
  (define str (strip-cr (get-line in)))
  (cond
    [(eof-object? str) str]
    [(string=? str "") str]
    [(eqv? #\\ (string-ref str (sub1 (string-length str))))
     (string-append (substring str 0 (sub1 (string-length str)))
                    (read-continued-line in))]
    [else str]))

(define (extract line var-str)
  (and (starts? line var-str)
       (split-line line (string-length var-str))))

(define (load-makefile f)
  (call-with-input-file
   f
   (lambda (in)
     (let loop ()
       (define line (read-continued-line in))
       (unless (eof-object? line)
         (cond
           [(extract line "macroobj =") => (lambda (v) (set! macroobjs v))]
           [(extract line "patchobj =") => (lambda (v) (set! patchobjs v))]
           [(extract line "basesrc =") => (lambda (v) (set! basesrcs v))]
           [(extract line "compilersrc =") => (lambda (v) (set! compilersrcs v))])
         (loop))))))

(load-makefile "Mf-base")

(define (target-so src)
  (replace src ".ss" (format ".~s" target-machine)))

(define (target-boot p)
  (format "../boot/~a/~a" target-machine p))

(define (newer-file?! src obj)
  (and (or newer?
           (not (file-exists? obj))
           (time>? (file-modification-time src)
                   (file-modification-time obj)))
       (begin
         (newer! src)
         #t)))

(define (newer-tree?! dir file)
  (ormap (lambda (f)
           (let ([f (string-append dir "/" f)])
             (if (file-directory? f)
                 (newer-tree?! f file)
                 (newer-file?! f file))))
         (directory-list dir)))

(define (compile-newer-file src obj)
  (when (newer-file?! src obj)
    (compile-file src obj)))

(define (compile-and-load obj)
  (compile-newer-file (replace obj ".so" ".ss") obj)
  (load obj))
  
(define (call-if-newer name dest-file . args)
  (when (or newer?
            (not (file-exists? dest-file)))
    (newer! name)
    (apply (eval name) dest-file args)))

(define (mkdir-p f)
  (unless (file-exists? f)
    (mkdir f)))

(define (configure)
  (reset-handler abort)
  (base-exception-handler (lambda (c) (fresh-line) (display-condition c) (newline) (reset)))
  (keyboard-interrupt-handler (lambda () (display "interrupted---aborting\n") (reset)))
  (optimize-level 3)
  (debug-level 0)
  (fasl-compressed #t)
  (generate-inspector-information #f)
  (subset-mode 'system)
  (library-directories "../nanopass"))

(case (string->symbol phase)
  [(macro)
   (printf "Creating cross compiler if needed...\n")
   (generate-def)

   (when (newer-tree?! "../nanopass" "nanopass_done")
     (compile-imported-libraries #t)
     (library-directories "../nanopass")
     (compile-library "../nanopass/nanopass.ss" "nanopass.so")
     (when (file-exists? "nanopass_done") (delete-file "nanopass_done"))
     (call-with-output-file "nanopass_done" (lambda (out) (void))))
   
   (configure)

   (for-each compile-and-load
             (remove "setup.so" macroobjs))

   (compile-and-load "mkheader.so")
   (compile-and-load "mkgc.so")

   (mkdir-p "../boot")
   (mkdir-p (target-boot ""))
   (call-if-newer 'mkscheme.h (target-boot "scheme.h") target-machine)
   (call-if-newer 'mkequates.h (target-boot "equates.h"))
   (call-if-newer 'mkgc-ocd.inc (target-boot "gc-ocd.inc"))
   (call-if-newer 'mkgc-oce.inc (target-boot "gc-oce.inc"))
   (call-if-newer 'mkgc-par.inc (target-boot "gc-par.inc"))
   (call-if-newer 'mkheapcheck.inc (target-boot "heapcheck.inc"))
   (compile-and-load "setup.so")

   (for-each (lambda (obj)
               (compile-newer-file (replace obj ".patch" ".ss") obj))
             patchobjs)

   (when (file-exists? "is_newer") (delete-file "is_newer"))
   (when newer? (call-with-output-file "is_newer" (lambda (out) (void))))]
  [(build)
   ;; If we compile anything, compile all together to have deterministic record names:
   (let ([srcs (append basesrcs
                       compilersrcs)])
     (when (or (file-exists? "is_newer")
               (ormap newer-file?! srcs (map target-so srcs)))
       (printf "Cross-compiling boot files...\n")
       ;; load patch
       (for-each load macroobjs)
       (for-each load patchobjs)
       (configure)
       
       (for-each (lambda (src)
                   (compile-file src (target-so src)))
                 srcs)
       
       (apply #%$make-boot-file (target-boot "petite.boot") target-machine '() (map target-so basesrcs))
       (apply #%$make-boot-file (target-boot "scheme.boot") target-machine '("petite") (map target-so compilersrcs))))])
