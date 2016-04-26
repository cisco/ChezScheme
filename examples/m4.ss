;;; m4.ss
;;; Copyright (C) 1988 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; speed improvement ideas:
;;;    use hash table rather than assoc for macro lookup
;;;    use extensible string buffer in place of lists
;;;    collect multiple characters when scanning text, arguments
;;;    use fixnum arithmetic where appropriate
 
(eval-when (compile) (optimize-level 3))

(define lexeme-type car)
(define lexeme-value cdr)

(define-structure (ibuf ip) ([pb '()]))

(define oparen #\()
(define cparen #\))

(define m4-get-char
   (lambda (ib)
      (let ([pb (ibuf-pb ib)])
         (if (null? pb)
             (read-char (ibuf-ip ib))
             (begin (set-ibuf-pb! ib (cdr pb))
                    (car pb))))))

(define m4-unget-char
   (lambda (c ib)
      (set-ibuf-pb! ib (cons c (ibuf-pb ib)))))

(define unget-string
   (lambda (s ib)
      (set-ibuf-pb! ib (append (string->list s) (ibuf-pb ib)))))

(define int->str
   (lambda (num)
      (format "~s" num)))

(define char->digit
   (let ([zero (char->integer #\0)])
      (lambda (c)
         (- (char->integer c) zero))))

(define str->int
   (let ([ustr->int
          (lambda (s i n)
             (let f ([a 0] [i i])
                (if (= i n)
                    a
                    (f (+ (* a 10) (char->digit (string-ref s i)))
                       (+ i 1)))))])
      (lambda (s)
         (let ([n (string-length s)])
            (if (= n 0)
                0
                (if (char=? (string-ref s 0) #\-)
                    (- (ustr->int s 1 n))
                    (ustr->int s 0 n)))))))

(define eval-string
   (let ([str #f] [port #f] [token #f] [value #f])
      (define eval-error
         (lambda ()
            (error 'm4 "invalid arithmetic expression ~s" str)))
      (define next-token!
         (lambda ()
            (let ([c (read-char port)])
               (cond
                  [(eof-object? c) (set! token 'eof)]
                  [(char-whitespace? c) (next-token!)]
                  [(char-numeric? c)
                   (let loop ([a (char->digit c)])
                      (let ([c (read-char port)])
                         (cond
                            [(eof-object? c)
                             (set! token 'integer)
                             (set! value a)]
                            [(char-numeric? c)
                             (loop (+ (* a 10) (char->digit c)))]
                            [else
                             (unread-char c port)
                             (set! token 'integer)
                             (set! value a)])))]
                  [(char=? c oparen) (set! token 'oparen)]
                  [(char=? c cparen) (set! token 'cparen)]
                  [(char=? c #\-) (set! token '-)]
                  [(char=? c #\*)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '*)]
                         [(char=? c #\*) (set! token '**)]
                         [else (unread-char c port) (set! token '*)]))]
                  [(char=? c #\+) (set! token '+)]
                  [(char=? c #\-) (set! token '+)]
                  [(char=? c #\/) (set! token '/)]
                  [(char=? c #\%) (set! token '%)]
                  [(char=? c #\!)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '!)]
                         [(char=? c #\=) (set! token '!=)]
                         [else (unread-char c port) (set! token '!)]))]
                  [(char=? c #\|)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\|) (set! token 'or)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\&)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\&) (set! token 'and)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\=)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\=) (set! token '==)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\<)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '<)]
                         [(char=? c #\=) (set! token '<=)]
                         [else (unread-char c port) (set! token '<)]))]
                  [(char=? c #\>)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '>)]
                         [(char=? c #\=) (set! token '>=)]
                         [else (unread-char c port) (set! token '>)]))]))))
      (define E0 ; or
         (lambda ()
            (E0* (E1))))
      (define E0*
         (lambda (v)
            (case token
               [or (next-token!) (E0* (if (= (+ v (E1)) 0) 0 1))]
               [else v])))
      (define E1 ; and
         (lambda ()
            (E1* (E2))))
      (define E1*
         (lambda (v)
            (case token
               [and (next-token!) (E1* (if (= (* v (E2)) 0) 0 1))]
               [else v])))
      (define E2 ; ==, !=
         (lambda ()
            (E2* (E3))))
      (define E2*
         (lambda (v)
            (case token
               [== (next-token!) (E2* (if (= v (E3)) 1 0))]
               [!= (next-token!) (E2* (if (= v (E3)) 0 1))]
               [else v])))
      (define E3 ; <, <=, >, >=
         (lambda ()
            (E3* (E4))))
      (define E3*
         (lambda (v)
            (case token
               [< (next-token!) (E3* (if (< v (E4)) 1 0))]
               [<= (next-token!) (E3* (if (<= v (E4)) 1 0))]
               [> (next-token!) (E3* (if (> v (E4)) 1 0))]
               [>= (next-token!) (E3* (if (>= v (E4)) 1 0))]
               [else v])))
      (define E4 ; +, -
         (lambda ()
            (E4* (E5))))
      (define E4*
         (lambda (v)
            (case token
               [+ (next-token!) (E4* (+ v (E5)))]
               [- (next-token!) (E4* (- v (E5)))]
               [else v])))
      (define E5 ; *, /, %
         (lambda ()
            (E5* (E6))))
      (define E5*
         (lambda (v)
            (case token
               [* (next-token!) (E5* (* v (E6)))]
               [/ (next-token!) (E5* (quotient v (E6)))]
               [% (next-token!) (E5* (modulo v (E6)))]
               [else v])))
      (define E6 ; **
         (lambda ()
            (E6* (E7))))
      (define E6*
         (lambda (v)
            (case token
               [** (next-token!) (E6* (expt v (E7)))]
               [else v])))
      (define E7 ; -, integer, paren
         (lambda ()
            (case token
               [- (next-token!) (- (E7))]
               [! (next-token!) (if (= (E7) 0) 1 0)]
               [oparen
                (next-token!)
                (let ([v (E0)])
                   (unless (eq? token 'cparen) (eval-error))
                   (next-token!)
                   v)]
               [integer (next-token!) value]
               [else (eval-error)])))
      (lambda (s)
         (fluid-let ([str s] [port (open-input-string s)] [token #f] [value #f])
            (next-token!)
            (let ([v (E0)])
               (unless (eq? token 'eof) (eval-error))
               v)))))

(define *divnum* #f)
(define *diversions* #f)

(define m4-put-string
   (lambda (s)
      (unless (= *divnum* -1)
         (display s (vector-ref *diversions* *divnum*)))))

(define *open-quote* #f)
(define *close-quote* #f)

(define *macros* #f)
(define builtin-macros '())

(define *translit-table* #f)

(define define-builtin-macro
   (lambda (name proc)
      (set! builtin-macros (cons (cons name proc) builtin-macros))))

(define m4
   (lambda (ofn ifn . rest)
      (let ([op (open-output-file ofn 'replace)])
         (fluid-let ([*macros* builtin-macros]
                     [*open-quote* #\`]
                     [*close-quote* #\']
                     [*translit-table* #f]
                     [*divnum* 0]
                     [*diversions* (vector op #f #f #f #f #f #f #f #f #f)])
            (let loop ([ip (open-input-file ifn)] [rest rest])
               (m4-process (make-ibuf ip) op)
               (close-input-port ip)
               (unless (null? rest)
                  (loop (open-input-file (car rest)) (cdr rest))))
            (for-each undivert '(1 2 3 4 5 6 7 8 9)))
         (close-output-port op))))

(define m4-process
   (lambda (ib op)
      (let ([lexeme (read-lexeme ib)])
         (case (lexeme-type lexeme)
            [(comment literal)
             (m4-put-string (lexeme-value lexeme))
             (m4-process ib op)]
            [macro
             ((cdr (lexeme-value lexeme)) (read-args ib) ib)
             (m4-process ib op)]
            [eof #t]
            [else (error 'm4-internal "unexpected lexeme ~s" lexeme)]))))

(define name-start-char?
   (lambda (c)
      (or (char-alphabetic? c)
          (char=? c #\_))))

(define name-char?
   (lambda (c)
      (or (name-start-char? c)
          (char-numeric? c))))

(define read-lexeme
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) (cons 'eof c)]
            [(char=? c #\#) (cons 'comment (read-comment ib))]
            [(char=? c *open-quote*) (cons 'literal (read-quoted ib))]
            [(name-start-char? c) (lookup-macro (cons c (read-alpha ib)))]
            [else (cons 'literal (string c))]))))

(define read-comment
   (lambda (ib)
      (let loop ([ls '(#\#)])
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c) (list->string (reverse ls))]
               [(char=? c #\newline) (list->string (reverse (cons c ls)))]
               [else (loop (cons c ls))])))))

(define read-quoted
   (lambda (ib)
      (let loop ([ls '()] [n 0])
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c)
                (error 'm4 "end-of-file detected at quote level ~s" n)]
               [(char=? c *close-quote*)
                (if (= n 0)
                    (list->string (reverse ls))
                    (loop (cons c ls) (- n 1)))]
               [(char=? c *open-quote*) (loop (cons c ls) (+ n 1))]
               [else (loop (cons c ls) n)])))))

(define read-alpha
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) '()]
            [(name-char? c) (cons c (read-alpha ib))]
            [else (m4-unget-char c ib) '()]))))

(define lookup-macro
   (lambda (ls)
      (let ([s (list->string ls)])
         (let ([a (assoc s *macros*)])
            (if a
                (cons 'macro a)
                (cons 'literal s))))))

(define read-args
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) '()]
            [(char=? c oparen)
             (let next-arg ()
                (let skip-white ()
                   (let ([c (m4-get-char ib)])
                      (cond
                         [(eof-object? c) '()]
                         [(char-whitespace? c) (skip-white)]
                         [else (m4-unget-char c ib)])))
                (let this-arg ([strings '()])
                   (let ([c (m4-get-char ib)])
                      (cond
                         [(or (eof-object? c) (char=? c cparen))
                          (if (null? strings)
                              '()
                              (cons (apply string-append (reverse strings))
                                    '()))]
                         [(char=? c oparen)
                          (let nest ([strings (cons (string oparen)
                                                    strings)]
                                     [k this-arg])
                             (let ([c (m4-get-char ib)])
                                (cond
                                   [(eof-object? c) (this-arg strings)]
                                   [(char=? c cparen)
                                    (k (cons (string cparen) strings))]
                                   [(char=? c oparen)
                                    (nest (cons (string oparen) strings)
                                          (lambda (strings)
                                             (nest strings k)))]
                                   [else
                                    (m4-unget-char c ib)
                                    (let ([lexeme (read-lexeme ib)])
                                       (case (lexeme-type lexeme)
                                          [comment (nest strings k)]
                                          [literal
                                           (nest (cons (lexeme-value lexeme)
                                                       strings)
                                                 k)]
                                          [macro
                                           ((cdr (lexeme-value lexeme))
                                            (read-args ib)
                                            ib)
                                           (nest strings k)]
                                          [else
                                           (error 'm4-internal
                                                  "unexpected lexeme ~s"
                                                  lexeme)]))])))]
                         [(char=? c #\,)
                          (cons (apply string-append (reverse strings))
                                (next-arg))]

                         [else
                          (m4-unget-char c ib)
                          (let ([lexeme (read-lexeme ib)])
                             (case (lexeme-type lexeme)
                                [comment (this-arg strings)]
                                [literal
                                 (this-arg
                                     (cons (lexeme-value lexeme) strings))]
                                [macro
                                 ((cdr (lexeme-value lexeme)) (read-args ib) ib)
                                 (this-arg strings)]
                                [else
                                 (error 'm4-internal
                                        "unexpected lexeme ~s"
                                        lexeme)]))]))))]
            [else (m4-unget-char c ib) '()]))))

;;; builtin macros

(define $$ (lambda (ls) (if (null? ls) ls (cdr ls))))
(define $1 (lambda (ls) (if (null? ls) "" (car ls))))
(define $2 (lambda (ls) ($1 ($$ ls))))
(define $3 (lambda (ls) ($2 ($$ ls))))
(define $4 (lambda (ls) ($3 ($$ ls))))
(define $5 (lambda (ls) ($4 ($$ ls))))
(define $6 (lambda (ls) ($5 ($$ ls))))
(define $7 (lambda (ls) ($6 ($$ ls))))
(define $8 (lambda (ls) ($7 ($$ ls))))
(define $9 (lambda (ls) ($8 ($$ ls))))

(define-builtin-macro "changequote"
   (lambda (args ib)
      (set! *open-quote*
            (if (string=? ($1 args) "") #\` (string-ref ($1 args) 0)))
      (set! *close-quote*
            (if (string=? ($2 args) "") #\' (string-ref ($2 args) 0)))))

(define-builtin-macro "define"
   (lambda (args ib)
      (let ([name ($1 args)])
         (unless (let ([n (string-length name)])
                    (and (fx> n 0)
                         (name-start-char? (string-ref name 0))
                         (let ok? ([i 1])
                            (or (fx= i n)
                                (and (name-char? (string-ref name i))
                                     (ok? (fx+ i 1)))))))
            (error 'm4-define "invalid macro name ~s" name))
         (let ([proc (make-macro ($2 args))])
            (let ([a (assoc name *macros*)])
               (if a
                   (set-cdr! a proc)
                   (set! *macros* (cons (cons name proc) *macros*))))))))

(define make-macro
   (lambda (s)
      (let ([ls (string->list s)])
         (lambda (args ib)
            (let loop ([ls ls])
               (unless (null? ls)
                  (case (and (char=? (car ls) #\$)
                             (not (null? (cdr ls)))
                             (cadr ls))
                     [#\1 (loop (cddr ls)) (unget-string ($1 args) ib)]
                     [#\2 (loop (cddr ls)) (unget-string ($2 args) ib)]
                     [#\3 (loop (cddr ls)) (unget-string ($3 args) ib)]
                     [#\4 (loop (cddr ls)) (unget-string ($4 args) ib)]
                     [#\5 (loop (cddr ls)) (unget-string ($5 args) ib)]
                     [#\6 (loop (cddr ls)) (unget-string ($6 args) ib)]
                     [#\7 (loop (cddr ls)) (unget-string ($7 args) ib)]
                     [#\8 (loop (cddr ls)) (unget-string ($8 args) ib)]
                     [#\9 (loop (cddr ls)) (unget-string ($9 args) ib)]
                     [else (loop (cdr ls)) (m4-unget-char (car ls) ib)])))))))

(define-builtin-macro "divert"
   (lambda (args ib)
      (set! *divnum*
         (if (string=? ($1 args) "")
             0
             (case (string-ref ($1 args) 0)
                [#\0 0]
                [#\1 1]
                [#\2 2]
                [#\3 3]
                [#\4 4]
                [#\5 5]
                [#\6 6]
                [#\7 7]
                [#\8 8]
                [#\9 9]
                [else -1])))
      (when (and (<= 1 *divnum* 9) (not (vector-ref *diversions* *divnum*)))
         (vector-set! *diversions* *divnum* (open-output-string)))))

(define-builtin-macro "divnum"
   (lambda (args ib)
      (unget-string (format "~a" *divnum*) ib)))

(define-builtin-macro "dnl"
   (lambda (args ib)
      (let loop ()
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c) '()]
               [(char=? c #\newline) '()]
               [else (loop)])))))

(define-builtin-macro "dumpdef"
   (lambda (args ib)
      (printf "m4 warning: no dumpdef yet~%")))

(define-builtin-macro "errprint"
   (lambda (args ib)
      (display ($1 args) *error-output*)
      (newline *error-output*)))

(define-builtin-macro "eval"
   (lambda (args ib)
      (unget-string (int->str (eval-string ($1 args))) ib)))

(define-builtin-macro "ifdef"
   (lambda (args ib)
      (unget-string ((if (assoc ($1 args) *macros*) $2 $3) args) ib)))

(define-builtin-macro "ifelse"
   (rec ifelse
      (lambda (args ib)
         (if (string=? ($1 args) ($2 args))
             (unget-string ($3 args) ib)
             (if (> (length args) 4)
                 (ifelse ($$ ($$ ($$ args))) ib)
                 (unget-string ($4 args) ib))))))

(define-builtin-macro "include"
   (lambda (args ib)
      (printf "m4 warning: no include yet~%")))

(define-builtin-macro "incr"
   (lambda (args ib)
      (unget-string (int->str (+ (str->int ($1 args)) 1)) ib)))

(define-builtin-macro "index"
   (lambda (args ib)
      (let ([s1 ($1 args)] [s2 ($2 args)])
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
            (let find ([i 0])
               (if (fx> n2 (fx- n1 i))
                   (unget-string "-1" ib)
                   (let try ([i1 i] [i2 0])
                      (if (fx= i2 n2)
                          (unget-string (int->str i) ib)
                          (if (char=? (string-ref s1 i1) (string-ref s2 i2))
                              (try (fx+ i1 1) (fx+ i2 1))
                              (find (fx+ i 1)))))))))))

(define-builtin-macro "len"
   (lambda (args ib)
      (unget-string (int->str (string-length ($1 args))) ib)))

(define-builtin-macro "maketemp"
   (lambda (args ib)
      (printf "m4 warning: no maketemp yet~%")))

(define-builtin-macro "shift"
   (lambda (args ib)
      (printf "m4 warning: no shift yet~%")))

(define-builtin-macro "sinclude"
   (lambda (args ib)
      (printf "m4 warning: no sinclude yet~%")))

(define-builtin-macro "substr"
   (lambda (args ib)
      (let ([s ($1 args)] [start ($2 args)] [count ($3 args)])
         (let ([n (string-length s)])
            (let ([start (min (max (str->int start) 0) n)])
               (let ([end (if (string=? count "")
                              n
                              (min (max (+ (str->int count) start) start) n))])
                  (unget-string (substring s start end) ib)))))))

(define-builtin-macro "syscmd"
   ;;; cannot be written in Scheme---needs something more powerful than
   ;;; "system" or "process"
   (lambda (args ib)
      (printf "m4 warning: no syscmd yet~%")))

(define-builtin-macro "translit"
   (lambda (args ib)
      (let ([s1 ($1 args)] [s2 ($2 args)] [s3 ($3 args)])
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
            (unless (= n2 (string-length s3))
               (error 'm4 "translit arguments ~s and ~s are not of same length"
                      s2 s3))
            (when (null? *translit-table*)
               (set! *translit-table* (make-string 256)))
            (do ([i 0 (fx+ i 1)])
                ((fx= i 256))
                (string-set! *translit-table* i (integer->char i)))
            (do ([i 0 (fx+ i 1)])
                ((fx= i n2))
                (string-set! *translit-table*
                             (char->integer (string-ref s2 i))
                             (string-ref s3 i)))
            (let ([s4 (make-string n1)])
               (do ([i 0 (fx+ i 1)])
                   ((fx= i n1))
                   (string-set! s4 i
                      (string-ref *translit-table*
                                  (char->integer (string-ref s1 i)))))
               (unget-string s4 ib))))))

(define-builtin-macro "undefine"
   (lambda (args ib)
      (let ([a (assoc ($1 args) *macros*)])
         (unless a (error 'm4 "cannot undefine ~s (not defined)" ($1 args)))
         (set-car! a #f))))

(define-builtin-macro "undivert"
   (rec myself
      (lambda (args ib)
         (if (null? args)
             (myself '("1" "2" "3" "4" "5" "6" "7" "8" "9") ib)
             (for-each
                (lambda (x)
                   (case (and (not (string=? x "")) (string-ref x 0))
                      [#\1 (undivert 1)]
                      [#\2 (undivert 2)]
                      [#\3 (undivert 3)]
                      [#\4 (undivert 4)]
                      [#\5 (undivert 5)]
                      [#\6 (undivert 6)]
                      [#\7 (undivert 7)]
                      [#\8 (undivert 8)]
                      [#\9 (undivert 9)]))
                args)))))

(define undivert
   (lambda (n)
      (let ([op (vector-ref *diversions* n)])
         (when op
            (display (get-output-string op) (vector-ref *diversions* 0))))))
