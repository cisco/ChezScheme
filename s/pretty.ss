;;; pretty.ss
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

;;;; TODO

;;; support enough power to describe generic as a pattern?
;;; retain external comments in pretty-file?

;;; $make-graph-env and $last-new-vector-element are from print.ss

;;;; EXPORTED VARIABLES

;;; pretty tries to fit things within line length
(begin
(define pretty-line-length
   ($make-thread-parameter
      75
      (lambda (x)
         (unless (and (fixnum? x) (fx> x 0))
            ($oops 'pretty-line-length "~s is not a positive fixnum" x))
         x)))

;;; pretty tries to break things longer than one line limit
(define pretty-one-line-limit
   ($make-thread-parameter
      60
      (lambda (x)
         (unless (and (fixnum? x) (fx> x 0))
            ($oops 'pretty-one-line-limit "~s is not a positive fixnum" x))
         x)))

;;; initial indentation of first line; used to properly align subsequent
;;; lines
(define pretty-initial-indent
   ($make-thread-parameter
      0
      (lambda (x)
         (unless (and (fixnum? x) (fx>= x 0))
            ($oops 'pretty-initial-indent "~s is not a nonnegative fixnum" x))
         x)))

;;; standard indent
(define pretty-standard-indent
   ($make-thread-parameter
      1
      (lambda (x)
         (unless (and (fixnum? x) (fx>= x 0))
            ($oops 'pretty-standard-indent "~s is not a nonnegative fixnum" x))
         x)))

(define pretty-maximum-lines
   ($make-thread-parameter
      #f
      (lambda (x)
         (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
            ($oops 'pretty-maximum-lines "~s is not a positive fixnum or #f" x))
         x)))

(define pretty-print)
(define pretty-file)
(define pretty-format)

;;;; IMPLEMENTATION

(let ()

;;; supported formats:

;;; <fmt>      -> (quote <symbol>)
;;;             | var
;;;             | <symbol>
;;;             | (read-macro <string> <symbol>)
;;;             | (meta)
;;;             | (alt <fmt> <fmt>*)
;;;             | (bracket . <fmt-tail>)
;;;             | <fmt-tail>
;;; <fmt-tail> -> ()
;;;             | (<tab> <fmt> ...)
;;;             | (<fmt> <tab> ...)
;;;             | (<tab> <fmt> . <fmt-tail>)
;;;             | (<fmt> ...)
;;;             | (<fmt> . <fmt-tail>)
;;;             | (fill <tab> <fmt> ...)
;;; <tab>      -> <int>
;;;             | #f

(define (tab? x) (or (eq? x #f) (fixnum? x)))
(define (dots? x) (eq? x '...))

(define (fmt? x)
  (define (fmt-tail? x)
    (syntax-case x (fill)
      [() #t]
      [(fill tab x dots) (and (tab? #'tab) (dots? #'dots) (fmt? #'x))]
      [(tab x dots) (and (tab? #'tab) (dots? #'dots)) (fmt? #'x)]
      [(x tab dots) (and (tab? #'tab) (dots? #'dots)) (fmt? #'x)]
      [(tab x . t) (tab? #'tab) (and (fmt? #'x) (fmt-tail? #'t))]
      [(x dots) (dots? #'dots) (fmt? #'x)]
      [(x . t) (and (fmt? #'x) (fmt-tail? #'t))]
      [_ #f]))
  (syntax-case x (quote var read-macro meta alt bracket)
    [(quote sym) (symbol? #'sym)]
    [var #t]
    [sym (and (symbol? #'sym) (not (dots? #'sym)) (not (eq? #'sym 'fill))) #t]
    [(read-macro str sym) (and (string? #'str) (symbol? #'sym))]
    [(meta) #t]
    [(alt . x*) (and (list? #'x*) (not (null? #'x*)) (andmap fmt? #'x*))]
    [(bracket . x) (fmt-tail? #'x)]
    [x (fmt-tail? #'x)]))

(define (score-fmt fmt expr) ; lower is better, zero is best
  (define (score-fmt-tail fmt-tail expr s)
    (define (score-rest fmt expr s)
      (if (list? expr)
          (let ([s (fl/ s (fixnum->flonum (fx+ (length expr) 1)))])
            (apply fl+ (map (lambda (expr) (score-fmt fmt expr s)) expr)))
          s))
    (syntax-case fmt-tail (fill)
      [() (if (null? expr) 0.0 s)]
      [(fill tab fmt dots) (score-rest #'fmt expr s)]
      [(tab fmt dots) (and (tab? #'tab) (dots? #'dots)) (score-rest #'fmt expr s)]
      [(fmt tab dots) (and (tab? #'tab) (dots? #'dots)) (score-rest #'fmt expr s)]
      [(tab fmt . fmt-tail)
       (tab? #'tab)
       (if (pair? expr)
           (let ([s (fl/ s 3.0)])
             (fl+ (score-fmt #'fmt (car expr) s)
                  (score-fmt #'fmt-tail (cdr expr) s)))
           s)]
      [(fmt dots) (dots? #'dots) (score-rest #'fmt expr s)]
      [(fmt . fmt-tail)
       (if (pair? expr)
           (let ([s (fl/ s 3.0)])
             (fl+ (score-fmt #'fmt (car expr) s)
                  (score-fmt #'fmt-tail (cdr expr) s)))
           s)]))
  (define (score-fmt fmt expr s)
    (syntax-case fmt (quote var read-macro meta alt bracket s)
      [(quote sym) (if (eq? expr #'sym) 0.0 s)]
      [var (if (symbol? expr) 0.0 s)]
      [sym (symbol? #'sym) 0.0]
      [(read-macro str sym) 
       (syntax-case expr ($primitive)
         [(_ x) 0.0]
         [($primitive n x) (if (memq #'n '(2 3)) 0.0 s)]
         [else s])]
      [(meta)
       (syntax-case expr (meta)
         [(meta . x) 0.0]
         [else s])]
      [(alt . fmt*)
       (let f ([fmt* (cdr #'fmt*)] [min-s (score-fmt (car #'fmt*) expr s)])
         (if (or (null? fmt*) (fl= min-s 0.0))
             min-s
             (f (cdr fmt*) (min (score-fmt (car fmt*) expr s) min-s))))]
      [(bracket . fmt-tail) (score-fmt-tail #'fmt-tail expr s)]
      [fmt-tail (score-fmt-tail #'fmt-tail expr s)]))
  (score-fmt fmt expr 1.0))

(define (select-alt-fmt fmt expr)
  (cond
    [(syntax-case fmt (alt)
       [(alt . fmt*) #'fmt*]
       [_ #f]) =>
     (lambda (fmt*)
       (let ([fmt (car fmt*)])
         (let f ([fmt* (cdr fmt*)] [min-s (score-fmt fmt expr)] [min-fmt fmt])
           (if (or (null? fmt*) (fl= min-s 0.0))
               (select-alt-fmt min-fmt expr)
               (let* ([fmt (car fmt*)] [s (score-fmt fmt expr)])
                 (if (fl< s min-s)
                     (f (cdr fmt*) s fmt)
                     (f (cdr fmt*) min-s min-fmt)))))))]
    [else fmt]))

(define-syntax decr
  (lambda (x)
    (syntax-case x ()
      ((_ x) (identifier? #'x) #'(and x (fx- x 1))))))
 
(define-syntax limit?
  (syntax-rules ()
    ((_ x) (eq? x 0))))

(define-record-type prty
  (fields (mutable fmt) (mutable len) (mutable obj))
  (nongenerative)
  (sealed #t))

(define-threaded si)                ; standard indent
(define-threaded lines)             ; max number of lines to print
(define-threaded col)
(define-threaded room)
(define-threaded port)
(define-threaded pretty-string-output-port)
(define-threaded graph-env)
(define lparen #\()        ;)( make paren bouncer happy
(define rparen #\))
(define lbrack #\[)        ;][ make paren bouncer happy
(define rbrack #\])

(define check-line-maximum
   (lambda ()
      (when lines
         (if (= lines 0)
             ($oops 'pretty-print "maximum-lines parameter exceeded")
             (set! lines (- lines 1))))))

(define tab-amount
   (lambda (x)
      (if (and (integer? x) (nonnegative? x))
          x
          (if (eq? x #f)
              si
              #f))))

(define mk-prty
  (lambda (x lev len fmt)
    (let ([a (and graph-env (graph-env 'tag x))])
      (if (not a)
          (mk-prty-help x lev len fmt)
          (record-case a
            [(mark) n
             (let ([s (format "#~d=" n)]
                   [r (mk-prty-help x lev len fmt)])
               (make-prty `(read-macro ,s x)
                          (+ (string-length s) (prty-len r))
                          r))]
            [(ref) n
             (let ([s (format "#~d#" n)])
               (make-prty 'x (string-length s) s))])))))

(define mk-prty-help
  (lambda (x lev len fmt)
    (define (get-pretty-format x)
      (let ([fmt (and (symbol? (car x)) (pretty-format (car x)))])
        (and fmt (select-alt-fmt fmt x))))
    (cond
      [(pair? x)
      ; choose x's format, if any, if it is a read macro or if the
      ; incoming format is atomic.
       (let ([fmt (let ([x-fmt (get-pretty-format x)])
                    (let ([fmt (select-alt-fmt fmt x)])
                      (if (and x-fmt (syntax-case fmt (quote)
                                       [(quote sym) #t]
                                       [sym (symbol? #'sym) #t]
                                       [_ #f]))
                          x-fmt
                          fmt)))])
         (syntax-case fmt (read-macro meta)
           [(read-macro str sym)
            (cond
              [(and (pair? (cdr x))
                    (null? (cddr x))
                    (or (not (eq? (car x) '$primitive))
                        (symbol? (cadr x))))
               (let ([p (mk-prty (cadr x) lev len 'x)])
                 (make-prty fmt (+ (string-length #'str) (prty-len p)) p))]
              [(and (eq? (car x) '$primitive)
                    (pair? (cdr x))
                    (pair? (cddr x))
                    (null? (cdddr x))
                    (memv (cadr x) '(2 3))
                    (symbol? (caddr x)))
               (let ([p (mk-prty (caddr x) lev len 'x)]
                     [s (format "#~d%" (cadr x))])
                 (make-prty `(read-macro ,s x)
                            (+ (string-length s) (prty-len p))
                            p))]
              [else (mk-prty-list x lev len 'x)])]
           [(meta)
            (let ([defn (cdr x)])
              (let ([fmt (and (pair? defn) (get-pretty-format defn))])
                (mk-prty-list x lev len
                  (if fmt `(meta . ,fmt) '(meta x #f ...)))))]
           [_ (mk-prty-list x lev len fmt)]))]
      [(vector? x)
       (let ([n (vector-length x)])
          (if (= n 0)
              (if (print-vector-length)
                  (make-prty '() 4 "#0()")
                  (make-prty '() 3 "#()"))
              (let ([p (mk-prty-vector vector-length vector-ref x lev len)]
                    [s (if (print-vector-length) (format "#~d" n) "#")])
                 (make-prty `(read-macro ,s x)
                            (+ (string-length s) (prty-len p))
                            p))))]
      [(fxvector? x)
       (let ([n (fxvector-length x)])
          (if (= n 0)
              (if (print-vector-length)
                  (make-prty '() 4 "#0vfx()")
                  (make-prty '() 3 "#vfx()"))
              (let ([p (mk-prty-vector fxvector-length fxvector-ref x lev len)]
                    [s (if (print-vector-length) (format "#~dvfx" n) "#vfx")])
                 (make-prty `(read-macro ,s x)
                            (+ (string-length s) (prty-len p))
                            p))))]
      [(bytevector? x)
       (let ([n (bytevector-length x)])
          (if (= n 0)
              (if (print-vector-length)
                  (make-prty '() 4 "#0vu8()")
                  (make-prty '() 3 "#vu8()"))
              (let ([p (mk-prty-vector bytevector-length bytevector-u8-ref x lev len)]
                    [s (if (print-vector-length) (format "#~dvu8" n) "#vu8")])
                 (make-prty `(read-macro ,s x)
                            (+ (string-length s) (prty-len p))
                            p))))]
      [(box? x)
       (if (limit? lev)
           (make-prty '() 5 "#&...")
           (let ([r (mk-prty (unbox x) (decr lev) len 'x)])
              (make-prty '(read-macro "#&" x) (+ 2 (prty-len r)) r)))]
      [(eq? x '#0=#0#)
       (if (limit? lev)
           (make-prty 'x 3 "...")
           (mk-prty x (decr lev) len 'x))]
      [else
       ($write-pretty-quick x lev len graph-env pretty-string-output-port)
       (let ([s (get-output-string pretty-string-output-port)])
          (make-prty 'x (string-length s) s))])))

(define mk-prty-vector
   (lambda (vlen vref x lev len)
      (if (limit? lev)
          (make-prty '() 5 "(...)")
          (let ([prtys
                 (let ([m (if (print-vector-length)
                              ($last-new-vector-element vlen vref x)
                              (fx- (vlen x) 1))])
                    (if (and len (fx<= len m))
                        (mk-prty-vector-help vref
                           x (decr lev) len (fx- len 1)
                           (list (make-prty '() 3 "...")))
                        (mk-prty-vector-help vref
                           x (decr lev) len m '())))])
             (make-prty '(fill 0 x ...) (prtys-size prtys) prtys)))))

; Order of evaluation is important here to make sure that "#n#" numbers
; get assigned in order and that the "#n=" assignment comes before any
; "#n# references.

(define mk-prty-vector-help
   (lambda (vref x lev len n prtys-tail)
      (let mk ([i 0])
         (if (fx> i n)
             prtys-tail
             (let ([first (mk-prty (vref x i) lev len 'x)])
                (cons first (mk (fx+ i 1))))))))

(define prtys-size
   (lambda (ls)
      (let f ([ls ls] [n 1])
         (if (null? ls)
             n
             (f (cdr ls) (+ (+ (prty-len (car ls)) n) 1))))))

(define mk-prty-list
  (lambda (x lev len fmt)
    (let* ([fmt (syntax-case fmt (quote)
                  [(quote sym) '()]
                  [sym (symbol? #'sym) '()]
                  [_ fmt])]
           [prtys (mk-prty-list-help x lev len
                    (syntax-case fmt (quote bracket)
                      [(bracket . fmt-tail) #'fmt-tail]
                      [_ fmt]))])
      (make-prty fmt (prtys-size prtys) prtys))))

; Order of evaluation is important here to make sure that "#n#" numbers
; get assigned in order and that the "#n=" assignment comes before any
; "#n# references.

(define mk-prty-list-help
  (lambda (x lev len fmt-tail)
    (define (fmt-next fmt-tail)
      (syntax-case fmt-tail (fill)
        [() (values 'x '())]
        [(fill tab fmt dots)
         (values #'fmt fmt-tail)]
        [(tab fmt dots)
         (and (tab? #'tab) (dots? #'dots))
         (values #'fmt fmt-tail)]
        [(fmt tab dots)
         (and (tab? #'tab) (dots? #'dots))
         (values #'fmt fmt-tail)]
        [(tab fmt . fmt-tail)
         (tab? #'tab)
         (values #'fmt #'fmt-tail)]
        [(fmt dots)
         (dots? #'dots)
         (values #'fmt fmt-tail)]
        [(fmt . fmt-tail)
         (values #'fmt #'fmt-tail)]))
    (if (or (limit? lev) (limit? len))
        (list (make-prty '() 3 "..."))
        (let-values ([(fmt fmt-tail) (fmt-next fmt-tail)])
          (let ([first (mk-prty (car x) (decr lev) len fmt)])
            (cons first
                  (let mk-rest ([x (cdr x)] [n (decr len)] [fmt-tail fmt-tail])
                     (cond
                       [(null? x) '()]
                       [(and (pair? x)
                             (not (and graph-env (graph-env 'tag? x)))
                             (not (and (eq? (car x) 'unquote)
                                       (pair? (cdr x))
                                       (null? (cddr x)))))
                        (if (limit? n)
                            (list (make-prty '() 3 "..."))
                            (let-values ([(fmt fmt-tail) (fmt-next fmt-tail)])
                              (let ([next (mk-prty (car x) (decr lev) len fmt)])
                                (cons next (mk-rest (cdr x) (decr n) fmt-tail)))))]
                       [else
                        (list (make-prty '() 1 ".")
                              (mk-prty x (decr lev) len 'x))]))))))))

(define pretty
  ; p   = prty object
  ; pps = pending parens
  (lambda (p pps)
    (let* ([len (prty-len p)] [oneline (<= (+ len pps) room)])
      (pretty-help p pps oneline))))

(define pretty-help
  (lambda (p pps oneline)
    (let ([obj (prty-obj p)] [fmt (prty-fmt p)])
      (cond
         [(string? obj) (pretty-write-prty p)]
         [(not (pair? fmt))
          (pretty-generic (car obj) (cdr obj) pps oneline)]
         [(eq? (car fmt) 'read-macro)
          (pretty-write-string (cadr fmt))
          (pretty obj pps)]
         [else
          (let* ([b? (and (eq? (car fmt) 'bracket) (print-brackets))]
                 [fmt (if (eq? (car fmt) 'bracket) (cdr fmt) fmt)])
            (pretty-write-char (if b? lbrack lparen))
            (let ([start col] [pps (+ pps 1)])
              (cond
                [(eq? (car fmt) 'fill)
                 (pretty-fill obj start pps oneline (tab-amount (cadr fmt)) #f)]
                [else
                 (pretty (car obj) (if (null? (cdr obj)) pps 0))
                 (pretty-tail (cdr obj) (cdr fmt) start pps oneline)]))
            (pretty-write-char (if b? rbrack rparen)))]))))

(define pretty-fill
  (lambda (obj start pps oneline tab space?)
   ; recompute room so that first of multiple lines of fill is treated like
   ; the remaining lines, possibly exceeding "one-line-limit" for that line
    (when (> (apply + (map prty-len obj)) (* 2 room))
      (set! room
        (min (- (pretty-line-length) start)
             (pretty-one-line-limit))))
    (let f ([obj obj] [space? space?])
      (let* ([p (car obj)] [len (prty-len p)])
        (cond
          [(or oneline
               (fx<= (fx+ len
                          (if (null? (cdr obj)) pps 0)
                          (if space? 1 0))
                     room))
           (when space? (pretty-write-char #\space))
           (pretty-help p (if (null? (cdr obj)) pps 0) #t)
           (unless (null? (cdr obj)) (f (cdr obj) #t))]
          [space?
           (pretty-tab (+ tab start) #f)
           (f obj #f)]
          [else
           (pretty-help p (if (null? (cdr obj)) pps 0) #f)
           (unless (null? (cdr obj))
             (pretty-tab (+ tab start) #f)
             (f (cdr obj) #f))])))))

(define pretty-generic
   (lambda (fcn args pps oneline)
      (pretty-write-char lparen)
      (let ((n (prty-len fcn)) (start col) (pps (+ pps 1)) (nargs (length args)))
         (cond
            ((fx= nargs 0) (pretty fcn pps))
            ((fx>= nargs 20) ; probably not a procedure call
             (pretty-fill (cons fcn args) start pps oneline 0 #f))
            ((<= n (+ si 2))
             (pretty-write-prty fcn)
             (pretty-write-char #\space)
             (if (fx>= nargs 5)
                 (pretty-fill args start pps oneline (+ n 1) #f)
                 (let f ((l args))
                    (if (null? (cdr l))
                        (pretty (car l) pps)
                        (begin
                           (pretty (car l) 0)
                           (pretty-tab (1+ (+ start n)) oneline)
                           (f (cdr l)))))))
            (else
             (pretty fcn 0)
             (if (fx>= nargs 5)
                 (pretty-fill args start pps oneline si #t)
                 (let f ((l args))
                    (pretty-tab (+ start si) oneline)
                    (if (null? (cdr l))
                        (pretty (car l) pps)
                        (begin
                           (pretty (car l) 0)
                           (f (cdr l)))))))))
      (pretty-write-char rparen)))

(define pretty-tail
   (lambda (obj fmt start pps oneline)
      (cond
         ((null? obj)
          ; ran out of object, just ignore remaining format
          (void))
         ((null? fmt)
          ; out of format, try to do something reasonable
          (pretty-tab (+ si start) oneline)
          (pretty (car obj) (if (null? (cdr obj)) pps 0))
          (pretty-tail (cdr obj) '() start pps oneline))
         ((tab-amount (car fmt)) =>
          (lambda (tab)
             (cond
                ((null? (cdr fmt)) ; shouldn't happen
                 ; tab at end of format, treat as null format
                 (pretty-tail obj '() start pps oneline))
                ((dots? (cadr fmt))
                 (let f ((l obj))
                    (pretty-tab (+ tab start) oneline)
                    (if (null? (cdr l))
                        (pretty (car l) pps)
                        (begin
                           (pretty (car l) 0)
                           (f (cdr l))))))
                ((and (not (null? (cddr fmt))) (dots? (caddr fmt)))
                 (let f ((l obj))
                    (pretty-tab (+ tab start) oneline)
                    (if (null? (cdr l))
                        (pretty (car l) pps)
                        (begin
                           (pretty (car l) 0)
                           (f (cdr l))))))
                (else
                 (pretty-tab (+ tab start) oneline)
                 (pretty (car obj) (if (null? (cdr obj)) pps 0))
                 (pretty-tail (cdr obj) (cddr fmt) start pps oneline)))))
         ((dots? (car fmt)) ; no newlines...
          (pretty-write-char #\space)
          (pretty (car obj) (if (null? (cdr obj)) pps 0))
          (pretty-tail (cdr obj) fmt start pps oneline))
         ((eq? (car fmt) 'fill)
          (pretty-fill obj start pps oneline (tab-amount (cadr fmt)) #t))
         (else
          (pretty-write-char #\space)
          (pretty (car obj) (if (null? (cdr obj)) pps 0))
          (pretty-tail (cdr obj) (cdr fmt) start pps oneline)))))

(define pretty-write-char
   (lambda (c)
      (write-char c port)
      (set! col (1+ col))
      (set! room (1- room))))

(define pretty-write-string
   (lambda (s)
      (let ((n (string-length s)))
         (display-string s port)
         (set! col (+ col n))
         (set! room (- room n)))))

(define pretty-tab
   (lambda (n oneline)
      (if oneline
          (pretty-write-char #\space)
          (begin
             (check-line-maximum)
             (newline port)
             (do ((i n (1- i))) ((zero? i)) (write-char #\space port))
             (set! col n)
             (set! room
                   (min (- (pretty-line-length) n)
                        (pretty-one-line-limit)))))))

(define pretty-write-prty
   (lambda (p)
      (let f ((o (prty-obj p)) (fmt (prty-fmt p)))
         (cond
            ((string? o) (display-string o port))
            ((and (pair? fmt) (eq? (car fmt) 'read-macro))
             (display-string (cadr fmt) port)
             (f (prty-obj o) (prty-fmt o)))
            (else
             (begin
                (write-char lparen port)
                (f (prty-obj (car o)) (prty-fmt (car o)))
                (for-each
                   (lambda (x)
                      (write-char #\space port)
                      (f (prty-obj x) (prty-fmt x)))
                   (cdr o))
                (write-char rparen port)))))
      (set! col (+ col (prty-len p)))
      (set! room (- room (prty-len p)))))

(set! pretty-print
   (case-lambda
      [(x) (pretty-print x (current-output-port))]
      [(x p)
       (unless (and (output-port? p) (textual-port? p))
          ($oops 'pretty-print "~s is not a textual output port" p))
       (let ([lev (print-level)]
             [len (print-length)]
             [indent (pretty-initial-indent)])
          (fluid-let ([si (pretty-standard-indent)]
                      [lines (pretty-maximum-lines)]
                      [col indent]
                      [room (min (- (pretty-line-length) indent)
                                 (pretty-one-line-limit))]
                      [port p]
                      [pretty-string-output-port (open-output-string)]
                      [graph-env ($make-graph-env 'pretty-print x lev len)])
             (check-line-maximum)
             (pretty (mk-prty x lev len 'x) 0)
             (newline port)))]))

(set-who! pretty-file
  (lambda (in out)
    (unless (string? in) ($oops who "~s is not a string" in))
    (unless (string? out) ($oops who "~s is not a string" out))
    (let ([i ($open-file-input-port who in (file-options)
               (buffer-mode block) (current-transcoder))]
          [o ($open-file-output-port who out (file-options replace)
               (buffer-mode block) (current-transcoder))])
      (on-reset
        (begin
          (close-input-port i)
          (delete-file out #f))
        (on-reset
          (close-output-port o)
          (let loop ()
            (let ([x (read i)])
              (unless (eof-object? x)
                (pretty-print x o)
                (newline o)
                (loop))))
          (close-input-port i)
          (close-output-port o))))))

(set! pretty-format
  (case-lambda
    [(key)
     (unless (symbol? key)
       ($oops 'pretty-format "~s is not a symbol" key))
     ($sgetprop key '*pretty-format* #f)]
    [(key fmt)
     (unless (symbol? key)
       ($oops 'pretty-format "~s is not a symbol" key))
     (unless (or (eq? fmt #f) (fmt? fmt))
       ($oops 'pretty-format "invalid format ~s" fmt))
     (with-tc-mutex
       (if (eq? fmt #f)
           ($sremprop key '*pretty-format*)
           ($sputprop key '*pretty-format* fmt)))]))

(pretty-format 'alias '(_ x y))
(pretty-format 'and '(_ e 4 ...))
(pretty-format 'assertion-violation '(_ who #f arg ...))
(pretty-format 'assertion-violationf '(_ who #f arg ...))
(pretty-format 'begin '(_ #f e ...))
(pretty-format 'case '(_ exp #f [bracket (fill 0 k ...) 0 e ...] ...))
(pretty-format 'case-lambda '(_ #f [bracket (fill 0 x ...) 0 e ...] ...))
(pretty-format 'cond '(_ #f (alt [bracket test '=> 0 exp] [bracket test 0 exp ...]) ...))
(pretty-format 'critical-section '(_ #f e ...))
(pretty-format 'datum '(_ x))
(pretty-format 'define '(_ (fill 0 x ...) #f e ...))
(pretty-format 'define-enumeration `(_ x #f ...))
(pretty-format 'define-property '(_ x #f e ...))
(pretty-format 'define-record
  '(alt (_ var (x 0 ...))
        (_ var (x 0 ...) #f ([bracket x y] 0 ...))
        (_ var (x 0 ...) #f ([bracket x y] 0 ...) #f ([bracket x y ...] ...))
        (_ var var (x 0 ...))
        (_ var var (x 0 ...) #f ([bracket x y] 0 ...))
        (_ var var (x 0 ...) #f ([bracket x y] 0 ...) #f ([bracket x y ...] ...))))
; if we had tail alts:
#;(pretty-format 'define-record
  `(alt (_ var (x 0 ...) . (alt () (#f ([bracket x y] 0 ...) . (alt () (#f ([bracket x y ...] ...))))))
        (_ var var (x 0 ...) . (alt () (#f ([bracket x y] 0 ...) . (alt () (#f ([bracket x y ...] ...))))))))
(pretty-format 'define-record-type '(_ x #f ...))
(pretty-format 'define-structure '(_ (x 0 ...) #f ([bracket x 0 ...] 0 ...)))
(pretty-format 'define-syntax '(_ (x ...) #f e ...))
(pretty-format 'define-values '(_ (fill 0 x ...) #f e ...))
(pretty-format 'delay '(_ exp))
(pretty-format 'exclusive-cond '(_ #f (alt [bracket test '=> 0 exp] [bracket test 0 exp ...]) ...))
(pretty-format 'do '(_ ([bracket x ...] 0 ...) 3 (e1 0 ...) #f e ...))
(pretty-format 'endianness '(_ x))
(pretty-format 'error '(_ who #f arg ...))
(pretty-format 'errorf '(_ who #f arg ...))
(pretty-format 'eval-when '(_ (k ...) #f e ...))
(pretty-format 'extend-syntax '(_ (keys 0 ...) #f [bracket e1 0 e2 ...] ...))
(pretty-format 'fluid-let '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'fluid-let-syntax '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'foreign-callable
  '(alt (foreign-callable #f proc #f (x 0 ...) #f y)
        (foreign-callable conv #f proc #f (x 0 ...) #f y)))
(pretty-format 'foreign-procedure
  '(alt (foreign-procedure entry #f (x 0 ...) #f y)
        (foreign-procedure conv entry #f (x 0 ...) #f y)))
(pretty-format 'guard
  '(_ (_ #f (alt [bracket test '=> 0 exp] [bracket test 0 exp ...]) ...)
     #f e ...))
(pretty-format 'identifier-syntax
  '(alt (identifier-syntax #f [bracket x x] #f [bracket x x])
        (identifier-syntax x)))
(pretty-format 'if '(_ exp 3 exp ...))
(pretty-format 'lambda '(_ (fill 0 x ...) #f e ...))
(pretty-format 'let
  '(alt (let ([bracket x e] 0 ...) #f e #f e ...)
        (let var ([bracket x e] 0 ...) #f e #f e ...)))
(pretty-format 'let* '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'letrec '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'letrec* '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'let-values '(_ ([bracket (x ...) 0 e] 0 ...) #f e ...))
(pretty-format 'let*-values '(_ ([bracket (x ...) 0 e] 0 ...) #f e ...))
(pretty-format 'let-syntax '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'letrec-syntax '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'library '(_ (name ...) #f e ...))
(pretty-format 'meta '(meta)) ; meta is handled as a special case
(pretty-format 'meta-cond '(_ #f (alt [bracket test '=> 0 exp] [bracket test 0 exp ...]) ...))
(pretty-format 'module
  '(alt (module (fill 0 (alt (fill 0 x ...) x) ...) #f e ...)
        (module var (fill 0 (alt (fill 0 x ...) x) ...) #f e ...)))
(pretty-format 'or '(_ exp 3 ...))
(pretty-format 'parameterize '(_ ([bracket x 0 e] 0 ...) #f e ...))
(pretty-format 'syntax-case '(_ exp (fill 0 lit ...) #f [bracket pat 0 e ...] ...))
(pretty-format 'syntax-rules '(_ (fill 0 lit ...) #f [bracket pat 0 e ...] ...))
(pretty-format 'rec '(_ id #f e))
(pretty-format 'record-case '(_ exp #f [bracket tag (x 0 ...) 0 exp ...] ...))
(pretty-format 'set! '(_ id #f e ...))
(pretty-format 'trace-case-lambda '(_ name #f [bracket (fill 0 x ...) 0 e ...] ...))
(pretty-format 'trace-define '(_ (fill 0 x ...) #f e ...))
(pretty-format 'trace-define-syntax '(_ (x ...) #f e ...))
(pretty-format 'trace-do '(_ ([bracket x ...] 0 ...) 3 (e1 0 ...) #f e ...))
(pretty-format 'trace-lambda '(_ name (fill 0 x ...) #f e ...))
(pretty-format 'trace-let '(_ name ([bracket x e] 0 ...) #f e ...))
(pretty-format 'unless '(_ test #f e ...))
(pretty-format 'warning '(_ who #f arg ...))
(pretty-format 'warningf '(_ who #f arg ...))
(pretty-format 'when '(_ test #f e ...))
(pretty-format 'with '(_ ([bracket x e] 0 ...) #f e ...))
(pretty-format 'with-implicit '(_ (x 0 ...) #f e ...))
(pretty-format 'with-interrupts-disabled '(_ #f e ...))
(pretty-format 'with-mutex '(_ x #f e ...))
(pretty-format 'with-syntax '(_ ([bracket x e] 0 ...) #f e ...))

(pretty-format 'quasiquote '(read-macro "`" x))
(pretty-format 'quasisyntax '(read-macro "#`" x))
(pretty-format 'quote '(read-macro "'" x))
(pretty-format 'syntax '(read-macro "#'" x))
(pretty-format 'unquote '(read-macro "," x))
(pretty-format 'unquote-splicing '(read-macro ",@" x))
(pretty-format 'unsyntax '(read-macro "#," x))
(pretty-format 'unsyntax-splicing '(read-macro "#,@" x))
(pretty-format '$primitive '(read-macro "#%" x))

; ftypes
(pretty-format 'define-ftype '(_ x #f ...))
(pretty-format 'struct '(_ #f [bracket x ...] ...))
(pretty-format 'union '(_ #f [bracket x ...] ...))
(pretty-format 'array '(_ n #f ...))
(pretty-format 'bits '(_ #f [bracket x ...] ...))
(pretty-format 'endian '(_ x #f ...))
(pretty-format 'packed '(_ #f ...))
(pretty-format 'unpacked '(_ #f ...))

; support for things that aren't built in...
(pretty-format 'match '(_ x #f [bracket e 0 ...] ...))

(record-writer (type-descriptor prty)
  (lambda (x p wr)
    (display "#<prty " p)
    (wr (prty-fmt x) p)
    (write-char #\space p)
    (wr (prty-len x) p)
    (write-char #\space p)
    (wr (prty-obj x) p)
    (write-char #\> p)))
)
)
