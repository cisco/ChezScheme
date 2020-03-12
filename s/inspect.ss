;;; inspect.ss
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

;;; todo

; ---be sensitive to system mode
; ---argument names for code objects
; ---nesting level numbers for all variables
;    (sort variable displays by nesting and position)
; ---add "loop" variable type
; ---keep track of loop names?
; ---information about foreign procedures
; ---distinguish between user and compiler gensym variables?
;    (right now both are stripped)
; ---disassembler
; ---port info should include file descriptor, perhaps provide access
;    location in file

(begin
(let ()

(define-syntax make-dispatch-table
  (lambda (x)
    (syntax-case x ()
      [(_ [key message (ids e1 e2 ...) ...] ...)
       (and (andmap (lambda (x)
                      (or (string? x)
                          (and (pair? x) (string? (car x)) (string? (cdr x)))))
                    (datum (key ...)))
            (andmap string? (datum (message ...))))
       #'`([key message
                ,(case-lambda
                   (ids e1 e2 ...)
                   ...
                   (l (invalid-command)))]
           ...)])))

(define-record-type sfile
  (fields (immutable path) (immutable port) (mutable line) (mutable line-valid?))
  (nongenerative)
  (sealed #t))

(define-threaded source-files '())

(define find-source-file
  (lambda (path line)
    (define path=?
     ; trivial definition for now
      (lambda (p1 p2)
        (string=? p1 p2)))
    (let f ((ls source-files))
      (if (null? ls)
          (guard (c [#t #f])
            (let ((line (or line 1)))
              (set! source-files
                (cons (make-sfile path (open-input-file path)
                        line
                        (= line 1))
                      source-files)))
            #t)
          (if (path=? path (sfile-path (car ls)))
              (let ((sf (car ls)))
                (when line
                  (unless (= line (sfile-line sf))
                    (sfile-line-valid?-set! sf #f)
                    (sfile-line-set! sf line)))
                (set! source-files
                  (cons sf (remq sf source-files)))
                #t)
              (f (cdr ls)))))))

(define open-source-file
  (case-lambda
    [(path) (open-source-file path #f)]
    [(path line)
     (or (if ($fixed-path? path)
             (find-source-file path line)
             (let ([dir* (append (source-directories) (map car (library-directories)))])
               (let pathloop ([path path])
                 (let dirloop ([dir* dir*])
                   (if (null? dir*)
                       (let ([rest (path-rest path)])
                         (and (not (string=? rest path))
                              (pathloop rest)))
                       (or (find-source-file
                             (let* ((dir (car dir*)) (n (string-length dir)))
                               (format (if (and (fx> n 0)
                                                (directory-separator?
                                                  (string-ref dir (fx- n 1))))
                                           "~a~a"
                                           "~a/~a")
                                 dir path))
                             line)
                           (dirloop (cdr dir*))))))))
         (inspect-error "Cannot open ~a" path))]))

(define open-recorded-source-file
  (lambda (object)
    (call-with-values
      (lambda () (object 'source-path))
      (case-lambda
        [() (inspect-error "Source file unknown.")]
        [(path pos)
         (inspect-error
           "Cannot locate (unmodified) source file ~a.~%Try changing source-directories parameter.~%Source is at character ~s."
           path pos)]
        [(path line char)
         (if (find-source-file path
               (max (- line (quotient lines-to-list 2)) 1))
             (show "line ~d, character ~d of ~a" line char path)
             (inspect-error "Cannot open ~a" path))]))))

(define close-source-file
  (lambda (sf)
    (close-input-port (sfile-port sf))))

(define lines-to-list 10)

(module (list-source-file)
(define base10-length
  (lambda (n)
    (cond
      [(fx< n 10) 1]
      [(fx< n 100) 2]
      [(fx< n 1000) 3]
      [(fx< n 10000) 4]
      [else (+ 4 (base10-length (quotient n 10000)))])))

(define list-source-file
  (case-lambda
    [() (list-source-file #f #f)]
    [(line) (list-source-file line #f)]
    [(line count)
     (when (null? source-files)
       (inspect-error "No source file open."))
     (let* ((sf (car source-files))
            (ip (sfile-port sf)))
       (when line (require (fixnum? line)))
       (when count (require (and (fixnum? count) (fx> count 0))))
       (let* ((line (cond [(not line) (sfile-line sf)]
                          [(fx> line 0) line]
                          [else (max (+ (sfile-line sf) line (- lines-to-list))
                                     1)]))
              (count (if count
                         (begin (set! lines-to-list count) count)
                         lines-to-list)))
         (let f ((new-line
                  (if (and (sfile-line-valid? sf) (fx>= line (sfile-line sf)))
                      (begin
                        (sfile-line-valid?-set! sf #f)
                        (sfile-line sf))
                      (begin
                        (sfile-line-valid?-set! sf #f)
                        (file-position ip 0)
                        1))))
           (unless (fx= new-line line)
             (let ((c (read-char ip)))
               (cond
                 [(eof-object? c)
                  (inspect-error "Not that many lines in ~a." (sfile-path sf))]
                 [(char=? c #\newline) (f (fx+ new-line 1))]
                 [else (f new-line)]))))
         (let ((line-chars (base10-length (+ line count -1))))
           (let f ((line line) (count count))
             (if (fx= count 0)
                 (begin
                   (sfile-line-set! sf line)
                   (sfile-line-valid?-set! sf #t))
                 (let ((c (read-char ip)))
                   (if (eof-object? c)
                       (fprintf (console-output-port) "*** end of file ***~%")
                       (begin
                         (do ((n (base10-length line) (fx+ n 1)))
                             ((fx= n line-chars))
                           (write-char #\space (console-output-port)))
                         (fprintf (console-output-port) "~d: " line)
                         (do ((c c (read-char ip)))
                             ((or (eof-object? c) (char=? c #\newline))
                              (newline (console-output-port)))
                           (write-char c (console-output-port)))
                         (f (fx+ line 1) (fx- count 1))))))))))]))
)

(define (waiter-read)
  (parameterize ([waiter-prompt-string ""])
    ((waiter-prompt-and-read) 1)))

(define show
   (lambda (s . args)
      (apply fprintf (console-output-port) s args)
      (newline (console-output-port))))

(define inspect-error
  (lambda (s . args)
    (apply show s args)
    (reset)))

(define invalid-command
  (lambda ()
    (inspect-error "Invalid command or argument.  Type ? for options.")))

(define invalid-movement
  (lambda ()
    (inspect-error "Invalid movement.")))

(define line-indent "  ")

(define prompt-line-limit 65)

(define display-line-limit 80)

(define descrip-limit 25)

(define-threaded marks)

(define-threaded current-state)

(define-record-type state
  (fields (immutable object) (immutable level) (immutable position) (immutable link) (mutable find-next))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (case-lambda
        [(object) (new object 0 #f #f #f)]
        [(object level position link) (new object level position link #f)]))))

(define object (lambda () (state-object current-state)))

(define level (lambda () (state-level current-state)))

(define position (lambda () (state-position current-state)))

(define type?
   (lambda (flag x)
      (eq? (x 'type) flag)))

(define default-mark (void))

(define make-mark
   (lambda (m)
      (if (string? m)
          (string->symbol m)
          m)))

(define put-mark
   (lambda (m)
      (let ([a (eq-hashtable-cell marks m #f)])
         (set-cdr! a current-state))))

(define get-mark
   (lambda (m)
      (eq-hashtable-ref marks m #f)))

(define to-mark
   (lambda (m)
      (let ([s (get-mark m)])
         (unless s (invalid-movement))
         (put-mark default-mark)
         (set! current-state s))))

(define down
   (lambda (x pos)
      (set! current-state
          (make-state (if (eq? (x 'type) 'variable) (x 'ref) x)
                      (+ (level) 1)
                      pos
                      current-state))))

(define up
   (lambda ()
      (set! current-state (state-link current-state))
      (unless current-state (invalid-movement))))

(define display-links
   (lambda (n)
      (let loop ([i 0] [x (object)])
         (unless (= i n)
            (when (type? 'continuation x)
               (label-line-display x i)
               (loop (+ i 1) (x 'link)))))))

(define display-refs
   (lambda (n)
      (let ([x (object)])
         (let loop ([i 0])
            (unless (= i n)
               (label-line-display (x 'ref i) i)
               (loop (+ i 1)))))))

(define display-variable-refs
   (lambda (n)
      (let ([x (object)])
         (if ((x 'code) 'info)
             (let loop ([i 0])
                (unless (= i n)
                   (variable-line-display (x 'ref i) i)
                   (loop (+ i 1))))
             (display-refs n)))))

(define display-list
  (lambda (n)
    (let ((x (object)))
      (if (or (type? 'pair (x 'cdr))
              (and (type? 'simple (x 'cdr)) (null? ((x 'cdr) 'value))))
          (let loop ([i 0] [x x])
            (if (and (< i n) (type? 'pair x))
                (begin
                  (label-line-display (x 'car) i)
                  (loop (+ i 1) (x 'cdr)))
                (unless (and (type? 'simple x) (null? (x 'value)))
                  (name-line-display x "tail"))))
          (begin
            (name-line-display (x 'car) "car")
            (name-line-display (x 'cdr) "cdr"))))))

(define charschemecode
   (lambda (x)
      (let ([x (format "~s" x)])
         (format "~a~a" x (spaces (- 11 (string-length x)))))))

(define unicodehexcode
  (lambda (x)
    (format "~6,'0x " (char->integer x))))

(define asciihexcode
  (lambda (x)
    (let ([n (char->integer x)])
      (if (>= n 256)
          "-- "
          (format "~2,'0x " n)))))

(define display-chars
   (lambda (n former no/line)
      (let ([x (object)])
         (let loop1 ([i 0])
            (unless (= i n)
               (let ([label (format "~a~d: " line-indent i)])
                  (let loop2 ([j 0] [i i] [strings '()])
                     (if (or (= j no/line) (= i n))
                         (begin
                            (show "~a~a~a"
                                  label
                                  (spaces (- 6 (string-length label)))
                                  (apply string-append (reverse strings)))
                            (loop1 i))
                         (loop2 (+ j 1)
                                (+ i 1)
                                (cons (former ((x 'ref i) 'value))
                                      strings))))))))))

(define label-line-display
   (lambda (x n)
      (let ([label (format "~a~d: " line-indent n)])
         (show "~a~a"
               label
               (form x (string-length label) display-line-limit)))))

(define name-label-line-display
   (lambda (x name n)
      (let ([label (format "~a~d. ~a:" line-indent n name)])
         (let ([label (format "~a~a"
                         label
                         (spaces (- descrip-limit (string-length label))))])
            (show "~a~a"
                  label
                  (form x (string-length label) display-line-limit))))))

(define name-line-display
   (lambda (x name)
      (let ([label (format "~a~a:" line-indent name)])
         (let ([label (format "~a~a"
                         label
                         (spaces (- descrip-limit (string-length label))))])
            (show "~a~a"
                  label
                  (form x (string-length label) display-line-limit))))))

(define variable-line-display
   (lambda (x n)
      (if (x 'name)
          (name-label-line-display (x 'ref) (x 'name) n)
          (label-line-display (x 'ref) n))))

(define ref-list
   (lambda (n)
      (unless (and (fixnum? n) (>= n 0)) (invalid-movement))
      (let ref ([i n] [x (object)])
         (cond
            [(not (type? 'pair x)) (invalid-movement)]
            [(= i 0) (down (x 'car) n)]
            [else (ref (- i 1) (x 'cdr))]))))

(define ref
  (lambda (n)
    (unless (and (fixnum? n) (< -1 n ((object) 'length)))
      (invalid-movement))
    (down ((object) 'ref n) n)))

(define set
  (lambda (n v)
    (unless (and (fixnum? n) (< -1 n ((object) 'length)))
      (invalid-movement))
    (let ([x ((object) 'ref n)])
      (unless (x 'assignable?)
        (inspect-error "~s is not assignable" (or (x 'name) 'unnamed)))
      (x 'set! v))))

(module (variable-ref variable-set)
  (define get-var-obj
    (lambda (sym)
      (let ([n ((object) 'length)])
        (let loop ([i 0])
          (if (fx= i n)
              (invalid-movement)
              (let ([x ((object) 'ref i)])
                (if (let ([name (x 'name)])
                      (and (symbol? name)
                           (string=?
                             (symbol->string name)
                             (symbol->string sym))))
                    (values x i)
                    (loop (fx+ i 1)))))))))
  (define variable-ref
    (lambda (x)
      (if (symbol? x)
          (with-values (get-var-obj x) down)
          (ref x))))
  (define variable-set
    (lambda (x val)
      (if (symbol? x)
          (with-values (get-var-obj x)
            (lambda (var-obj i)
              (unless (var-obj 'assignable?) (inspect-error "~s is not assignable" x))
              (var-obj 'set! val)))
          (set x val)))))

(define move
   (lambda (n)
      (require (position))
      (let ([n (+ n (position))])
         (up)
         (case ((object) 'type)
            [(pair) (ref-list n)]
            [(continuation procedure vector fxvector bytevector string record
              ftype-struct ftype-union ftype-array ftype-bits)
             (ref n)]
            [else (invalid-movement)]))))

(define require
   (lambda (x)
      (unless x (invalid-command))))

(define range-check
   (case-lambda
      [(n) (require (and (fixnum? n) (fx<= 0 n)))]
      [(n max) (require (and (fixnum? n) (fx<= 0 n max)))]
      [(min n max) (require (and (fixnum? n) (fx<= min n max)))]))

(define display-one-option
   (lambda (key message)
      (let ([s (if (pair? key) (format "~a(~a)" (car key) (cdr key)) key)])
         (show "   ~a ~a ~a"
               s
               (make-string (max (- 20 (string-length s)) 0) #\.)
               message))))

(define display-options
   (lambda (table generic?)
      (show "")
      (for-each display-one-option (map car table) (map cadr table))
      (unless generic? (display-one-option "??" "display more options"))
      (show "")))

(define select-dispatch-table
   (lambda ()
      (case ((object) 'type)
         [(pair) pair-dispatch-table]
         [(symbol) (if (eq? (subset-mode) 'system)
                       system-symbol-dispatch-table
                       symbol-dispatch-table)]
         [(vector) vector-dispatch-table]
         [(fxvector) fxvector-dispatch-table]
         [(bytevector) bytevector-dispatch-table]
         [(record) record-dispatch-table]
         [(string) string-dispatch-table]
         [(box) box-dispatch-table]
         [(continuation) continuation-dispatch-table]
         [(procedure) procedure-dispatch-table]
         [(code) code-dispatch-table]
         [(port) port-dispatch-table]
         [(simple)
          (let ([x ((object) 'value)])
             (cond
                [(char? x) char-dispatch-table]
                [else empty-dispatch-table]))]
         [(tlc) tlc-dispatch-table]
         [(ftype-struct) ftype-struct-dispatch-table]
         [(ftype-union) ftype-union-dispatch-table]
         [(ftype-array) ftype-array-dispatch-table]
         [(ftype-*) ftype-pointer-dispatch-table]
         [(ftype-bits) ftype-bits-dispatch-table]
         [(ftype-base) ftype-pointer-dispatch-table]
         [(ftype-function) ftype-function-dispatch-table]
         [else empty-dispatch-table])))

(define inspector-read
   (lambda (ip)
      (let* ([ip (console-input-port)] [c (read-char ip)])
         (cond
            [(eof-object? c)
             (newline (console-output-port))
             '("quit")]
            [(char=? c #\newline)
             (set-port-bol! (console-output-port) #t)
             '()]
            [(char-whitespace? c)
             (inspector-read ip)]
            [else
             (unread-char c ip)
             (let ([first (inspector-read-command ip)])
                (cons first (inspector-read-tail ip)))]))))

(define inspector-read-command
   (lambda (ip)
      (let ([p (open-output-string)])
         (let read-letters ()
            (let ([c (peek-char ip)])
               (if (and (char? c)
                        (not (char-numeric? c))
                        (not (char-whitespace? c)))
                   (begin (read-char ip)
                          (write-char c p)
                          (read-letters))
                   (get-output-string p)))))))

(define inspector-read-tail
   (lambda (ip)
      (let ([c (peek-char ip)])
         (cond
            [(char=? c #\newline)
             (read-char ip)
             (set-port-bol! (console-output-port) #t)
             '()]
            [(or (char-whitespace? c)    ; [(
                 (memv c '(#\) #\])))
             (read-char ip)
             (inspector-read-tail ip)]
            [else
             (let ([x (read ip)])
                (cons x (inspector-read-tail ip)))]))))

(define dispatch
   (lambda (c t)
      (let ([handler (or (search-dispatch-table (car c) t)
                         (search-dispatch-table (car c)
                                                generic-dispatch-table))])
         (if handler
             (apply handler (cdr c))
             (invalid-command)))))

(define search-dispatch-table
   (lambda (s t)
      (and (not (null? t))
           (let ([first (car t)])
              (let ([key (car first)])
                 (if (if (string? key)
                         (string=? key s)
                         (or (string=? (car key) s)
                             (string=? (cdr key) s)))
                     (caddr first)
                     (search-dispatch-table s (cdr t))))))))

(define spaces
   (lambda (n)
      (if (> n 0)
          (make-string n #\space)
          "")))

(define write-to-string
   (lambda (x)
      (let ([p (open-output-string)])
         (x 'write p)
         (get-output-string p))))

(define short-form-rec
   (lambda (x limit)
      (let try ([low 1]
                [high #f]
                [r (parameterize ([print-level 0] [print-length 0])
                      (write-to-string x))])
         (let ([mid (+ low (if high (quotient (- high low) 2) low))])
            (if (= mid low)
                r
                (let ([s (parameterize ([print-level mid] [print-length mid])
                            (write-to-string x))])
                   (cond
                      [(string=? s r) s]
                      [(> (string-length s) limit) (try low mid r)]
                      [else (try mid high s)])))))))

(define short-form-lambda
   ; x looks like "(lambda vars body)"
   ; print the "lambda" and all of the vars that fit
   (lambda (x limit)
      (let ([first (format "(lambda ~a "                                  ;)
                           (short-form-rec ((x 'cdr) 'car) (- limit 14)))])
         (let ([rest (short-form-rec ((x 'cdr) 'cdr)
                                     (- limit (string-length first)))])
            (if (and (> (string-length rest) 0)
                     (char=? (string-ref rest 0) #\())                    ;)
                 (string-append first (substring rest 1 (string-length rest)))
                 (short-form-rec x limit))))))

(define short-form
   (lambda (x limit)
      (case (x 'type)
         [(pair)
          (if (and (eq? ((x 'car) 'type) 'symbol)
                   (eq? ((x 'car) 'value) 'lambda)
                   (eq? ((x 'cdr) 'type) 'pair)
                   (eq? (((x 'cdr) 'cdr) 'type) 'pair))
              (short-form-lambda x limit)
              (short-form-rec x limit))]
         [(string)
          (let ([s (format "~s"
                    ; avoid passing format the whole of a large string
                     (let ([s (x 'value)])
                       (if (<= (string-length s) limit)
                           s
                           (substring s 0 limit))))])
            (if (<= (string-length s) limit)
                s
                (string-append
                  (substring s 0 (max (- limit 4) 1))
                  "...\"")))]
         [else (short-form-rec x limit)])))

(define form
  (lambda (x used limit)
    (short-form x (- limit used))))

(define inspector-prompt
   (lambda ()
      (let ([obj (form (object) 0 prompt-line-limit)])
         (fprintf (console-output-port)
                  "~a~a : "
                  obj
                  (spaces (- prompt-line-limit (string-length obj)))))))

(define outer-reset-handler ($make-thread-parameter values))

(define inspector
  (lambda (last-command)
    (inspector
      (let ([saved-state current-state])
        (parameterize ([reset-handler (call/cc
                                        (lambda (k)
                                          (rec f
                                            (lambda ()
                                              (clear-output-port (console-output-port))
                                              (set! current-state saved-state)
                                              (k f)))))])
          (let ([ip (console-input-port)])
            (clear-input-port ip)
            (inspector-prompt)
            (let ([cmd (let ([cmd (inspector-read ip)])
                         (cond
                           [(null? cmd)
                            (if (equal? (car last-command) "list")
                                '("list")
                                last-command)]
                           [(number? (car cmd)) (cons "ref" cmd)]
                           [else cmd]))])
              (cond
                [(equal? cmd '("?"))
                 (let ([t (select-dispatch-table)])
                   (if (null? t)
                       (display-options generic-dispatch-table #t)
                       (display-options t #f)))]
                [(equal? cmd '("??"))
                 (display-options generic-dispatch-table #t)]
                [else
                 (guard (c [#t (let ([op (console-output-port)])
                                 (fresh-line op)
                                 (display-condition c op)
                                 (newline op)
                                 (set! current-state saved-state))])
                   (dispatch cmd (select-dispatch-table)))])
              cmd)))))))

(define-syntax inspector-print
  (syntax-rules ()
    [(_ e)
     (call-with-values (lambda () e)
       (case-lambda
         [(x) (unless (eq? x (void)) (pretty-print x (console-output-port)))]
         [args (for-each (lambda (x) (pretty-print x (console-output-port))) args)]))]))

(module (inspector-find inspector-find-next)
  (define down-path
    (lambda (path)
      (assert (and (list? path) (>= (length path) 1)))
      (let f ([path path])
        (let ([x (car path)] [path (cdr path)])
          (if (null? path)
              (assert (eq? x ((object) 'value)))
              (begin
                (f path)
                (down ((object) 'make-me-a-child x) #f)))))))
  (define inspector-find
    (lambda (pred gen)
      (state-find-next-set! current-state (make-object-finder pred ((object) 'value) gen))
      (let ([path ((state-find-next current-state))])
        (unless path (inspect-error "Not found"))
        (down-path path))))
  (define inspector-find-next
    (lambda ()
      (let loop ([state current-state])
        (cond
          [(not state) (inspect-error "No current find.")]
          [(state-find-next state) =>
           (lambda (find-next)
             (let ([path (find-next)])
               (unless path (inspect-error "Not found"))
               (set! current-state state)
               (down-path path)))]
          [else (loop (state-link state))])))))

(define generic-dispatch-table
 (make-dispatch-table

  [("print" . "p")
   "pretty-print object"
   (()
    (newline (console-output-port))
    ((object) 'print (console-output-port))
    (newline (console-output-port)))]

  [("write" . "w")
   "write object"
   (()
    (newline (console-output-port))
    ((object) 'write (console-output-port))
    (newline (console-output-port))
    (newline (console-output-port)))]

  ["size"
   "recursively compute storage occupied by object"
   (() (fprintf (console-output-port) "~s\n" ((object) 'size (collect-maximum-generation))))
   ((g) 
    (require (or (and (fixnum? g) (fx<= 0 g (collect-maximum-generation))) (eq? g 'static)))
    (fprintf (console-output-port) "~s\n" ((object) 'size g)))]

  ["find"
   "find within object, given a predicate"
   (()
    (let ([x (waiter-read)])
      (unless (eof-object? x)
        (let ([x (eval x)])
          (unless (procedure? x) (inspect-error "~s is not a procedure" x))
          (inspector-find x (collect-maximum-generation))))))
   ((x)
    (let ([x (eval x)])
      (unless (procedure? x) (inspect-error "~s is not a procedure" x))
      (inspector-find x (collect-maximum-generation))))
   ((x g)
    (require (or (and (fixnum? g) (fx<= 0 g (collect-maximum-generation))) (eq? g 'static)))
    (let ([x (eval x)])
      (unless (procedure? x) (inspect-error "~s is not a procedure" x))
      (inspector-find x g)))]

  ["find-next"
   "repeat find"
   (()
    (inspector-find-next))]

  [("up" . "u")
   "return to [nth] previous level"
   (() (up))
   ((n)
    (range-check n)
    (let backup ([n n])
       (unless (= n 0)
          (up)
          (backup (- n 1)))))]

  [("top" . "t")
   "return to initial object"
   (()
    (let top ()
       (let ([next (state-link current-state)])
          (when next
             (set! current-state next)
             (top)))))]

  [("forward" . "f")
   "move to [nth] next expression"
   (() (move 1))
   ((n)
    (range-check n)
    (move n))]

  [("back" . "b")
   "move to [nth] previous expression"
   (() (move -1))
   ((n)
    (range-check n)
    (move (- n)))]

  ["=>"
   "send object to procedure"
   (()
    (let ([x (waiter-read)])
       (unless (eof-object? x)
          (let ([x (eval x)])
             (unless (procedure? x) (inspect-error "~s is not a procedure" x))
             (inspector-print (x ((object) 'value)))))))
   ((x)
    (let ([x (eval x)])
       (unless (procedure? x) (inspect-error "~s is not a procedure" x))
       (inspector-print (x ((object) 'value)))))]

  ["file"
   "switch to named source file"
   ((path)
    (unless (or (string? path) (symbol? path))
      (inspect-error "invalid path ~s" path))
    (open-source-file (if (symbol? path) (symbol->string path) path)))]

  ["list"
   "list the current source file [line [count]]"
   (() (list-source-file))
   ((n) (list-source-file n))
   ((n m) (list-source-file n m))]

  ["files"
   "show open files"
   (()
    (for-each
      (lambda (sf) (show "~a" (sfile-path sf)))
      source-files))]

  [("mark" . "m")
   "mark location [with symbolic mark]"
   (() (put-mark default-mark))
   ((m) (put-mark (make-mark m)))]

  [("goto" . "g")
   "go to marked location [mark]"
   (() (to-mark default-mark))
   ((m) (to-mark (make-mark m)))]

  [("new-cafe" . "n")
   "enter a new cafe"
   (()
    (newline (console-output-port))
    (new-cafe)
    (newline (console-output-port)))]

  [("quit" . "q")
   "exit inspector"
   (()
    (newline (console-output-port))
    (exit))]

  [("reset" . "r")
   "reset scheme"
   (()
    (newline (console-output-port))
    ((outer-reset-handler)))]

  [("abort" . "a")
   "abort scheme [with exit code n]"
   (()
    (newline (console-output-port))
    (abort))
   ((x)
    (newline (console-output-port))
    (abort x))]

  [("help" . "h")
   "help"
   (()
    (show "
     An overview of the current object is displayed as part of each
     prompt.  There are commands for displaying more of an object or
     inspecting its components.  \"?\" displays type-specific command
     options and \"??\" displays command options that are always
     available.  Some commands take parameters, which are entered
     following the command on the same line.  An empty command line
     repeats the previous command.  To perform more complex actions,
     enter the command \"n\", which creates a new top level with access
     to the usual Scheme environment.  The inspector is resumed upon
     exit from the new top level.  Enter \"quit\" (or end-of-file) to
     exit from the inspector.
"))]

))

(define empty-dispatch-table (make-dispatch-table))

(define pair-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display list length"
    (()
     (apply (lambda (type len)
               (case type
                  [(proper) (show "   proper list, length ~d" len)]
                  [(improper) (show "   improper list, length ~d" len)]
                  [(circular) (show "   circular list, length ~d" len)]))
            ((object) 'length)))]

   ["car"
    "inspect car of pair"
    (() (ref-list 0))]

   ["cdr"
    "inspect cdr of pair"
    (() (down ((object) 'cdr) #f))]

   [("ref" . "r")
    "inspect [nth] car"
    (() (ref-list 0))
    ((n) (ref-list n))]

   ["tail"
    "inspect [nth] cdr"
    (() (down ((object) 'cdr) #f))
    ((n)
     (range-check n)
     (let tail ([i n])
        (unless (= i 0)
           (unless (type? 'pair (object)) (invalid-movement))
           (down ((object) 'cdr) #f)
           (tail (- i 1)))))]

   [("show" . "s")
     "show [n] elements of list"
     (() (display-list (cadr ((object) 'length))))
     ((n)
      (range-check n)
      (display-list n))]

))

(define vector-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display vector length"
    (() (show "   ~d elements" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] element"
    (() (ref 0))
    ((n) (ref n))]

   [("show" . "s")
     "show [n] elements"
     (() (display-refs ((object) 'length)))
     ((n)
      (range-check n ((object) 'length))
      (display-refs n))]

))

(define fxvector-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display fxvector length"
    (() (show "   ~d elements" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] element"
    (() (ref 0))
    ((n) (ref n))]

   [("show" . "s")
     "show [n] elements"
     (() (display-refs ((object) 'length)))
     ((n)
      (range-check n ((object) 'length))
      (display-refs n))]

))

(define bytevector-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display bytevector length"
    (() (show "   ~d elements" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] element"
    (() (ref 0))
    ((n) (ref n))]

   [("show" . "s")
     "show [n] elements"
     (() (display-refs ((object) 'length)))
     ((n)
      (range-check n ((object) 'length))
      (display-refs n))]

))

(define ftype-struct-dispatch-table
 (make-dispatch-table
   ["fields"
    "inspect fields"
    (() (down ((object) 'fields) #f))]

   [("ref" . "r")
    "inspect named or nth element"
    (() (down ((object) 'ref 0) 0))
    ((f) (down ((object) 'ref f) (and (fixnum? f) f)))]

   ["set!"
    "set named element, if assignable"
    ((f)
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! f x)))))
    ((f v) ((object) 'set! f (eval v)))]

   ["ftype"
    "inspect the ftype"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
    "show contents of struct"
    (()
     (let ([fields (((object) 'fields) 'value)])
       (if (null? fields)
           (show "*** struct has no fields ***")
           (for-each
             (lambda (f i)
               (name-label-line-display
                 ((object) 'ref i)
                 f
                 i))
             fields
             (iota (length fields))))))]))

(define ftype-union-dispatch-table
 (make-dispatch-table
   ["fields"
    "inspect fields"
    (() (down ((object) 'fields) #f))]

   [("ref" . "r")
    "inspect named or nth element"
    (() (down ((object) 'ref 0) 0))
    ((f) (down ((object) 'ref f) (and (fixnum? f) f)))]

   ["set!"
    "set named element, if assignable"
    ((f)
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! f x)))))
    ((f v) ((object) 'set! f (eval v)))]

   ["ftype"
    "inspect the ftype"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
    "show contents of union"
    (()
     (let ([fields (((object) 'fields) 'value)])
       (if (null? fields)
           (show "*** union has no fields ***")
           (for-each
             (lambda (f i)
               (name-label-line-display
                 ((object) 'ref i)
                 f
                 i))
             fields
             (iota (length fields))))))]))

(define ftype-array-dispatch-table
 (make-dispatch-table
   [("length" . "l")
    "display array length"
    (() (show "   ~d elements" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] element"
    (() (ref 0))
    ((n) (ref n))]

   ["set!"
    "set [nth] element, if assignable"
    ((f)
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! f x)))))
    ((f v) ((object) 'set! f (eval v)))]

   ["ftype"
    "inspect the ftype"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
     "show [n] elements"
     (() (display-refs ((object) 'length)))
     ((n)
      (range-check n ((object) 'length))
      (display-refs n))]
   ))

(define ftype-pointer-dispatch-table
 (make-dispatch-table
   [("ref" . "r")
    "inspect target of pointer"
    (() (down ((object) 'ref) #f))
    ((n)
     (unless (memv n '(* 0)) (invalid-movement))
     (down ((object) 'ref) #f))]

   ["set!"
    "set target of pointer, if assignable"
    (()
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! x)))))
    ((v) ((object) 'set! (eval v)))]

   ["ftype"
    "inspect ftype of target"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
     "show the target"
     (() (label-line-display ((object) 'ref) 0))]
   ))

(define ftype-function-dispatch-table
 (make-dispatch-table
   ["name"
    "inspect foreign-function name"
    (() (down ((object) 'name) #f))]

   ["address"
    "inspect foreign-function address"
    (() (down ((object) 'address) #f))]

   ["ftype"
    "inspect ftype of target"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
     "show the target"
     (() (label-line-display ((object) 'name) 0)
         (label-line-display ((object) 'address) 1))]
   ))

(define ftype-bits-dispatch-table
 (make-dispatch-table
   ["fields"
    "inspect fields"
    (() (down ((object) 'fields) #f))]

   [("ref" . "r")
    "inspect named or nth element"
    (() (down ((object) 'ref 0) 0))
    ((f) (down ((object) 'ref f) (and (fixnum? f) f)))]

   ["set!"
    "set named element, if assignable"
    ((f)
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! f x)))))
    ((f v) ((object) 'set! f (eval v)))]

   ["ftype"
    "inspect the ftype"
    (() (down ((object) 'ftype) #f))]

   [("show" . "s")
    "show bit fields"
    (()
     (let ([fields (((object) 'fields) 'value)])
       (if (null? fields)
           (show "*** no fields ***")
           (for-each
             (lambda (f i)
               (name-label-line-display
                 ((object) 'ref i)
                 f
                 i))
             fields
             (iota (length fields))))))]))

(define record-dispatch-table
 (make-dispatch-table

   ["fields"
    "inspect fields"
    (() (down ((object) 'fields) #f))]

   ["name"
    "inspect record name"
    (() (down ((object) 'name) #f))]

   ["rtd"
    "inspect record-type descriptor"
    (() (down ((object) 'rtd) #f))]

   [("ref" . "r")
    "inspect named or nth element"
    ((f) (down ((object) 'ref f) (and (fixnum? f) f)))]

   ["set!"
    "set named element, if assignable"
    ((f)
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (let ((x (eval x)))
           ((object) 'set! f x)))))
    ((f v) ((object) 'set! f (eval v)))]

   [("show" . "s")
     "show contents of record"
    (()
     (when (and (eq? (subset-mode) 'system)
                (record-type-opaque? (((object) 'rtd) 'value)))
       (show "*** inspecting opaque record ***"))
     (let ([fields (((object) 'fields) 'value)])
       (if (null? fields)
           (show "*** record has no fields ***")
           (for-each
             (lambda (f i)
               (name-label-line-display
                 (if ((object) 'accessible? i)
                     ((object) 'ref i)
                     (inspect/object "*** inaccessible ***"))
                 f
                 i))
             fields
             (iota (length fields))))))]
))


(define string-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display string length"
    (() (show "   ~d characters" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] character"
    (() (ref 0))
    ((n) (ref n))]

   [("show" . "s")
     "show [n] characters"
     (() (display-chars ((object) 'length) charschemecode 5))
     ((n)
      (range-check n ((object) 'length))
      (display-chars n charschemecode 5))]

   ["unicode"
     "display [n] characters as hexadecimal unicode codes"
     (() (display-chars ((object) 'length) unicodehexcode 8))
     ((n)
      (range-check n ((object) 'length))
      (display-chars n unicodehexcode 8))]

   ["ascii"
     "display [n] characters as hexadecimal ascii codes"
     (() (display-chars ((object) 'length) asciihexcode 16))
     ((n)
      (range-check n ((object) 'length))
      (display-chars n asciihexcode 16))]
))

(define char-dispatch-table
 (make-dispatch-table

   ["unicode"
    "display character as hexadecimal ascii code"
     (() (show "   U+~x" (unicodehexcode ((object) 'value))))]

   ["ascii"
    "display character as hexadecimal ascii code"
     (() (show "   ~x" (asciihexcode ((object) 'value))))]

))

(define box-dispatch-table
 (make-dispatch-table

   ["unbox"
     "inspect contents of box"
     (() (down ((object) 'unbox) #f))]

   [("ref" . "r")
     "inspect contents of box"
     (() (down ((object) 'unbox) #f))]

   [("show" . "s")
     "show contents of box"
     (() (label-line-display ((object) 'unbox) 0))
     ((n)
      (range-check n 0)
      (label-line-display ((object) 'unbox) 0))]
))


(define system-symbol-dispatch-table
 (make-dispatch-table

   [("ref" . "r")
    "inspect value field [n] of symbol"
    (()
     (down ((object) 'top-level-value) 0))
    ((n)
     (range-check n 5)
     (down ((object)
            (case n
               [(0) 'top-level-value]
               [(1) '$top-level-value]
               [(2) 'name]
               [(3) 'property-list]
               [(4) 'system-property-list]
               [(5) 'symbol-hash]))
           n))]

   [("value" . "v")
    "inspect top-level-value of symbol"
    (() (down ((object) 'top-level-value) 0))]

   [("value-slot" . "vs")
    "inspect value slot of symbol"
    (() (down ((object) '$top-level-value) 0))]

   [("name" . "n")
    "inspect name of symbol"
    (() (down ((object) 'name) 1))]

   [("property-list" . "pl")
    "inspect property-list of symbol"
    (() (down ((object) 'property-list) 2))]

   [("system-property-list" . "spl")
    "inspect system property-list of symbol"
    (() (down ((object) 'system-property-list) 4))]

   [("symbol-hash" . "sh")
    "inspect hash code"
    (() (down ((object) 'symbol-hash) 5))]

   [("show" . "s")
     "show fields of symbol"
     (()
      (name-label-line-display ((object) 'top-level-value) "top-level value" 0)
      (name-label-line-display ((object) '$top-level-value) "value slot" 1)
      (name-label-line-display ((object) 'name) "name" 2)
      (name-label-line-display ((object) 'property-list) "properties" 3)
      (name-label-line-display ((object) 'system-property-list) "system properties" 4)
      (name-label-line-display ((object) 'symbol-hash) "hash code" 5))]
))

(define symbol-dispatch-table
 (make-dispatch-table

   [("ref" . "r")
    "inspect value field [n] of symbol"
    (()
     (down ((object) 'top-level-value) 0))
    ((n)
     (range-check n 2)
     (down ((object)
            (case n
               [(0) 'top-level-value]
               [(1) 'name]
               [(2) 'property-list]))
           n))]

   [("value" . "v")
    "inspect top-level-value of symbol"
    (() (down ((object) 'top-level-value) 0))]

   [("name" . "n")
    "inspect name of symbol"
    (() (down ((object) 'name) 1))]

   [("property-list" . "pl")
    "inspect property-list of symbol"
    (() (down ((object) 'property-list) 2))]

   [("show" . "s")
     "show fields of symbol"
     (()
      (name-label-line-display ((object) 'top-level-value) "top level value" 0)
      (name-label-line-display ((object) 'name) "name" 1)
      (name-label-line-display ((object) 'property-list) "properties" 2))]
))

(define procedure-dispatch-table
 (make-dispatch-table

   [("length" . "l")
    "display number of free variables"
    (() (show "   ~d free variables" ((object) 'length)))]

   [("ref" . "r")
    "inspect [nth] free variable"
    (() (ref 0))
    ((x) (variable-ref x))]

   [("set!" . "!")
    "set [nth or named] free variable to value, if assignable"
    (()
     (let ([e (waiter-read)])
       (unless (eof-object? e)
         (set 0 ((object) 'eval e)))))
    ((x)
     (let ([e (waiter-read)])
       (unless (eof-object? e)
         (variable-set x ((object) 'eval e)))))
    ((x e) (variable-set x ((object) 'eval e)))]

  [("eval" . "e")
    "evaluate expression in context of procedure environment"
    (()
     (let ([x (waiter-read)])
       (unless (eof-object? x)
         (inspector-print ((object) 'eval x)))))
    ((x)
     (inspector-print ((object) 'eval x)))]

   [("show" . "s")
    "show code and free variables"
    (()
     (let ([source (((object) 'code) 'source)])
        (when source (name-line-display source "code")))
     (when (> ((object) 'length) 0)
        (show "~afree variables:" line-indent)
        (display-variable-refs ((object) 'length))))]

   [("code" . "c")
    "inspect the code for the procedure"
    (()
     (let ([source (((object) 'code) 'source)])
        (if source
            (down source #f)
            (show "source code not available"))))]

   ["file"
    "switch to source file containing the procedure"
    (() (open-recorded-source-file ((object) 'code)))
    ((path)
     (unless (or (string? path) (symbol? path))
       (inspect-error "invalid path ~s" path))
     (open-source-file (if (symbol? path) (symbol->string path) path)))]
))

(define code-dispatch-table
 (make-dispatch-table

  [("length" . "l")
   "display number of free variables"
   (() (show "   ~d free variables" ((object) 'free-count)))]

  [("show" . "s")
   "show code"
   (()
    (let ([source ((object) 'source)])
       (when source (name-line-display source "code"))))]

  [("code" . "c")
   "inspect the code"
   (()
    (let ([source ((object) 'source)])
       (if source
           (down source #f)
           (show "source code not available"))))]

  ["file"
   "switch to source file containing the procedure"
   (() (open-recorded-source-file (object)))
   ((path)
    (unless (or (string? path) (symbol? path))
      (inspect-error "invalid path ~s" path))
    (open-source-file (if (symbol? path) (symbol->string path) path)))]
))


(define continuation-dispatch-table
  (let ()
    (define reposition
      (lambda (incr)
        (let ([old-pos ((object) 'pos)])
          (unless (fx= old-pos 0) (up))
          (let ([pos (fx+ old-pos incr)])
            (when (fx>= pos ((object) 'depth)) (invalid-movement))
            (if (fx> pos 0)
                (let ((link ((object) 'reposition pos)))
                  (unless (type? 'continuation link) (invalid-movement))
                  (down link #f))
                (unless (fx= pos 0) (invalid-movement)))))))

    (define continuation-show
      (lambda (free?)
        (name-line-display ((object) 'link) "continuation")
        (let ([source (((object) 'code) 'source)])
          (when source (name-line-display source "procedure code")))
        (let ([source ((object) 'source)])
          (when source (name-line-display source "call code")))
        (let ([cp ((object) 'closure)])
          (when cp (name-line-display cp "closure")))
        (let ([len ((object) (if free? 'length 'frame-length))])
          (when (> len 0)
            (show "~a~a:" line-indent (if free? "frame and free variables" "frame variables"))
            (display-variable-refs len)))))

     (make-dispatch-table

       [("length" . "l")
        "display number of frame and closure variables"
        (() (show "   ~d variables" ((object) 'length)))]

       ["depth"
         "display number of frames in continuation stack"
         (() (let ((d ((object) 'depth)))
               (show (if (= d 1) "   ~d frame" "   ~d frames") d)))]

       [("ref" . "r")
        "inspect [named or nth] variable"
        (() (ref 0))
        ((x) (variable-ref x))]

       [("set!" . "!")
        "set [named or nth] variable to value, if assignable"
        (()
         (let ([e (waiter-read)])
           (unless (eof-object? e)
             (set 0 ((object) 'eval e)))))
        ((x)
         (let ([e (waiter-read)])
           (unless (eof-object? e)
             (variable-set x ((object) 'eval e)))))
        ((x e) (variable-set x ((object) 'eval e)))]

       [("forward" . "f")
        "move to [nth] next frame"
        (() (reposition 1))
        ((pos)
         (range-check pos)
         (reposition pos))]

       [("back" . "b")
        "move to [nth] previous frame"
        (() (reposition -1))
        ((pos)
         (range-check pos)
         (reposition (fx- pos)))]

       [("down" . "d")
        "inspect [nth] next frame"
        (() (let ((link ((object) 'link)))
              (unless (type? 'continuation link) (invalid-movement))
              (down link #f)))
        ((n)
         (range-check n (- ((object) 'depth) 1))
         (let ((link ((object) 'link* n)))
           (unless (type? 'continuation link) (invalid-movement))
           (down link #f)))]

       [("closure" . "cp")
        "inspect the frame's closure, if any"
        (() (let ([cp ((object) 'closure)])
              (unless cp (inspect-error "this frame has no closure"))
              (down cp #f)))]

       [("eval" . "e")
        "evaluate expression in context of current frame"
        (()
         (let ([x (waiter-read)])
           (unless (eof-object? x)
             (inspector-print ((object) 'eval x)))))
        ((x)
         (inspector-print ((object) 'eval x)))]

       [("show" . "s")
        "show frame with free variables"
        (() (continuation-show #t))]

       [("show-local" . "sl")
        "show frame without free variables"
        (() (continuation-show #f))]

       [("show-frames" . "sf")
        "show the next [n] frames"
        (() (display-links (most-positive-fixnum)))
        ((n)
         (range-check n)
         (display-links n))]

       ["call"
         "inspect the code for the pending call"
         (()
          (let ([source ((object) 'source)])
            (if source
                (down source #f)
                (show "source code not available"))))]

       [("code" . "c")
        "inspect the code for the pending procedure"
        (()
         (let ([source (((object) 'code) 'source)])
           (if source
               (down source #f)
               (show "source code not available"))))]

       ["file"
         "switch to source file containing the pending call"
         (() (open-recorded-source-file (object)))
         ((path)
          (unless (or (string? path) (symbol? path))
            (inspect-error "invalid path ~s" path))
          (open-source-file (if (symbol? path) (symbol->string path) path)))]

       )))

(define port-dispatch-table
 (make-dispatch-table

   [("show" . "s")
    "show port contents"
    (()
     (name-line-display ((object) 'name) "name")
     (name-line-display ((object) 'handler) "handler")
     (when ((object) 'input?)
        (show "~ainput size: ~s" line-indent ((object) 'input-size))
        (show "~ainput index: ~s" line-indent ((object) 'input-index)))
     (when ((object) 'output?)
        (show "~aoutput size: ~s" line-indent ((object) 'output-size))
        (show "~aoutput index: ~s" line-indent ((object) 'output-index))))]

   ["name"
    "inspect port name"
    (() (down ((object) 'name) #f))]

   ["handler"
    "inspect port handler"
    (() (down ((object) 'handler) #f))]

   [("output-buffer" . "ob")
    "inspect output buffer"
    (() (if ((object) 'output?)
            (down ((object) 'output-buffer) #f)
            (show "not an output port")))]

   [("input-buffer" . "ib")
    "inspect input buffer"
    (() (if ((object) 'input?)
            (down ((object) 'input-buffer) #f)
            (show "not an input port")))]
))

(define tlc-dispatch-table
 (make-dispatch-table

   ["keyval"
     "inspect keyval field"
     (() (down ((object) 'keyval) #f))]

   ["ht"
     "inspect ht field"
     (() (down ((object) 'ht) #f))]

   ["next"
     "inspect next field"
     (() (down ((object) 'next) #f))]

   [("ref" . "r")
    "inspect named field"
    ((x)
     (down ((object)
            (case x
               [(keyval) 'keyval]
               [(ht) 'ht]
               [(next) 'next]
               [else (invalid-command)]))
           x))]

   [("show" . "s")
     "show fields of tlc"
     (()
      (name-line-display ((object) 'keyval) "keyval")
      (name-line-display ((object) 'ht) "ht")
      (name-line-display ((object) 'next) "next"))]
))

(set! inspect
  (lambda (x)
    (let ([t (set-timer 0)])
      (call/cc
        (lambda (k)
          (fluid-let ([current-state (make-state (inspect/object x))]
                      [marks (make-eq-hashtable)]
                      [source-files '()])
             (parameterize ([outer-reset-handler (reset-handler)]
                            [exit-handler k]
                            [$interrupt reset])
               (put-mark default-mark)
               (dynamic-wind
                 void
                 (lambda () (inspector '("?")))
                 (lambda () (for-each close-source-file source-files)))))))
      (set-timer t))
    (void)))

)

(define inspect/object
  (lambda (x)
    (define compute-size
      (let ([size-ht #f])
        (lambda (x g)
          (unless (or (and (fixnum? g) (fx<= 0 g (collect-maximum-generation))) (eq? g 'static))
            ($oops 'inspector-object "invalid generation ~s" g))
          ; using a common size-ht for a single inspect/object call means:
          ;   (inspect (let ([x (list 1 2)]) (set-car! x x) (set-car! (cdr x) x) (set-cdr! (cdr x) x) x))
          ;     size => 16
          ;     cdr, size => 8
          ; might be what we want, might not be
          (unless size-ht (set! size-ht (make-eq-hashtable)))
          ($compute-size x (if (eq? g 'static) (constant static-generation) g) size-ht))))

    (define-syntax make-object-maker
      (lambda (x)
        (syntax-case x ()
          [(_ object-name inits [method args e1 e2 ...] ...)
           (andmap identifier? #'(object-name method ...))
           #'(lambda inits
               (let ([method (lambda args e1 e2 ...)] ...)
                 (lambda (m . rest)
                   (case m
                     [(type) 'object-name]
                     [(make-me-a-child) (make-object (car rest))]
                     [(method) (#2%apply method rest)]
                     ...
                     [else ($oops 'inspector-object
                             "invalid message ~s to object type ~s"
                             m
                             'object-name)]))))])))

    (define frame-eval
      (lambda (vars expr)
        (define frame-name
          (let ((ls '(%0 %1 %2 %3 %4 %5 %6 %7)))
            (let ((n (length ls)))
              (lambda (i)
                (if (< i n)
                    (list-ref ls i)
                    (string->symbol (format "%~d" i)))))))
        (define ->nongensym
          (lambda (name)
            (if (gensym? name)
                (string->symbol (symbol->string name))
                name)))
        (let ((n (vector-length vars)))
          (eval (let f ((i 0))
                  (if (= i n)
                      expr
                      (let ([var (vector-ref vars i)]
                            [body (f (+ i 1))])
                        (let ([raw-val (var 'raw-value)]
                              [name (var 'name)]
                              [fv (frame-name i)]
                              [t (gensym)])
                          `(let ([,t (quote ,raw-val)])
                             (let-syntax ([,fv ,(if (assignable? raw-val)
                                                    `(identifier-syntax [id (car ,t)] [(set! id e) (set-car! ,t e)])
                                                    `(identifier-syntax
                                                       [id ,t]
                                                       [(set! id e)
                                                        (syntax-error #'id "cannot set non-assigned variable")]))])
                               ,(if name `(begin (alias ,(->nongensym name) ,fv) ,body) body)))))))))))

    (define make-pair-object
      (make-object-maker pair (x)
        [value () x]
        [car () (make-object (car x))]
        [cdr () (make-object (cdr x))]
        [length ()
          (let ([ht (make-eq-hashtable)])
            (let length ([x x] [n 0])
              (cond
                [(null? x) `(proper ,n)]
                [(not (pair? x)) `(improper ,n)]
                [else
                  (let ([a (eq-hashtable-cell ht x #f)])
                    (if (cdr a)
                        `(circular ,n)
                        (begin (set-cdr! a #t)
                          (length (cdr x) (+ n 1)))))])))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-box-object
      (make-object-maker box (x)
        [value () x]
        [unbox () (make-object (unbox x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-tlc-object
      (make-object-maker tlc (x)
        [value () x]
        [keyval () (make-object ($tlc-keyval x))]
        [ht () (make-object ($tlc-ht x))]
        [next () (make-object ($tlc-next x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-vector-object
      (make-object-maker vector (x)
        [value () x]
        [length () (vector-length x)]
        [ref (i)
          (unless (and (fixnum? i) (fx< -1 i (vector-length x)))
            ($oops 'vector-object "invalid index ~s" i))
          (make-object (vector-ref x i))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-fxvector-object
      (make-object-maker fxvector (x)
        [value () x]
        [length () (fxvector-length x)]
        [ref (i)
          (unless (and (fixnum? i) (fx< -1 i (fxvector-length x)))
            ($oops 'fxvector-object "invalid index ~s" i))
          (make-object (fxvector-ref x i))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-bytevector-object
      (make-object-maker bytevector (x)
        [value () x]
        [length () (bytevector-length x)]
        [ref (i)
          (unless (and (fixnum? i) (fx< -1 i (bytevector-length x)))
            ($oops 'bytevector-object "invalid index ~s" i))
          (make-object (bytevector-u8-ref x i))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-ftype-pointer-object
      (lambda (x)
        (define (unrecognized-ux ux)
          ($oops 'ftype-pointer-object "unrecognized ftype-pointer type ~s" x))
        (define (invalid-field-specifier f)
          ($oops 'ftype-pointer-object "invalid field specifier ~s" f))
        (define (invalid-index f)
          ($oops 'ftype-pointer-object "invalid index ~s" f))
        (define (get-field f field*)
          (cond
            [(assq f field*) => cdr]
            [(and (fixnum? f) (#%$fxu< f (length field*)))
             (cdr (list-ref field* f))]
            [else (invalid-field-specifier f)]))
        (define (deref x)
          (let ([ux ($unwrap-ftype-pointer x)])
            (record-case ux
              [(struct union array * bits) ignore (make-object x)]
              [(base) (type getter setter) (make-object (getter))]
              [else (unrecognized-ux ux)])))
        (define (deset! who x v)
          (let ([ux ($unwrap-ftype-pointer x)])
            (record-case ux
              [(struct union array bits) ignore ($oops who "cannot assign struct, union, or array")]
              [(*) (get-fptr set-fptr!) (set-fptr! who v)]
              [(base) (type getter setter) (setter v)]
              [else (unrecognized-ux ux)])))
        (let ([ux ($unwrap-ftype-pointer x)])
          (record-case ux
            [(struct) field*
             ((make-object-maker ftype-struct (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [fields () (make-object (map (lambda (x) (or (car x) '_)) field*))]
                [length () (length field*)]
                [ref (f) (deref (get-field f field*))]
                [set! (f v) (deset! 'ftype-struct-object (get-field f field*) v)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype struct ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(union) field*
             ((make-object-maker ftype-union (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [fields () (make-object (map (lambda (x) (or (car x) '_)) field*))]
                [length () (length field*)]
                [ref (f) (deref (get-field f field*))]
                [set! (f v) (deset! 'ftype-union-object (get-field f field*) v)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype union ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(array) (n get-fptr)
             ((make-object-maker ftype-array (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [length () n]
                [ref (f)
                  (unless (and (integer? f) (exact? f) (#%$fxu< f n))
                    (invalid-index f))
                  (deref (get-fptr f))]
                [set! (f v)
                  (unless (and (integer? f) (exact? f) (#%$fxu< f n))
                    (invalid-index f))
                  (deset! 'ftype-array-object (get-fptr f) v)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype array ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(*) (get-fptr set-fptr!)
             ((make-object-maker ftype-* (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [ref () (deref (get-fptr))]
                [set! (v) (deset! 'ftype-*-object (get-fptr) v)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype * ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(bits) field*
             ((make-object-maker ftype-bits (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [fields () (make-object (map (lambda (x) (or (car x) '_)) field*))]
                [length () (length field*)]
                [ref (f) (apply (lambda (getter setter) (make-object (getter)))
                           (get-field f field*))]
                [set! (f v) (apply (lambda (getter setter) (make-object (setter v)))
                              (get-field f field*))]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype bits ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(base) (type getter setter)
             ((make-object-maker ftype-base (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [ref () (make-object (getter))]
                [set! (v) (setter v)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype ,type ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [(function) (name)
             ((make-object-maker ftype-function (x)
                [value () x]
                [ftype () (make-object (ftype-pointer-ftype x))]
                [address () (make-object (ftype-pointer-address x))]
                [name () (make-object name)]
                [size (g) (compute-size x g)]
                [write (p) (write `(ftype function ...) p)]
                [print (p) (pretty-print (ftype-pointer->sexpr x) p)])
              x)]
            [else (unrecognized-ux ux)]))))

    (define make-record-object
      (lambda (x)
        (let* ((rtd ($record-type-descriptor x))
               (fields (csv7:record-type-field-names rtd)))
          (define check-field
            (lambda (f)
              (unless (or (and (symbol? f) (memq f fields))
                          (and (fixnum? f) (fx>= f 0) (fx< f (length fields))))
                ($oops 'record-object "invalid field specifier ~s" f))))
          ((make-object-maker record (x)
             [value () x]
             [length () (length fields)]
             [fields () (make-object fields)]
             [accessible? (f)
               (check-field f)
               (csv7:record-field-accessible? rtd f)]
             [mutable? (f)
               (check-field f)
               (csv7:record-field-mutable? rtd f)]
             [name () (make-object (csv7:record-type-name rtd))]
             [rtd () (make-object rtd)]
             [ref (f)
               (check-field f)
               (unless (csv7:record-field-accessible? rtd f)
                 ($oops 'record-object "field ~s is inaccessible" f))
               (make-object ((csv7:record-field-accessor rtd f) x))]
             [set! (f v)
               (check-field f)
               (unless (csv7:record-field-mutable? rtd f)
                 ($oops 'record-object "field ~s is immutable" f))
               ((csv7:record-field-mutator rtd f) x v)]
             [size (g) (compute-size x g)]
             [write (p) (write x p)]
             [print (p) (pretty-print x p)])
           x))))

    (define make-string-object
      (make-object-maker string (x)
        [value () x]
        [length () (string-length x)]
        [ref (i)
          (unless (and (fixnum? i) (< -1 i (string-length x)))
            ($oops 'string-object "invalid index ~s" i))
          (make-object (string-ref x i))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-simple-object
      (make-object-maker simple (x)
        [value () x]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-unbound-object
      (make-object-maker unbound (x)
        [value () x]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-procedure-object
      (lambda (x)
        (real-make-procedure-object x (list->vector (make-procedure-vars x)))))

    (define real-make-procedure-object
      (make-object-maker procedure (x vars)
        [value () x]
        [length () (vector-length vars)]
        [ref (i)
          (unless (and (fixnum? i) (fx< -1 i (vector-length vars)))
            ($oops 'procedure-object "invalid index ~s" i))
          (vector-ref vars i)]
        [eval (x) (frame-eval vars x)]
        [code () (make-object ($closure-code x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-procedure-vars
      (lambda (x)
        (include "types.ss")
        (let ([code ($closure-code x)])
          (let ([info ($code-info code)]
                [len ($code-free-count code)])
            (let ([free (and (code-info? info) (code-info-free info))])
              (unless (or (not free) (fx= (vector-length free) len))
                ($oops 'inspector "invalid info structure ~s" info))
              (let vars ([i 0])
                (if (= i len)
                    '()
                    (cons (make-variable-object
                            ($closure-ref x i)
                            (and free (vector-ref free i)))
                      (vars (+ i 1))))))))))

    (define assignable?
      (lambda (raw-val)
        (and (pair? raw-val) ($unbound-object? (cdr raw-val)))))

    (define make-variable-object
      (make-object-maker variable (x name)
        [name () name]
        [assignable? () (assignable? x)]
        [raw-value () x]
        [ref () (make-object
                  (if (assignable? x)
                      (car x)
                      x))]
        [set! (v) (make-object
                    (if (assignable? x)
                        (set-car! x v)
                        ($oops 'variable-object "unassignable variable")))]
        [size (g)
         (if (assignable? x)
             (fx+ (constant size-pair) (compute-size (car x) g))
             (compute-size x g))]
        [write (p) (display "#<variable>" p)]
        [print (p) (display "#<variable>" p) (newline p)]))

    (define get-reloc-objs
      (foreign-procedure "(cs)s_get_reloc"
        (scheme-object) scheme-object))

    (module (get-code-src get-code-sexpr)
      (include "types.ss")
      (define get-code-src
        (lambda (x)
          (let ([info ($code-info x)])
            (and (code-info? info) (code-info-src info)))))
      (define get-code-sexpr
        (lambda (x)
          (let ([info ($code-info x)])
            (and (code-info? info) (code-info-sexpr info))))))

    (define make-code-object
      (make-object-maker code (x)
        [value () x]
        [name () ($code-name x)]
        [info () (make-object ($code-info x))]
        [free-count () ($code-free-count x)]
        [source ()
          (cond
            [(get-code-sexpr x) => make-object]
            [else #f])]
        [source-path () (return-source (get-code-src x))]
        [source-object () (get-code-src x)]
        [reloc () (make-object (get-reloc-objs x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define return-source
      (lambda (src)
        (include "types.ss")
        (if src
            (call-with-values
              (lambda () ((current-locate-source-object-source) src #t #f))
              (case-lambda
                [() (let ([sfd (source-sfd src)] [fp (source-bfp src)])
                      (values (source-file-descriptor-name sfd) fp))]
                [(path line char) (values path line char)]))
            (values))))

    (define-who make-continuation-object
      (lambda (x pos)
        (include "types.ss")
        (define find-rpi
          (lambda (offset rpis)
            (let f ([start 0] [end (fx1- (vector-length rpis))])
              (if (fx< end start)
                  #f
                  (let* ([curr (fx+ (fx/ (fx- end start) 2) start)]
                         [rpi (vector-ref rpis curr)]
                         [rpi-offset (rp-info-offset rpi)])
                    (cond
                      [(fx= offset rpi-offset) rpi]
                      [(fx< offset rpi-offset) (f start (fx1- curr))]
                      [else  (f (fx1+ curr) end)]))))))
        ($split-continuation x 0)
        (let ([info ($code-info ($continuation-return-code x))]
              [offset ($continuation-return-offset x)]
              [len ($continuation-stack-length x)]
              [lpm ($continuation-return-livemask x)])
          (cond
            [(and (code-info? info) (code-info-rpis info) (find-rpi offset (code-info-rpis info))) =>
             (lambda (rpi)
               (let ([cookie '(chocolate . chip)])
                 (let ([vals (make-vector len cookie)] [vars (make-vector len '())] [live (code-info-live info)])
                   ; fill vals based on live-pointer mask
                   (let f ([i 1] [lpm lpm])
                     (unless (>= i len)
                       (when (odd? lpm)
                         (vector-set! vals (fx1- i) ($continuation-stack-ref x i)))
                       (f (fx1+ i) (ash lpm -1))))
                   ; fill vars based on code-info variable mask
                   (let f ([i 0] [mask (rp-info-mask rpi)])
                     (unless (eqv? mask 0)
                       (when (odd? mask)
                         (let ([p (vector-ref live i)])
                           (let ([index (fx1- (cdr p))])
                             (vector-set! vars index (cons (car p) (vector-ref vars index))))))
                       (f (+ i 1) (ash mask -1))))
                   ; create return vector
                   (with-values
                     (let f ([i 0] [count 0] [cp #f] [cpvar* '()])
                       (if (fx= i len)
                           (if cp
                               (let ([v (let f ([count count] [cpvar* cpvar*])
                                          (if (null? cpvar*)
                                              (make-vector count)
                                              (let ([v (f (fx+ count 1) (cdr cpvar*))])
                                                (vector-set! v count (car cpvar*))
                                                v)))])
                                 (values v count cp))
                               (values (make-vector count) count cp))
                           (let ([obj (vector-ref vals i)] [var* (vector-ref vars i)])
                             (cond
                               [(eq? obj cookie)
                                (unless (null? var*) ($oops who "expected value for ~s but it was not in lpm" (car var*)))
                                (f (fx1+ i) count cp cpvar*)]
                               [(null? var*)
                                (let-values ([(v frame-count cp) (f (fx1+ i) (fx1+ count) cp cpvar*)])
                                  (vector-set! v count (make-variable-object obj #f))
                                  (values v frame-count cp))]
                               [else
                                 (let g ([var* var*] [count count] [cp cp] [cpvar* cpvar*])
                                   (if (null? var*)
                                       (f (fx1+ i) count cp cpvar*)
                                       (let ([var (car var*)])
                                         (if (eq? var cpsymbol)
                                             (g (cdr var*) count obj (if (procedure? obj) (make-procedure-vars obj) '()))
                                             (cond
                                               [(pair? var) ; closure environment represented as a pair
                                                (unless (pair? obj)
                                                  ($oops who "expected pair value for paired environment, not ~s" obj))
                                                (g (cdr var*) count obj (list
                                                                          (make-variable-object (car obj) (car var))
                                                                          (make-variable-object (cdr obj) (cdr var))))]
                                               [(vector? var) ; closure environment represented as a vector
                                                (unless (vector? obj)
                                                  ($oops who "expected vector value for vector environment, not ~s" obj))
                                                (g (cdr var*) count obj (map (lambda (obj var) (make-variable-object obj var))
                                                                          (vector->list obj)
                                                                          (vector->list var)))]
                                               [else
                                                 (let-values ([(v frame-count cp) (g (cdr var*) (fx1+ count) cp cpvar*)])
                                                   (vector-set! v count (make-variable-object obj var))
                                                   (values v frame-count cp))])))))]))))
                     (lambda (v frame-count cp)
                       (real-make-continuation-object x (rp-info-src rpi) (rp-info-sexpr rpi) cp v frame-count pos))))))]
            [else
              (let ([v (list->vector
                         (let f ([i 1] [lpm lpm])
                           (cond
                             [(>= i len) '()]
                             [(odd? lpm)
                              (cons (make-variable-object ($continuation-stack-ref x i) #f)
                                (f (fx1+ i) (ash lpm -1)))]
                             [else (f (fx1+ i) (ash lpm -1))])))])
                (real-make-continuation-object x #f #f #f v (vector-length v) pos))]))))

    (define real-make-continuation-object
      (let ((continuation-depth
              (foreign-procedure "(cs)continuation_depth" (scheme-object)
                iptr)))
        (make-object-maker continuation (x src sexpr cp vars frame-count pos)
          [value () x]
          [length () (vector-length vars)]
          [closure () (and cp (make-object cp))]
          [frame-length () frame-count]
          [depth () (continuation-depth x)]
          [ref (i)
            (unless (and (fixnum? i) (fx< -1 i (vector-length vars)))
              ($oops 'continuation-object "invalid index ~s" i))
            (vector-ref vars i)]
          [pos () pos]
          [reposition (pos)
            (let ((k (and (fixnum? pos) (fx> pos 0) ($split-continuation x pos))))
              (unless k ($oops 'continuation-object "invalid position ~s" pos))
              (make-continuation-object k pos))]
          [link () (make-object ($continuation-link x))]
          [link* (i)
            (let ((k (and (fixnum? i) (fx>= i 0) ($split-continuation x i))))
              (unless k ($oops 'continuation-object "invalid link* depth ~s" i))
              (make-object k))]
          [eval (x) (frame-eval vars x)]
          [code () (make-object ($continuation-return-code x))]
          [source () (and sexpr (make-object sexpr))]
          [source-object () src]
          [source-path () (return-source src)]
          [size (g) (compute-size x g)]
          [write (p) (write x p)]
          [print (p) (pretty-print x p)])))

    (define make-port-object
      (make-object-maker port (x)
        [value () x]
        [input? () (input-port? x)]
        [output? () (output-port? x)]
        [binary? () (binary-port? x)]
        [closed? () (port-closed? x)]
        [handler () (make-object ($port-handler x))]
        [output-buffer () (and (output-port? x)
                               (make-object
                                 (if (textual-port? x)
                                     (textual-port-output-buffer x)
                                     (binary-port-output-buffer x))))]
        [output-size () (and (output-port? x)
                             (if (textual-port? x)
                                 (textual-port-output-size x)
                                 (binary-port-output-size x)))]
        [output-index () (and (output-port? x)
                              (if (textual-port? x)
                                  (textual-port-output-index x)
                                  (binary-port-output-index x)))]
        [input-buffer () (and (input-port? x)
                              (make-object
                                (if (textual-port? x)
                                    (textual-port-input-buffer x)
                                    (binary-port-input-buffer x))))]
        [input-size () (and (input-port? x)
                            (if (textual-port? x)
                                (textual-port-input-size x)
                                (binary-port-input-size x)))]
        [input-index () (and (input-port? x)
                             (if (textual-port? x)
                                 (textual-port-input-index x)
                                 (binary-port-input-index x)))]
        [info () (make-object ($port-info x))]
        [name () (make-object (port-name x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-symbol-object
      (make-object-maker symbol (x)
        [value () x]
        [gensym? () (gensym? x)]
        [top-level-value ()
          (if (top-level-bound? x)
              (make-object (top-level-value x))
              (make-object ($unbound-object)))]
        [$top-level-value ()
          (if ($top-level-bound? x)
              (make-object ($top-level-value x))
              (make-object ($unbound-object)))]
        [system-property-list () (make-object ($system-property-list x))]
        [symbol-hash () (make-object ($symbol-hash x))]
        [name () (make-object (symbol->string x))]
        [property-list () (make-object ($symbol-property-list x))]
        [size (g) (compute-size x g)]
        [write (p) (write x p)]
        [print (p) (pretty-print x p)]))

    (define make-object
      (lambda (x)
        (cond
          [(pair? x) (make-pair-object x)]
          [(symbol? x) (make-symbol-object x)]
          [(vector? x) (make-vector-object x)]
          [(fxvector? x) (make-fxvector-object x)]
          [(bytevector? x) (make-bytevector-object x)]
          ; ftype-pointer? test must come before record? test
          [($ftype-pointer? x) (make-ftype-pointer-object x)]
          [(or (record? x) (and (eq? (subset-mode) 'system) ($record? x)))
           (make-record-object x)]
          [(string? x) (make-string-object x)]
          [(box? x) (make-box-object x)]
          [(procedure? x)
           (if ($continuation? x)
               (if (= ($continuation-stack-length x)
                      (constant unscaled-shot-1-shot-flag))
                   (make-simple-object x)
                   (make-continuation-object x 0))
               (make-procedure-object x))]
          [($code? x) (make-code-object x)]
          [(port? x) (make-port-object x)]
          [($unbound-object? x) (make-unbound-object x)]
          [($tlc? x) (make-tlc-object x)]
          [else (make-simple-object x)])))

    (make-object x)))

(let ()
  (define rtd-size (csv7:record-field-accessor #!base-rtd 'size))
  (define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))
  (define $generation (foreign-procedure "(cs)generation" (ptr) ptr))
  (define $get-code-obj (foreign-procedure "(cs)get_code_obj" (int ptr iptr iptr) ptr))
  (define $code-reloc-size
    (lambda (x)
      (let ([reloc-table ($object-ref 'scheme-object x (constant code-reloc-disp))])
        (if (eqv? reloc-table 0)
            0
            ($object-ref 'iptr reloc-table (constant reloc-table-size-disp))))))
  (define $code-length
    (lambda (x)
      ($object-ref 'iptr x (constant code-length-disp))))
  (define $get-reloc
    (lambda (x i)
      (let ([reloc-table ($object-ref 'scheme-object x (constant code-reloc-disp))])
        (and (not (eqv? reloc-table 0))
             ($object-ref 'uptr reloc-table
               (fx+ (constant reloc-table-data-disp)
                 (fx* i (constant ptr-bytes))))))))
  (define-syntax tc-ptr-offsets
    (lambda (x)
      #`'#,(datum->syntax #'*
             (fold-left
               (lambda (ls fld)
                 (apply (lambda (name type disp len)
                          (if (eq? type 'ptr)
                              (if len
                                  (do ([len len (fx- len 1)]
                                       [disp disp (fx+ disp (constant ptr-bytes))]
                                       [ls ls (cons disp ls)])
                                    ((fx= len 0) ls))
                                  (cons disp ls))
                              ls))
                   fld))
               '()
               (or (getprop 'tc '*fields* #f) ($oops 'tc-ptr-offsets "missing fields for tc"))))))
  (define align
    (lambda (n)
      (fxlogand (fx+ n (fx- (constant byte-alignment) 1)) (fx- (constant byte-alignment)))))

  (set-who! $compute-size
    (rec $compute-size
      (case-lambda
        [(x maxgen) ($compute-size x maxgen (make-eq-hashtable))]
        [(x maxgen size-ht)
         (define cookie (cons 'date 'nut)) ; recreate on each call to $compute-size
         (define compute-size
           (lambda (x)
             (if (or ($immediate? x)
                     (let ([g ($generation x)])
                       (or (not g) (fx> g maxgen))))
                 0
                 (let ([a (eq-hashtable-cell size-ht x #f)])
                   (cond
                     [(cdr a) =>
                      (lambda (p)
                        ; if we find our cookie, return 0 to avoid counting shared structure twice.
                        ; otherwise, (car p) must be a cookie from an earlier call to $compute-size,
                        ; so return the recorded size
                        (if (eq? (car p) cookie)
                            0
                            (begin
                              (set-car! p cookie)
                              (cdr p))))]
                     [else
                      (let ([p (cons cookie 0)])
                        (set-cdr! a p)
                        (let ([size (really-compute-size x)])
                          (set-cdr! p size)
                          size))])))))
         (define really-compute-size
           (lambda (x)
             (cond
               [(pair? x) (fx+ (constant size-pair) (compute-size (car x)) (compute-size (cdr x)))]
               [(symbol? x)
                (fx+ (constant size-symbol)
                  (compute-size (#3%$top-level-value x))
                  (compute-size (property-list x))
                  (compute-size ($system-property-list x))
                  (compute-size ($symbol-name x)))]
               [(vector? x)
                (let ([n (vector-length x)])
                  (do ([i 0 (fx+ i 1)]
                       [size (align (fx+ (constant header-size-vector) (fx* (vector-length x) (constant ptr-bytes))))
                         (fx+ size (compute-size (vector-ref x i)))])
                    ((fx= i n) size)))]
               [(fxvector? x) (align (fx+ (constant header-size-fxvector) (fx* (fxvector-length x) (constant ptr-bytes))))]
               [(bytevector? x) (align (fx+ (constant header-size-bytevector) (bytevector-length x)))]
               [($record? x)
                (let ([rtd ($record-type-descriptor x)])
                  (fold-left (lambda (size fld)
                               (if (eq? (fld-type fld) 'scheme-object)
                                   (fx+ size (compute-size ($object-ref 'scheme-object x (fld-byte fld))))
                                   size))
                    (fx+ (align (rtd-size rtd)) (compute-size rtd))
                    (rtd-flds rtd)))]
               [(string? x) (align (fx+ (constant header-size-string) (fx* (string-length x) (constant string-char-bytes))))]
               [(box? x) (fx+ (constant size-box) (compute-size (unbox x)))]
               [(flonum? x) (constant size-flonum)]
               [(bignum? x) (align (fx+ (constant header-size-bignum) (fx* ($bignum-length x) (constant bigit-bytes))))]
               [(ratnum? x) (fx+ (constant size-ratnum) (compute-size ($ratio-numerator x)) (compute-size ($ratio-denominator x)))]
               [($exactnum? x) (fx+ (constant size-exactnum) (compute-size ($exactnum-real-part x)) (compute-size ($exactnum-imag-part x)))]
               [($inexactnum? x) (constant size-inexactnum)]
               [(procedure? x)
                (if ($continuation? x)
                    (if (or (eq? x $null-continuation) (= ($continuation-stack-length x) (constant unscaled-shot-1-shot-flag)))
                        (constant size-continuation)
                        (begin
                          ; NB: rather not do this...splitting creates new continuation objects and gives an inaccurate
                          ; NB: picture of the size prior to splitting.  will add overhead to eventual invocation of
                          ; NB: the continuation as well
                          ($split-continuation x 0)
                          ; not following RA slot at base of the frame, but this should always hold dounderflow,
                          ; which will be in the static generation and therefore ignored anyway after compact heap
                          (let ([len ($continuation-stack-length x)])
                            (let loop ([i 1]
                                       [lpm ($continuation-return-livemask x)]
                                       [size (fx+ (constant size-continuation)
                                               (align (fx* len (constant ptr-bytes)))
                                               (compute-size ($continuation-return-code x))
                                               (compute-size ($closure-code x))
                                               (compute-size ($continuation-link x))
                                               (compute-size ($continuation-winders x)))])
                              (if (fx>= i len)
                                  size
                                  (loop (fx+ i 1) (ash lpm -1) (if (odd? lpm) (fx+ size (compute-size ($continuation-stack-ref x i))) size)))))))
                    (let ([n ($closure-length x)])
                      (do ([i 0 (fx+ i 1)]
                           [size (fx+ (align (fx+ (constant header-size-closure) (fx* n (constant ptr-bytes)))) (compute-size ($closure-code x)))
                             (fx+ size (compute-size ($closure-ref x i)))])
                        ((fx= i n) size))))]
               [($code? x)
                (fx+ (align (fx+ (constant header-size-code) ($code-length x)))
                  (let ([n ($code-reloc-size x)])
                    (let loop ([i 0] [size (align (fx+ (constant header-size-reloc-table) (fx* n (constant ptr-bytes))))] [addr 0])
                      (if (fx= i n)
                          size
                          (let ([r ($get-reloc x i)])
                            (and r
                                 (let ([type (logand (bitwise-arithmetic-shift-right r (constant reloc-type-offset)) (constant reloc-type-mask))])
                                   (if (logtest r (constant reloc-extended-format))
                                       (let ([addr (fx+ addr ($get-reloc x (fx+ i 2)))])
                                         (loop (fx+ i 3)
                                           (fx+ size
                                             (compute-size
                                               ($get-code-obj type x addr ($get-reloc x (fx+ i 1)))))
                                           addr))
                                       (let ([addr (fx+ addr (logand (bitwise-arithmetic-shift-right r (constant reloc-code-offset-offset)) (constant reloc-code-offset-mask)))])
                                         (loop (fx+ i 1)
                                           (fx+ size
                                             (compute-size
                                               ($get-code-obj type x addr
                                                 (logand (bitwise-arithmetic-shift-right r (constant reloc-item-offset-offset)) (constant reloc-item-offset-mask)))))
                                           addr)))))))))
                  (compute-size ($code-name x))
                  (compute-size ($code-info x))
                  (compute-size ($code-pinfo* x)))]
               [(port? x)
                (fx+ (constant size-port)
                  (compute-size ($port-handler x))
                  (if (input-port? x) (compute-size (port-input-buffer x)) 0)
                  (if (output-port? x) (compute-size (port-output-buffer x)) 0)
                  (compute-size ($port-info x))
                  (compute-size (port-name x)))]
               [(thread? x)
                (let ([tc ($object-ref 'scheme-object x (constant thread-tc-disp))])
                  (fold-left
                    (lambda (size disp)
                      (fx+ size (compute-size ($object-ref 'scheme-object tc disp))))
                    (constant size-thread)
                    tc-ptr-offsets))]
               [($tlc? x)
                (fx+ (constant size-tlc)
                  (compute-size ($tlc-ht x))
                  (compute-size ($tlc-keyval x))
                  (compute-size ($tlc-next x)))]
               [($rtd-counts? x) (constant size-rtd-counts)]
               [else ($oops who "missing case for ~s" x)])))
         ; ensure size-ht isn't counted in the size of any object
         (eq-hashtable-set! size-ht size-ht (cons cookie 0))
         (compute-size x)])))

  (set-who! $compute-composition
    (lambda (x maxgen)
      (define cookie (cons 'oatmeal 'raisin))
      (define seen-ht (make-eq-hashtable))
      (define rtd-ht (make-eq-hashtable))
      (define-syntax define-counters
        (lambda (x)
          (syntax-case x ()
            [(_ (name-vec count-vec incr!) type ...)
             (with-syntax ([(i ...) (enumerate #'(type ...))])
               #'(begin
                   (define name-vec (vector 'type ...))
                   (define count-vec (make-vector (length #'(type ...)) #f))
                   (define-syntax incr!
                     (syntax-rules (type ...)
                       [(_ type size)
                        (let ([p (vector-ref count-vec i)])
                          (if p
                              (begin
                                (set-car! p (fx+ (car p) 1))
                                (set-cdr! p (fx+ (cdr p) size)))
                              (vector-set! count-vec i (cons 1 size))))]
                       ...))))])))
      (define-counters (type-names type-counts incr!)
        pair symbol vector fxvector bytevector string box flonum bignum ratnum exactnum
        inexactnum continuation stack procedure code-object reloc-table port thread tlc
        rtd-counts)
      (define compute-composition!
        (lambda (x)
          (unless (or ($immediate? x)
                      (let ([g ($generation x)])
                        (or (not g) (fx> g maxgen))))
            (let ([a (eq-hashtable-cell seen-ht x #f)])
              (unless (cdr a)
                (set-cdr! a #t)
                (really-compute-composition! x))))))
      (define really-compute-composition!
        (lambda (x)
          (cond
            [(pair? x)
             (incr! pair (constant size-pair))
             (compute-composition! (car x))
             (compute-composition! (cdr x))]
            [(symbol? x)
             (incr! symbol (constant size-symbol))
             (compute-composition! (#3%$top-level-value x))
             (compute-composition! (property-list x))
             (compute-composition! ($system-property-list x))
             (compute-composition! ($symbol-name x))]
            [(vector? x)
             (incr! vector (align (fx+ (constant header-size-vector) (fx* (vector-length x) (constant ptr-bytes)))))
             (vector-for-each compute-composition! x)]
            [(fxvector? x) (incr! fxvector (align (fx+ (constant header-size-fxvector) (fx* (fxvector-length x) (constant ptr-bytes)))))]
            [(bytevector? x) (incr! bytevector (align (fx+ (constant header-size-bytevector) (bytevector-length x))))]
            [($record? x)
             (let ([rtd ($record-type-descriptor x)])
               (let ([p (eq-hashtable-ref rtd-ht rtd #f)] [size (align (rtd-size rtd))])
                 (if p
                     (begin
                       (set-car! p (fx+ (car p) 1))
                       (set-cdr! p (fx+ (cdr p) size)))
                     (eq-hashtable-set! rtd-ht rtd (cons 1 size))))
               (compute-composition! rtd)
               (for-each (lambda (fld)
                           (when (eq? (fld-type fld) 'scheme-object)
                             (compute-composition! ($object-ref 'scheme-object x (fld-byte fld)))))
                 (rtd-flds rtd)))]
            [(string? x) (incr! string (align (fx+ (constant header-size-string) (fx* (string-length x) (constant string-char-bytes)))))]
            [(box? x)
             (incr! box (constant size-box))
             (compute-composition! (unbox x))]
            [(flonum? x) (incr! flonum (constant size-flonum))]
            [(bignum? x) (incr! bignum (align (fx+ (constant header-size-bignum) (fx* ($bignum-length x) (constant bigit-bytes)))))]
            [(ratnum? x)
             (incr! ratnum (constant size-ratnum))
             (compute-composition! ($ratio-numerator x))
             (compute-composition! ($ratio-denominator x))]
            [($exactnum? x)
             (incr! exactnum (constant size-exactnum))
             (compute-composition! ($exactnum-real-part x))
             (compute-composition! ($exactnum-imag-part x))]
            [($inexactnum? x) (incr! inexactnum (constant size-inexactnum))]
            [(procedure? x)
             (if ($continuation? x)
                 (begin
                   (incr! continuation (constant size-continuation))
                   (unless (or (eq? x $null-continuation) (= ($continuation-stack-length x) (constant unscaled-shot-1-shot-flag)))
                     ; NB: rather not do this...splitting creates new continuation objects and gives an inaccurate
                     ; NB: picture of the continuation counts & sizes prior to splitting.  will add overhead to eventual invocation of
                     ; NB: the continuation as well
                     ($split-continuation x 0)
                     (compute-composition! ($continuation-return-code x))
                     (compute-composition! ($closure-code x))
                     (compute-composition! ($continuation-link x))
                     (compute-composition! ($continuation-winders x))
                     (let ([len ($continuation-stack-length x)])
                       (incr! stack (align (fx* len (constant ptr-bytes))))
                       (let loop ([i 1] [lpm ($continuation-return-livemask x)])
                         (unless (fx>= i len)
                           (when (odd? lpm) (compute-composition! ($continuation-stack-ref x i)))
                           (loop (fx+ i 1) (ash lpm -1)))))))
                 (begin
                   (compute-composition! ($closure-code x))
                   (let ([n ($closure-length x)])
                     (incr! procedure (align (fx+ (constant header-size-closure) (fx* n (constant ptr-bytes)))))
                     (do ([i 0 (fx+ i 1)])
                       ((fx= i n))
                       (compute-composition! ($closure-ref x i))))))]
            [($code? x)
             (incr! code-object (align (fx+ (constant header-size-code) ($code-length x))))
             (let ([n ($code-reloc-size x)])
               (incr! reloc-table (align (fx+ (constant header-size-reloc-table) (fx* n (constant ptr-bytes)))))
               (let loop ([i 0] [addr 0])
                 (unless (fx= i n)
                   (let ([r ($get-reloc x i)])
                     (and r
                          (let ([type (logand (bitwise-arithmetic-shift-right r (constant reloc-type-offset)) (constant reloc-type-mask))])
                            (if (logtest r (constant reloc-extended-format))
                                (let ([addr (fx+ addr ($get-reloc x (fx+ i 2)))])
                                  (compute-composition! ($get-code-obj type x addr ($get-reloc x (fx+ i 1))))
                                  (loop (fx+ i 3) addr))
                                (let ([addr (fx+ addr (logand (bitwise-arithmetic-shift-right r (constant reloc-code-offset-offset)) (constant reloc-code-offset-mask)))])
                                  (compute-composition!
                                    ($get-code-obj type x addr
                                      (logand (bitwise-arithmetic-shift-right r (constant reloc-item-offset-offset)) (constant reloc-item-offset-mask))))
                                  (loop (fx+ i 1) addr)))))))))
             (compute-composition! ($code-name x))
             (compute-composition! ($code-info x))
             (compute-composition! ($code-pinfo* x))]
            [(port? x)
             (incr! port (constant size-port))
             (compute-composition! ($port-handler x))
             (if (input-port? x) (compute-composition! (port-input-buffer x)) 0)
             (if (output-port? x) (compute-composition! (port-output-buffer x)) 0)
             (compute-composition! ($port-info x))
             (compute-composition! (port-name x))]
            [(thread? x)
             (incr! thread (constant size-thread))
             (let ([tc ($object-ref 'scheme-object x (constant thread-tc-disp))])
               (for-each (lambda (disp) (compute-composition! ($object-ref 'scheme-object tc disp))) tc-ptr-offsets))]
            [($tlc? x)
             (incr! tlc (constant size-tlc))
             (compute-composition! ($tlc-ht x))
             (compute-composition! ($tlc-keyval x))
             (compute-composition! ($tlc-next x))]
            [($rtd-counts? x) (incr! rtd-counts (constant size-rtd-counts))]
            [else ($oops who "missing case for ~s" x)])))
      ; ensure hashtables aren't counted
      (eq-hashtable-set! seen-ht seen-ht #t)
      (eq-hashtable-set! seen-ht rtd-ht #t)
      (compute-composition! x)
      (append
        (filter cdr (vector->list (vector-map cons type-names type-counts)))
        (vector->list
          (let-values ([(keys vals) (hashtable-entries rtd-ht)])
            (vector-map cons keys vals))))))

  (set-who! $make-object-finder
    ; pred object maxgen => object-finder procedure that returns 
    ;                               next object satisfying pred
    ;                               or #f, if no object found
    (lambda (pred x maxgen)
      (let ([seen-ht (make-eq-hashtable)])
        (define saved-next-proc
          (lambda ()
            (find! x '() (lambda () #f))))
        (define find!
          (lambda (x path next-proc)
            (let ([path (cons x path)])
              (cond
                [(or ($immediate? x) (let ([g ($generation x)]) (or (not g) (fx> g maxgen))))
                 (if (pred x) 
                     (begin (set! saved-next-proc next-proc) path)
                     (next-proc))]
                [else
                  (if (eq-hashtable-ref seen-ht x #f)
                      (next-proc) ; detected a loop, so backtrack and keep looking
                      (begin
                        (eq-hashtable-set! seen-ht x #t) ; mark this node as visited
                        (really-find! x path next-proc)))]))))
        ; We're visiting this node for the first time
        (define really-find!
          (lambda (x path next-proc)
            (define-syntax construct-proc
              (syntax-rules ()
                [(_ ?next-proc) ?next-proc]
                [(_ ?e ?e* ... ?next-proc)
                 (lambda () (find! ?e path (construct-proc ?e* ... ?next-proc)))]))
            (let ([next-proc 
                    (cond
                      [(pair? x) (construct-proc (car x) (cdr x) next-proc)]
                      [(symbol? x)
                       (construct-proc
                         (#3%$top-level-value x)
                         (property-list x)
                         ($system-property-list x)
                         ($symbol-name x) next-proc)]
                      [(vector? x)
                       (let ([n (vector-length x)])
                         (let f ([i 0])
                           (if (fx= i n)
                               next-proc
                               (construct-proc (vector-ref x i) (f (fx+ i 1))))))]
                      [($record? x)
                       (let ([rtd ($record-type-descriptor x)])
                         (construct-proc rtd
                           (let f ([flds (rtd-flds rtd)])
                             (if (null? flds)
                                 next-proc
                                 (let ([fld (car flds)])
                                   (if (eq? (fld-type fld) 'scheme-object)
                                       (construct-proc ($object-ref 'scheme-object x (fld-byte fld)) (f (cdr flds)))
                                       (f (cdr flds))))))))]
                      [(or (fxvector? x) (bytevector? x) (string? x) (flonum? x) (bignum? x)
                           ($inexactnum? x) ($rtd-counts? x)) 
                       next-proc]
                      [(box? x) (construct-proc (unbox x) next-proc)]
                      [(ratnum? x) (construct-proc ($ratio-numerator x) ($ratio-denominator x) next-proc)]
                      [($exactnum? x) (construct-proc ($exactnum-real-part x) ($exactnum-imag-part x) next-proc)]
                      [(procedure? x)
                       (if ($continuation? x)
                           (if (or (eq? x $null-continuation) (= ($continuation-stack-length x) (constant unscaled-shot-1-shot-flag)))
                               next-proc
                               (begin
                                 ; NB: rather not do this...splitting creates new continuation objects and gives an inaccurate
                                 ; NB: picture of the size prior to splitting.  will add overhead to eventual invocation of
                                 ; NB: the continuation as well
                                 ($split-continuation x 0)
                                 ; not following RA slot at base of the frame, but this should always hold dounderflow,
                                 ; which will be in the static generation and therefore ignored anyway after compact heap
                                 (let ([len ($continuation-stack-length x)])
                                   (let loop ([i 1] [lpm ($continuation-return-livemask x)])
                                     (if (fx>= i len)
                                         (construct-proc ($continuation-return-code x) ($closure-code x) ($continuation-link x) ($continuation-winders x) next-proc)
                                         (if (odd? lpm)
                                             (construct-proc ($continuation-stack-ref x i) (loop (fx+ i 1) (ash lpm -1))) 
                                             (loop (fx+ i 1) (ash lpm -1))))))))
                           (construct-proc ($closure-code x)
                             (let ([n ($closure-length x)])
                               (let f ([i 0])
                                 (if (fx= i n)
                                     next-proc
                                     (construct-proc ($closure-ref x i) (f (fx+ i 1))))))))]
                      [($code? x)
                       (construct-proc ($code-name x) ($code-info x) ($code-pinfo* x)
                         (let ([n ($code-reloc-size x)])
                           (let loop ([i 0] [addr 0])
                             (if (fx= i n)
                                 next-proc
                                 (let ([r ($get-reloc x i)])
                                   (if (not r)
                                       next-proc
                                       (let ([type (logand (bitwise-arithmetic-shift-right r (constant reloc-type-offset)) (constant reloc-type-mask))])
                                         (if (logtest r (constant reloc-extended-format))
                                             (let ([addr (fx+ addr ($get-reloc x (fx+ i 2)))])
                                               (construct-proc ($get-code-obj type x addr ($get-reloc x (fx+ i 1)))
                                                 (loop (fx+ i 3) addr)))
                                             (let ([addr (fx+ addr (logand (bitwise-arithmetic-shift-right r (constant reloc-code-offset-offset)) (constant reloc-code-offset-mask)))])
                                               (construct-proc
                                                 ($get-code-obj type x addr
                                                   (logand (bitwise-arithmetic-shift-right r (constant reloc-item-offset-offset)) (constant reloc-item-offset-mask)))
                                                 (loop (fx+ i 1) addr)))))))))))]
                      [(port? x)
                       (construct-proc ($port-handler x) ($port-info x) (port-name x)
                         (let ([th (lambda () (if (output-port? x) (construct-proc (port-output-buffer x) next-proc) next-proc))])
                           (if (input-port? x) (construct-proc (port-input-buffer x) (th)) (th))))]
                      [(thread? x)
                       (let ([tc ($object-ref 'scheme-object x (constant thread-tc-disp))])
                         (let f ([disp-list tc-ptr-offsets])
                           (if (null? disp-list)
                               next-proc
                               (construct-proc ($object-ref 'scheme-object tc (car disp-list)) (f (cdr tc-ptr-offsets))))))]
                      [($tlc? x) (construct-proc ($tlc-ht x) ($tlc-keyval x) ($tlc-next x) next-proc)]
                      [else ($oops who "missing case for ~s" x)])])
              ; check if this node is what we're looking for
              (if (pred x)
                  (begin (set! saved-next-proc next-proc) path)
                  (next-proc)))))
        (rec find-next (lambda () (saved-next-proc)))))))

(let ()
  (define filter-generation
    (lambda (who g)
      (unless (or (and (fixnum? g) (fx<= 0 g (collect-maximum-generation))) (eq? g 'static))
        ($oops who "invalid generation ~s" g))
      (if (eq? g 'static) (constant static-generation) g)))

  (set-who! make-object-finder
    (case-lambda
      [(pred)
       (unless (procedure? pred) ($oops who "~s is not a procedure" pred))
       ($make-object-finder pred (oblist) (collect-maximum-generation))]
      [(pred x)
       (unless (procedure? pred) ($oops who "~s is not a procedure" pred))
       ($make-object-finder pred x (collect-maximum-generation))]
      [(pred x g)
       (unless (procedure? pred) ($oops who "~s is not a procedure" pred))
       ($make-object-finder pred x (filter-generation who g))]))

  (set-who! compute-size
    (case-lambda
      [(x) ($compute-size x (collect-maximum-generation))]
      [(x g) ($compute-size x (filter-generation who g))]))

  (set-who! compute-composition
    (case-lambda
      [(x) ($compute-composition x (collect-maximum-generation))]
      [(x g) ($compute-composition x (filter-generation who g))])))

(define object-counts (foreign-procedure "(cs)object_counts" () ptr))
)
